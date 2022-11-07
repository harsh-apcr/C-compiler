#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <vector>
#include <unordered_map>
#include <string>
#include <cassert>
#include "ast.hpp"
#include "scope_check.hpp"

#include "llvm/IR/Value.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalVariable.h"

#include "expression_codegen.hpp"
#include "statement_codegen.hpp"
#include "declarator_codgen.hpp"
#include "declarator_codgen.hpp"
#include "blockitem_codegen.hpp"

static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;
static symbol_table NamedValues;
static function_table NamedFunctions;

#define ZERO_VAL llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0))
#define ONE_VAL llvm::ConstantInt::get(TheContext, llvm::APInt(32, 1))
value_llvm one_val = value_llvm(ONE_VAL, type_llvm(llvm::Type::getInt32Ty(TheContext), true));
value_llvm zero_val = value_llvm(ZERO_VAL, type_llvm(llvm::Type::getInt32Ty(TheContext), true));

static llvm::Function *CurrFunction;    // put it in all location required (namely inside function definition)
                                        
static label_stack LabelValues;         // only for maintaining case/default statements (which might be nested)
                                        
static label_table LabelTable;          // for only LABELED_STMT (not for case/default)
                                        // search for label from goto statements will only search LabelTable
                                        // you clear label table only if you exit function definition (not neccessarily any cmpn_stmt)

static std::unordered_multimap<std::string, llvm::BranchInst *> NotFoundLabels;

static std::vector<llvm::BasicBlock *> continue_dest_list;
static std::vector<llvm::BasicBlock *> break_dest_list;

std::vector<llvm::SwitchInst *> switch_inst;

inline void push_continue(llvm::BasicBlock *bb) {
    continue_dest_list.push_back(bb);
}
inline void pop_continue() {
    continue_dest_list.pop_back();
}

inline void push_break(llvm::BasicBlock *bb) {
    break_dest_list.push_back(bb);
}
inline void pop_break() {
    break_dest_list.pop_back();
}

void ENTER_LABEL_BLOCK(label_stack &label_stack) {
    label_table new_table;
    label_stack.push_back(new_table);
}

int PUSH_LABEL_TOP(const std::string &name,label_stack &label_stack, llvm::BasicBlock *bb) {
    if (find_label_top(label_stack, name)) return PUSH_LABEL_TOP_FAIL;
    else {
        push_label(label_stack, name, bb);
        return PUSH_LABEL_TOP_SUCCESS;
    }
}

void POP_LABEL_BLOCK(label_stack &label_stack) {
    assert(!label_stack.empty());
    label_stack.pop_back();
}

llvm::BasicBlock *GET_DEF_FROM_TOP(label_stack &label_stack) {
    label_table top_table = label_stack.back();
    // "default" is prefix of the name we wish to find
    for(auto itr = top_table.begin();itr != top_table.end();itr++) {
        if (!strncmp(itr->first.c_str(), "default", 7 * sizeof(char))) {
            return itr->second;
        }
    }
    return nullptr;
}



// ======== HELPER FUNCTIONS ========

// Create and Alloca instruction in the entry block of the function. This is used for mutable variables etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                          llvm::Type *Ty,
                                          const std::string &VarName) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                 TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Ty, 0,
                           VarName.c_str());    // array size = 0(no array support right now)
}


llvm::AllocaInst *CreateVariableAlloca(type_llvm ty, const std::string &VarName) {
    return Builder.CreateAlloca(ty.Ty, 0, VarName);
}

int intcast_type(value_llvm &value, type_llvm type) {
    if (value.ty.Ty->isFloatingPointTy()) {
        bool is_signed = type.is_signed;
        if (is_signed) 
            value.val = Builder.CreateFPToSI(value.val, type.Ty, value.val->getName().str() + "_fptosi");
        else
            value.val = Builder.CreateFPToUI(value.val, type.Ty, value.val->getName().str() + "_fptoui");
        if (!value.val) return 1;
        value.ty.Ty = value.val->getType();
        return 0;
    }
    if (value.ty.Ty->isIntegerTy()) {
        auto ty1 = llvm::dyn_cast<llvm::IntegerType>(value.val->getType());
        auto ty2 = llvm::dyn_cast<llvm::IntegerType>(type.Ty);    
        if (!ty1 || !ty2) {
            return 1;
        }
        bool is_signed = value.ty.is_signed;
        if (is_signed)
            value.val = Builder.CreateSExtOrTrunc(value.val, ty2, value.val->getName().str() + "_cast");
        else
            value.val = Builder.CreateZExtOrTrunc(value.val, ty2, value.val->getName().str() + "_cast");
        // type of val has been changed accordingly
        value.ty.is_signed = type.is_signed;
        value.ty.Ty = value.val->getType(); // update ty as well
        return 0;
    }
    return 1;
}

int floatcast_type(value_llvm &value, type_llvm type) {
    if ((value.val->getType()->isFloatTy() && type.Ty->isFloatTy()) ||
        (value.val->getType()->isDoubleTy() && type.Ty->isDoubleTy())) 
        return 0;    // already the same type, no type casting needed
    bool is_signed = value.ty.is_signed;
    if (value.val->getType()->isIntegerTy()) {
        // int to float/double
        if (is_signed) 
            value.val = Builder.CreateUIToFP(value.val, type.Ty, value.val->getName().str() + "_uitofp");
        else
            value.val = Builder.CreateSIToFP(value.val, type.Ty, value.val->getName().str() + "_sitofp");
        if (!value.val) return 1;
        value.ty.Ty = value.val->getType();
        return 0;
    }
    // double to float trunc
    if (value.val->getType()->isDoubleTy() && type.Ty->isFloatTy()) {
        value.val = Builder.CreateFPTrunc(value.val, llvm::Type::getFloatTy(TheContext),
                                         value.val->getName().str() + "_ftrunc");
        if (!value.val) return 1;
        value.ty.Ty = value.val->getType();
        return 0;
    } 
    // float to double ext
    if (value.val->getType()->isFloatTy() && type.Ty->isDoubleTy()) {
        value.val = Builder.CreateFPExt(value.val, llvm::Type::getDoubleTy(TheContext),
                                         value.val->getName().str() + "_fext");
        if (!value.val) return 1;
        value.ty.Ty = value.val->getType();
        return 0;
    }
    return 1;
}

// to = from -> to = (to) from  // typecast `from` value to `to` value
value_llvm store_codegen(std::string idname, value_llvm from) {
    value_llvm tostore = find_symbol(NamedValues, idname);
    assert(tostore.val);
    if (tostore.ty.Ty->isIntegerTy()) {
        if (intcast_type(from, tostore.ty) == 1) {
            if (!CurrFunction) {
                fprintf(stderr, "warning: potential type mismatch while assigning to `%s`\nnote: this warning may appear if assigned types are not integer or float\n", idname.c_str());
            } else {
                fprintf(stderr, "warning: potential type mismatch while assigning to `%s` in function `%s`\nnote: this warning may appear if assigned types are not integer or float\n", idname.c_str(), CurrFunction->getName().str().c_str());
            }
        }
    }
    if (tostore.ty.Ty->isFloatingPointTy()) {
        if (floatcast_type(from, tostore.ty) == 1) {
            if (!CurrFunction)
                fprintf(stderr, "warning: potential type mismatch while assigning to `%s`\nnote: this warning may appear if assigned types are not integer or float\n", idname.c_str());
            else
                fprintf(stderr, "warning: potential type mismatch while assigning to `%s` in function `%s`\nnote: this warning may appear if assigned types are not integer or float\n", idname.c_str(), CurrFunction->getName().str().c_str());
        }
    }
    Builder.CreateStore(from.val, tostore.val);
    return from;
}

// returns true if statement contains any labeled_stmt
bool has_labeled_stmt(struct _ast_node *stmt) {
    switch(stmt->node_type) {
        case LABELED_STMT:
            return true;
        case CASE_STMT:
            return has_labeled_stmt(stmt->children[1]);
        case DEF_STMT:
            return has_labeled_stmt(stmt->children[0]);
        case CMPND_STMT: {
            struct _ast_node *blk_item_list = stmt->children[0];
            if (!blk_item_list) {
                return false;   // empty cmpnd_stmt
            } else {
                struct _ast_node **blk_item;    // a declaration or statement
                for(blk_item = blk_item_list->children;*blk_item != NULL;blk_item++) {
                    if ((*blk_item)->node_type != NODE_TYPE::DECLARATION) {
                        // (*blk_item) is a statement node
                        if (has_labeled_stmt(*blk_item)) return true;
                    }
                }
                return false;
            }
        } 
        case EXPR_STMT:
            return false;
        case IF_ELSE_STMT:
        case IF_STMT:
            if (has_labeled_stmt(stmt->children[1])) return true;
            if (stmt->children[2]) {
                // else stmt
                // if expression doesn't have labeled statement
                return has_labeled_stmt(stmt->children[2]);
            }
            return false;
        case SWITCH_STMT:
            return has_labeled_stmt(stmt->children[1]);
        case WHILE_STMT:
            return has_labeled_stmt(stmt->children[1]);
        case DO_WHILE_STMT:
            return has_labeled_stmt(stmt->children[0]);
        case FOR_STMT1:
        case FOR_STMT2:
        case FOR_STMT3:
        case FOR_STMT4:
            fprintf(stderr, "for-loops are not supported for code generation\n");
            exit(1);
        case GOTO_STMT:
        case CONTINUE_STMT:
        case BREAK_STMT:
        case RETURN_STMT1:
        case RETURN_STMT2:
            return false;
        default:
            // should be unreachable
            fprintf(stderr, "error: unexpected statement type\n");
            exit(1);
    }
}


// ============ GETTING THE TYPE INFORMATION FROM DECLARATION_SPECIFIER ============

llvm::Type *typespec_getllvmtype(struct _ast_node *typespec) {
    assert(typespec->node_type == NODE_TYPE::TYPE_SPECIFIER);
    enum NODE_TYPE type = typespec->children[0]->node_type;
    switch(type) {
        case TYPE_SPEC_VOID:
            return llvm::Type::getVoidTy(TheContext);
        case TYPE_SPEC_BOOL:
            return llvm::Type::getInt8Ty(TheContext);   // bool in C is 8-bits or a byte 
        case TYPE_SPEC_CHAR:
        case TYPE_SPEC_SCHAR:
        case TYPE_SPEC_UCHAR:
            return llvm::Type::getInt8Ty(TheContext);
        case TYPE_SPEC_SHORT:
        case TYPE_SPEC_SHORTINT:
        case TYPE_SPEC_SSHORT:
        case TYPE_SPEC_USHORT:
            return llvm::Type::getInt16Ty(TheContext);
        case TYPE_SPEC_INT:
        case TYPE_SPEC_UINT:
        case TYPE_SPEC_LONG:
        case TYPE_SPEC_ULONG:
            return llvm::Type::getInt32Ty(TheContext);
        case TYPE_SPEC_LONGLONG:
        case TYPE_SPEC_INTLONGLONG:
        case TYPE_SPEC_INTLONG:
        case TYPE_SPEC_ULONGLONG:
            return llvm::Type::getInt64Ty(TheContext);
        case TYPE_SPEC_FLOAT:
            return llvm::Type::getFloatTy(TheContext);
        case TYPE_SPEC_DOUBLE:
            return llvm::Type::getDoubleTy(TheContext);
        case TYPE_SPEC_LONGDOUBLE:
            return llvm::Type::getFP128Ty(TheContext);
        default:
            fprintf(stderr, "invalid type specifier `%s` provided\n", get_nodestr(type).c_str());
            exit(1);
    }
}

type_llvm typespec_gettype(struct _ast_node *typespec) {
    assert(typespec->node_type == NODE_TYPE::TYPE_SPECIFIER);
    return type_llvm(typespec_getllvmtype(typespec), is_signed(typespec));
}

// changes the passed in type specifier and returns it as well
void pointer_settype(struct _ast_node *ptr, type_llvm &typespec) {
    assert(ptr->node_type == PTR);
    struct _ast_node **type_qual;
    for(type_qual = ptr->children;*type_qual != NULL;type_qual++) {
        typespec.Ty = typespec.Ty->getPointerTo();  // not worrying about what type qualifiers are there
    }
}

// TODO: change the uses of get_type to support unsigned types as well
// type qualifiers semantics are not supported so won't be considered in type
type_llvm get_type(struct _ast_node *decl_spec, struct _ast_node *ptr) {
    assert(decl_spec->node_type == DECLARATION_SPEC);
    type_llvm ty = typespec_gettype(decl_spec->children[0]);
    if (ptr) 
        pointer_settype(ptr, ty);
    return ty;
}

void make_common_ftype(value_llvm &val1, value_llvm &val2) {
    if (val1.ty.Ty->isFloatingPointTy()) {
        if (val2.ty.Ty->isIntegerTy()) {
            floatcast_type(val2, val1.ty);
            return;
        }
        if (val2.ty.Ty->isFloatingPointTy()) {
            if (val1.ty.Ty->isDoubleTy() && val2.ty.Ty->isFloatTy()) {
                floatcast_type(val2, val1.ty);
                return;
            }
            if (val1.ty.Ty->isFloatTy() && val2.ty.Ty->isDoubleTy()) {
                floatcast_type(val1, val2.ty);
                return;
            }
        }
        // val2 is neither integer nor floating point type
        fprintf(stderr, "warning: unexpected type being operated with floats\n");
        return;
    }
    if (val2.ty.Ty->isFloatingPointTy()) {
        // val1 is not floating point ty
        if (!val1.ty.Ty->isIntegerTy()) {
            fprintf(stderr, "warning: unexpected type being operated with floats\n");
            return;
        } else {
            // integer type
            floatcast_type(val1, val2.ty);
            return;
        }
    }
    fprintf(stderr, "warning: unexpected type for binary operation\n");
    return;
}


// assert: val1 and val2 are both some form of integer types
void make_common_itype(value_llvm &val1, value_llvm &val2) {
    auto ty1 = llvm::dyn_cast<llvm::IntegerType>(val1.val->getType());
    auto ty2 = llvm::dyn_cast<llvm::IntegerType>(val2.val->getType());    
    assert(ty1 && "only integer type conversion allowed");
    assert(ty2 && "only integer type conversion allowed");

    int bits1 = ty1->getBitWidth();
    int bits2 = ty2->getBitWidth();

    type_llvm common_type = val1.ty;
    if (bits1 < bits2)
        common_type.Ty = ty2;
    else
        common_type.Ty = ty1;
    // got the common type
    common_type.is_signed = val1.ty.is_signed && val2.ty.is_signed;


    auto val1const = llvm::dyn_cast<llvm::ConstantInt>(val1.val);
    auto val2const = llvm::dyn_cast<llvm::ConstantInt>(val2.val);
    
    
    if (val1.ty.is_signed) {
    val1.val = Builder.CreateSExtOrTrunc(val1.val, common_type.Ty, val1.val->getName().str() + "cast");
    val1.ty.Ty = val1.val->getType();
    } else {
        val1.val = Builder.CreateZExtOrTrunc(val1.val, common_type.Ty, val1.val->getName().str() + "cast");
        val1.ty.Ty = val1.val->getType();
    }
    
    
    if (val2.ty.is_signed) {
    val2.val = Builder.CreateSExtOrTrunc(val2.val, common_type.Ty, val2.val->getName().str() + "cast");
    val2.ty.Ty = val2.val->getType();
    } else {
        val2.val = Builder.CreateZExtOrTrunc(val2.val, common_type.Ty, val2.val->getName().str() + "cast");
        val2.ty.Ty = val2.val->getType();
    }
    
    
}

inline void convert_bool(value_llvm &val) {
    zero_val = ZERO_VAL;
    make_common_itype(val, zero_val);
    val.val = Builder.CreateICmpNE(val.val, zero_val.val, val.val->getName().str() + "cast");
    val.val = Builder.CreateZExtOrBitCast(val.val, llvm::IntegerType::get(TheContext, 32), val.val->getName().str() + "cast");
    val.ty = type_llvm(llvm::IntegerType::get(TheContext, 32));
}

// ============ GETTING THE TYPE INFORMATION FROM DECLARATION_SPECIFIER and PTR node ============




llvm::Value *LogErrorV(const char *name) {
    fprintf(stderr, "log-error : unknown variable named `%s`", name);
    return nullptr;
}

llvm::Value *LogErrorF(const char *name) {
    fprintf(stderr, "log-error : unknown function named `%s`", name);
    return nullptr;
}

std::vector<std::string> getfun_argv(struct _ast_node *fun_decl) {
    assert(fun_decl->node_type == FUNCTION_DECL);
    struct _ast_node *param_list = fun_decl->children[1];   // param_list->node_type == PARAM_LIST
    // all the child nodes of param_list are PARAM_DECL
    std::vector<std::string> argv;
    if (!param_list)
        return argv;
    struct _ast_node **param_decl;
    struct _ast_node *declarator;
    struct _ast_node *id_decl;
    int no_name = 0;
    for(param_decl = param_list->children;*param_decl != NULL;++param_decl) {
        declarator = (*param_decl)->children[1];
        if (declarator) {
            id_decl = declarator->children[0];
            assert(id_decl->node_type == IDENTIFIER_DECL);
            argv.push_back(id_decl->children[0]->node_val);
        } else {
            argv.push_back("no_name" + std::to_string(no_name++));
        }
    }
    return argv;
}


bool isassign_op(enum NODE_TYPE op) {
    switch(op) {
        case ASSIGN:
        case MULASSIGN:
        case DIVASSIGN:
        case MODASSIGN:
        case ADDASSIGN: 
        case SUBASSIGN:
        case LEFTASSIGN: 
        case RIGHTASSIGN:
        case BITANDASSIGN:
        case BITXORASSIGN:
        case BITORASSIGN:
            return true;
        default:
            return false;
    }
}

bool isbin_op(enum NODE_TYPE op) {
    switch(op) {
        case LOR:
        case LAND:
        case BITOR:
        case BITXOR:
        case BITAND:
        case NEQOP:
        case EQOP:
        case GREATEREQ:
        case LESSEQ:
        case GREATER_THAN:
        case LESS_THAN:
        case LEFT_SHIFT:
        case RIGHT_SHIFT:
        case PLUS:
        case MINUS:
        case MULT:
        case DIV:
        case MOD:
            return true;
        default:
            return false;
    }
}

std::string binop_tostr(enum NODE_TYPE op) {
    switch(op) {
        case LOR:
            return "||";
        case LAND:
            return "&&";
        case BITOR:
            return "|";
        case BITXOR:
            return "^";
        case BITAND:
            return "&";
        case NEQOP:
            return "!=";
        case EQOP:
            return "==";
        case GREATEREQ:
            return ">=";
        case LESSEQ:
            return "<=";
        case GREATER_THAN:
            return ">";
        case LESS_THAN:
            return "<";
        case LEFT_SHIFT:
            return "<<";
        case RIGHT_SHIFT:
            return ">>";
        case PLUS:
            return "+";
        case MINUS:
            return "-";
        case MULT:
            return "*";
        case DIV:
            return "/";
        case MOD:
            return "%";
        default:
            return "unknown binary operator";
    }
}

// ======== HELPER FUNCTIONS ========



value_llvm constant_codegen(struct _ast_node *constant) {
    assert(constant->node_type == I_CONST 
        || constant->node_type == F_CONST);
    
    if (constant->node_type == I_CONST) {
        // a 32 bit signed integer
        type_llvm ty = type_llvm(llvm::Type::getInt32Ty(TheContext), true);
        return value_llvm(llvm::ConstantInt::get(TheContext, llvm::APInt(32, atoi(constant->node_val), true)), ty);
    }
    if (constant->node_type == F_CONST) {
        type_llvm ty = type_llvm(llvm::Type::getFloatTy(TheContext), false);
        return value_llvm(llvm::ConstantFP::get(TheContext, llvm::APFloat(atof(constant->node_val))), ty);
    }
    return value_llvm();    // unreachable statement
}

value_llvm string_codegen(struct _ast_node *string_node) {
    assert(string_node->node_type == NODE_TYPE::STRING);
    if (!CurrFunction) {
        fprintf(stderr, "error: string cannot be assigned to global variables\n");
        exit(1);
    }
    llvm::GlobalVariable *str = Builder.CreateGlobalString(string_node->node_val, "str_const");
    std::vector<llvm::Value *> indices{ZERO_VAL, ZERO_VAL};
    llvm::Value *ptr = Builder.CreateInBoundsGEP(str, indices, "strgep");
    return value_llvm(ptr, type_llvm(ptr->getType(), true)); // assumed to be `signed char*`
}

// id is on stack
value_llvm id_codegen(struct _ast_node *id) {
    assert(id->node_type == ID);
    value_llvm v = find_symbol(NamedValues, id->node_val);
    if (!v.val) LogErrorV(id->node_val);    // this check is anyway performed by scope checker
    v.val = Builder.CreateLoad(v.val, id->node_val);
    assert(v.ty.Ty);
    return v;
}

value_llvm binop_codegen(value_llvm &lhs, value_llvm &rhs, enum NODE_TYPE op, bool i1toi32 = true) {
    
    if (!lhs.val->getType()->isIntegerTy() || !rhs.val->getType()->isIntegerTy()) {
        // lhs or rhs is a float/double type
        make_common_ftype(lhs, rhs);
        value_llvm cmp_ret;
        switch(op) {
            case MULT:
                return value_llvm(Builder.CreateFMul(lhs.val, rhs.val, "fmultmp"),
                         type_llvm(llvm::Type::getFloatTy(TheContext), false));
            case DIV:
                return value_llvm(Builder.CreateFDiv(lhs.val, rhs.val, "fdivmp"),
                         type_llvm(llvm::Type::getFloatTy(TheContext), false));
            case PLUS:
                return value_llvm(Builder.CreateFAdd(lhs.val, rhs.val, "faddtmp"),
                         type_llvm(llvm::Type::getFloatTy(TheContext), false));
            case MINUS:
                return value_llvm(Builder.CreateFSub(lhs.val, rhs.val, "fsubtmp"),
                         type_llvm(llvm::Type::getFloatTy(TheContext), false));
            case LESS_THAN:
                // O version of compare returns true only if both lhs and rhs are not NaN and comp holds
                cmp_ret = value_llvm(Builder.CreateFCmpOLT(lhs.val, rhs.val, "flttemp"),
                            type_llvm(llvm::Type::getInt1Ty(TheContext), true));
                break;
            case GREATER_THAN:
                cmp_ret = value_llvm(Builder.CreateFCmpOGT(lhs.val, rhs.val, "fgttemp"),
                            type_llvm(llvm::Type::getInt1Ty(TheContext), true));
                break;
            case LESSEQ:
                cmp_ret = value_llvm(Builder.CreateFCmpOLE(lhs.val, rhs.val, "fletemp"),
                            type_llvm(llvm::Type::getInt1Ty(TheContext), true));
                break;
            case GREATEREQ:
                cmp_ret = value_llvm(Builder.CreateFCmpOGE(lhs.val, rhs.val, "fgetemp"),
                            type_llvm(llvm::Type::getInt1Ty(TheContext), true));
                break;
            case EQOP:
                cmp_ret = value_llvm(Builder.CreateFCmpOEQ(lhs.val, rhs.val, "feqtemp"),
                            type_llvm(llvm::Type::getInt1Ty(TheContext), true));
                break;
            case NEQOP:
                cmp_ret = value_llvm(Builder.CreateFCmpONE(lhs.val, rhs.val, "fnetemp"),
                            type_llvm(llvm::Type::getInt1Ty(TheContext), true));
                break;
            default:
                // binary operators that are not supported
                fprintf(stderr, "error: binary operator `%s` not supported on floats\n", binop_tostr(op).c_str());
                exit(1);
        }
        if (!cmp_ret.val) {
            fprintf(stderr, "error: couldn't do comparision on floats\n");
            exit(1);
        }
        return i1toi32 ? value_llvm(Builder.CreateZExtOrBitCast(cmp_ret.val, Builder.getInt32Ty(), "cmpcast"), 
                                    type_llvm(llvm::Type::getInt32Ty(TheContext), true)) :
                        cmp_ret;
    } else {
        // both lhs and rhs are int type
        // before doing any operation on them, need to convert them into same type for any llvm-opcode
        make_common_itype(lhs, rhs);
        value_llvm retval;
        retval.ty = type_llvm(lhs.val->getType(), lhs.ty.is_signed && rhs.ty.is_signed);
        bool is_signed = retval.ty.is_signed;
        llvm::Instruction::BinaryOps bin_op;
        llvm::CmpInst::Predicate cmp_op;
        llvm::Value *cmp_ret;

        switch(op) {
            case MULT:
                retval.val = Builder.CreateMul(lhs.val, rhs.val, "imultmp");
                return retval;
            case DIV:
                bin_op = is_signed ? llvm::Instruction::BinaryOps::SDiv : llvm::Instruction::BinaryOps::UDiv ;
                retval.val = Builder.CreateBinOp(bin_op, lhs.val, rhs.val, "idivtemp");
            case PLUS:
                retval.val = Builder.CreateAdd(lhs.val, rhs.val, "iaddtmp");
                return retval;
            case MINUS:
                retval.val = Builder.CreateSub(lhs.val, rhs.val, "isubtmp");
                return retval;
            case LESS_THAN:
                cmp_op = is_signed ? llvm::CmpInst::Predicate::ICMP_SLT : llvm::CmpInst::Predicate::ICMP_ULT;
                cmp_ret = Builder.CreateICmp(cmp_op, lhs.val, rhs.val, "ilttemp");
                if (i1toi32) {
                    retval.val = Builder.CreateZExtOrBitCast(cmp_ret, retval.ty.Ty, "ltcast");
                } else {
                    retval.ty.Ty = llvm::Type::getInt1Ty(TheContext);
                    retval.val = cmp_ret;
                }
                return retval;
            case GREATER_THAN:
                cmp_op = is_signed ? llvm::CmpInst::Predicate::ICMP_SGT : llvm::CmpInst::Predicate::ICMP_UGT;
                cmp_ret = Builder.CreateICmp(cmp_op, lhs.val, rhs.val, "igttemp");
                if (i1toi32) {
                    retval.val = Builder.CreateZExtOrBitCast(cmp_ret, retval.ty.Ty, "gtcast");
                } else {
                    retval.ty.Ty = llvm::Type::getInt1Ty(TheContext);
                    retval.val = cmp_ret;
                }
                return retval;
            case LESSEQ:
                cmp_op = is_signed ? llvm::CmpInst::Predicate::ICMP_SLE : llvm::CmpInst::Predicate::ICMP_ULE;
                cmp_ret = Builder.CreateICmp(cmp_op, lhs.val, rhs.val, "iletemp");
                if (i1toi32) {
                    retval.val = Builder.CreateZExtOrBitCast(cmp_ret, retval.ty.Ty, "lecast");
                } else {
                    retval.ty.Ty = llvm::Type::getInt1Ty(TheContext);
                    retval.val = cmp_ret;
                }
                return retval;
            case GREATEREQ:
                cmp_op = is_signed ? llvm::CmpInst::Predicate::ICMP_SGE : llvm::CmpInst::Predicate::ICMP_UGE;
                cmp_ret = Builder.CreateICmp(cmp_op, lhs.val, rhs.val, "igetemp");
                if (i1toi32) {
                    retval.val = Builder.CreateZExtOrBitCast(cmp_ret, retval.ty.Ty, "gecast");
                } else {
                    retval.ty.Ty = llvm::Type::getInt1Ty(TheContext);
                    retval.val = cmp_ret;
                }
                return retval;
            case EQOP:
                cmp_ret = Builder.CreateICmpEQ(lhs.val, rhs.val, "ieqtemp");
                if (i1toi32) {
                    retval.val = Builder.CreateZExtOrBitCast(cmp_ret, retval.ty.Ty, "eqcast");
                } else {
                    retval.ty.Ty = llvm::Type::getInt1Ty(TheContext);
                    retval.val = cmp_ret;
                }
                return retval;
            case NEQOP:
                cmp_ret = Builder.CreateICmpNE(lhs.val, rhs.val, "inetemp");
                if (i1toi32) {
                    retval.val = Builder.CreateZExtOrBitCast(cmp_ret, retval.ty.Ty, "necast");
                } else {
                    retval.ty.Ty = llvm::Type::getInt1Ty(TheContext);
                    retval.val = cmp_ret;
                }
                return retval;
            case MOD:
                bin_op = is_signed ? llvm::Instruction::BinaryOps::SRem : llvm::Instruction::BinaryOps::URem;
                retval.val = Builder.CreateBinOp(bin_op, lhs.val, rhs.val, "iremtemp");
                return retval;
            case LEFT_SHIFT:
                retval.val = Builder.CreateShl(lhs.val, rhs.val, "ilstemp");
                return retval;
            case RIGHT_SHIFT:
                retval.val = Builder.CreateAShr(lhs.val, rhs.val, "irstemp");
                return retval;
            case BITAND:
                retval.val = Builder.CreateAnd(lhs.val, rhs.val, "iandtemp");
                return retval;
            case BITXOR:
                retval.val = Builder.CreateXor(lhs.val, rhs.val, "ixortemp");
                return retval;
            case BITOR:
                retval.val = Builder.CreateOr(lhs.val, rhs.val, "iortemp");
                return retval;

            // logical operators here, convert lhs/rhs value to 0 or 1 via comparison and then do bitand/bitor
            case LAND: {
                // both lhs and rhs are not constant
                convert_bool(lhs);
                convert_bool(rhs);           

                retval.val = Builder.CreateAnd(lhs.val, rhs.val, "landtemp");
                return retval;
            }
            case LOR: {
                convert_bool(lhs);
                convert_bool(rhs);

                retval.val = Builder.CreateOr(lhs.val, rhs.val, "lortemp");   
                return retval;
            }
            default:
                // binop not supported
                fprintf(stderr, "error: binary operator `%s` not supported on integers\n", binop_tostr(op).c_str());
                exit(1);
        }
    }
}

value_llvm binop_codegen(struct _ast_node *left, struct _ast_node *right, enum NODE_TYPE op) {
    // do binary operator codegen
    llvm::Constant *constlhs_val = nullptr;
    llvm::Constant *constrhs_val = nullptr;
    value_llvm lhs = valexpr_codegen(left);

    // constant folding optimization for LAND and LOR only
    if (lhs.val->getType()->isIntegerTy()) {
        value_llvm retval;
        constlhs_val = llvm::dyn_cast<llvm::Constant>(lhs.val);
        switch(op) {
            case LAND:
                if (constlhs_val) {
                    if (constlhs_val->isZeroValue()) {
                        retval.val = ZERO_VAL;
                        retval.ty = type_llvm(llvm::IntegerType::get(TheContext, 32));
                        return retval;                   
                    }
                }
                break;
            case LOR:
                if (constlhs_val) {
                    if (!constlhs_val->isZeroValue()) {
                        retval.val = ONE_VAL;
                        retval.ty = type_llvm(llvm::IntegerType::get(TheContext, 32));
                        return retval;                   
                    }
                }
                break; 
            default:
                break;
        }
    }
    value_llvm rhs = valexpr_codegen(right);
    if (rhs.val->getType()->isIntegerTy()) {
        value_llvm retval;
        constrhs_val = llvm::dyn_cast<llvm::Constant>(rhs.val);
        switch(op) {
            case LAND: 
                if (constrhs_val) {
                    if (constrhs_val->isZeroValue()) {
                        retval.val = ZERO_VAL;
                        retval.ty = type_llvm(llvm::IntegerType::get(TheContext, 32));
                        return retval;                   
                    }
                }
                break;
            case LOR:
                if (constrhs_val) {
                    if (!constrhs_val->isZeroValue()) {
                        retval.val = ONE_VAL;
                        retval.ty = type_llvm(llvm::IntegerType::get(TheContext, 32));
                        return retval;                   
                    }
                }
                break;
            default:
                break;
        }
    }
    if (constlhs_val && !constrhs_val) {
        value_llvm retval;
        switch(op) {
            case LAND: // constlhs is non-zero
            case LOR:  // constlhs is zero
                retval = rhs;
                return retval;
            default:
                break;
        }
    }
    if (!constlhs_val && constrhs_val) {
        value_llvm retval;
        switch(op) {
            case LAND: // constrhs is non-zero
            case LOR:  // constrhs is zero
                retval = lhs;
                return retval;
            default:
                break;
        }
    }
    return binop_codegen(lhs, rhs, op);
}

// identifier declaration node along with its type
value_llvm iddecl_codegen(struct _ast_node *iddecl, type_llvm ty) {
    assert(iddecl->node_type == IDENTIFIER_DECL);
    assert(ty.Ty);
    if (ty.Ty->isVoidTy()) {
        fprintf(stderr, "void type for variable declaration is not allowed\n");
        exit(1);
    }    
    if (!CurrFunction) {
        TheModule->getOrInsertGlobal(iddecl->children[0]->node_val, ty.Ty);
        llvm::GlobalVariable *gv = TheModule->getNamedGlobal(iddecl->children[0]->node_val);
        gv->setLinkage(llvm::GlobalVariable::ExternalLinkage);
        value_llvm gv_val = value_llvm(gv, ty);
        ADD_IDNODE_VAL(iddecl, gv_val, NamedValues);
        return gv_val;
    } else {
        llvm::AllocaInst *Alloca = CreateVariableAlloca(ty, iddecl->children[0]->node_val);
        value_llvm iddecl_val = value_llvm(Alloca, ty);
        ADD_IDNODE_VAL(iddecl, iddecl_val, NamedValues);  // NOTE: value added to symbol table varname is nullptr by def
        return iddecl_val;
    }
    
}

// declarator : pointer direct_declarator | direct_declarator;
value_llvm declarator_codegen(struct _ast_node *declarator, type_llvm idtype) {
    assert(declarator->node_type == DECLARATOR);
    struct _ast_node *direct_declarator = declarator->children[0];    // identifier_decl or function_decl
    
    if (direct_declarator->node_type == IDENTIFIER_DECL)
        return iddecl_codegen(direct_declarator, idtype);
    else 
        // direct_declarator->node_type == FUNCTION_PTRDECL || FUNCTION_DECL
        // in either case just generate code for function declaration
        return function_decl_codegen(direct_declarator, idtype);
}

value_llvm preincdecop_codegen(struct _ast_node *unary_expr, enum NODE_TYPE op) {
    one_val = ONE_VAL;
    value_llvm expr_val = unaryexpr_codegen(unary_expr);
    if (unary_expr->node_type != ID) {
        fprintf(stderr, "increment/decrement operator can only be applied to variables\n");
        exit(1);
    }
    switch (op) {
        case PREINC_OP: {
            value_llvm new_val = binop_codegen(expr_val,one_val,NODE_TYPE::PLUS);
            store_codegen(unary_expr->node_val, new_val);
            return new_val;
        }
        case PREDEC_OP: {
            value_llvm new_val = binop_codegen(expr_val, one_val, NODE_TYPE::MINUS);
            store_codegen(unary_expr->node_val, new_val);
            return new_val;
        }
        default:
        // unreachable statements
            fprintf(stderr, "error: unexpected node-type\n");
            exit(1);
    }
}

value_llvm unaryop_codegen(struct _ast_node *unaryexpr, enum NODE_TYPE op) {
    value_llvm negone_val = value_llvm(llvm::ConstantInt::get(TheContext, llvm::APInt(32, -1)),
                                     type_llvm(llvm::Type::getInt32Ty(TheContext), true));
    zero_val = ZERO_VAL;
    
    value_llvm unexpr_val = unaryexpr_codegen(unaryexpr);
    switch (op) {
        case ADDR_OP:
        case DEREF_OP:
            fprintf(stderr, "error: reference and de-reference operators are not supported\n");
            exit(1);
        case UNPLUS:
            return unexpr_val;
        case UNMINUS:
            return binop_codegen(zero_val, unexpr_val, NODE_TYPE::MINUS);
        case BIT_COMP:
            return binop_codegen(negone_val, 
                                unexpr_val,
                                NODE_TYPE::BITXOR);
        case NOT:
            return binop_codegen(zero_val,
                                 unexpr_val,
                                 NODE_TYPE::EQOP);
        default:
            fprintf(stderr, "error: unknown unary operator\n");
            exit(1);
    }
}

// inc/dec the passed in postfix_expr
value_llvm postincdecop_codegen(struct _ast_node *postfix_expr, enum NODE_TYPE op) {
    one_val = ONE_VAL;
    value_llvm expr_val = postfixexpr_codegen(postfix_expr);
    if (postfix_expr->node_type != ID) {
        fprintf(stderr, "increment/decrement operator can only be applied to variables\n");
        exit(1);
    }
    switch (op) {
        case POSTINC_OP: {
            value_llvm new_val = binop_codegen(expr_val,one_val,NODE_TYPE::PLUS);
            store_codegen(postfix_expr->node_val, new_val);
            return expr_val;
        }
        case POSTDEC_OP: {
            value_llvm new_val = binop_codegen(expr_val, one_val, NODE_TYPE::MINUS);
            store_codegen(postfix_expr->node_val, new_val);
            return expr_val;
        }
        default:
            fprintf(stderr, "error: unknown unary operator\n");
            exit(1);
    }
}

value_llvm primaryexpr_codegen(struct _ast_node *primary_expr) {
    switch(primary_expr->node_type) {
        case ID:
            return id_codegen(primary_expr);
        case I_CONST:
        case F_CONST:
            return constant_codegen(primary_expr);
        case STRING:
            return string_codegen(primary_expr);
        default:
            // paren_expression
            return expression_codegen(primary_expr);
    }
}

value_llvm postfixexpr_codegen(struct _ast_node *postfix_expr) {
    switch(postfix_expr->node_type) {
        case POSTINC_OP:
        case POSTDEC_OP:
            return postincdecop_codegen(postfix_expr->children[0], postfix_expr->node_type);
        case FUNCTION_CALL:
            return function_call_codegen(postfix_expr);
        case ARRAY_ACCESS:
            fprintf(stderr, "error: array access is not supported\n");
            return nullptr;
        default:
            return primaryexpr_codegen(postfix_expr);
    }
}

value_llvm unaryexpr_codegen(struct _ast_node *unary_expr) {
    switch(unary_expr->node_type) {
        // preinc/dec operator
        case PREINC_OP:
        case PREDEC_OP:
            return preincdecop_codegen(unary_expr->children[0], unary_expr->node_type);
        // unary operator
        case ADDR_OP:
        case DEREF_OP:
        case UNPLUS:
        case UNMINUS:
        case BIT_COMP:
        case NOT:
            // pass in castexpr (which is unaryexpr itself)
            return unaryop_codegen(unary_expr->children[0], unary_expr->node_type);
        default:
            // postfix expression
            return postfixexpr_codegen(unary_expr);
    }
}

value_llvm expression_codegen(struct _ast_node *expression) {
    assert(expression->node_type == EXPRESSION);
    struct _ast_node **assign_expr;
    std::vector<value_llvm> vals;
    for(assign_expr = expression->children;*assign_expr!=NULL;++assign_expr) {
        vals.push_back(assignexpr_codegen(*assign_expr));
    }
    return vals.back(); // value of the last assign_expr is the value of the expression
}

// TODO : complete the implementation
value_llvm ternop_codegen(struct _ast_node *expr) {
    zero_val = ZERO_VAL;
    assert(expr->node_type == TERNOP);
    struct _ast_node *cond = expr->children[0];         // valexpr_codegen()
    struct _ast_node *true_expr = expr->children[1];    // expression_codegen()
    struct _ast_node *false_expr = expr->children[2];   // valexpr_codegen()

    value_llvm cond_val = valexpr_codegen(cond);
    
    // deadcode optimization
    if (auto *const_val = llvm::dyn_cast<llvm::Constant>(cond_val.val)) {
        if (const_val->isZeroValue()) 
            return valexpr_codegen(false_expr);
        else 
            return expression_codegen(true_expr);
    }

    // TODO : generate code for ternop (see code for if-else)
    // llvm::Value *trueexpr_val = expression_codegen(true_expr);  // may involve assignments or just valexpr
    // llvm::Value *falseexpr_val = valexpr_codegen(false_expr);
    cond_val = binop_codegen(cond_val, zero_val, NODE_TYPE::NEQOP, false);

    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(TheContext, "tern_then", CurrFunction);
    llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(TheContext, "tern_else");
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "tern_cont");
    Builder.CreateCondBr(cond_val.val, then_bb, else_bb);

    // emit code for then branch
    Builder.SetInsertPoint(then_bb);
    value_llvm then_val = expression_codegen(true_expr);
    assert(then_val.val);
    Builder.CreateBr(merge_bb);
    then_bb = Builder.GetInsertBlock();

    // emit code for else branch
    CurrFunction->getBasicBlockList().push_back(else_bb);
    Builder.SetInsertPoint(else_bb);
    value_llvm else_val = valexpr_codegen(false_expr);
    assert(else_val.val);
    Builder.CreateBr(merge_bb);
    else_bb = Builder.GetInsertBlock();

    // emit the merge block
    CurrFunction->getBasicBlockList().push_back(merge_bb);
    Builder.SetInsertPoint(merge_bb);
    if (!then_val.ty.Ty->isIntegerTy() || !else_val.ty.Ty->isIntegerTy()) {
        make_common_ftype(then_val, else_val);
    } else {
        make_common_itype(then_val, else_val);
    }

    llvm::PHINode *phi_node = Builder.CreatePHI(then_val.val->getType(), 2, "merge_phi");
    phi_node->addIncoming(then_val.val, then_bb);
    phi_node->addIncoming(else_val.val, else_bb);
    return phi_node;
}

// conditional_expression codegen
value_llvm valexpr_codegen(struct _ast_node *valexpr) {
    if (valexpr->node_type == TERNOP)
        return ternop_codegen(valexpr);
    else if (isbin_op(valexpr->node_type)) {
        return binop_codegen(valexpr->children[0], valexpr->children[1], valexpr->node_type);
    } else {
        // else unary_expression
        return unaryexpr_codegen(valexpr);
    }
}

value_llvm assignop_codegen(struct _ast_node *assign_expr) {
    struct _ast_node *unary_expr = assign_expr->children[0];
    struct _ast_node *assign_expr1 = assign_expr->children[1];
    if (unary_expr->node_type != NODE_TYPE::ID) {
        fprintf(stderr, "error: left hand side of an assignment must be a variable\n");
        exit(1);
    }
    value_llvm right = assignexpr_codegen(assign_expr1);
    enum NODE_TYPE op;
    switch(assign_expr->node_type) {
        case MULASSIGN:
            op = MULT;break;
        case DIVASSIGN:
            op = DIV;break;
        case MODASSIGN:
            op = MOD;break;
        case ADDASSIGN: 
            op = PLUS;break;
        case SUBASSIGN:
            op = MINUS;break;
        case LEFTASSIGN: 
            op = LEFT_SHIFT;break;
        case RIGHTASSIGN:
            op = RIGHT_SHIFT;break;
        case BITANDASSIGN:
            op = BITAND;break;
        case BITXORASSIGN:
            op = BITXOR;break;
        case BITORASSIGN:
            op = BITOR;break;
        case ASSIGN:
            op=ASSIGN;break;
        default:
            fprintf(stderr, "not an assign operator\n");
            exit(1);
    }
    if (op!=ASSIGN) {
        value_llvm left = unaryexpr_codegen(unary_expr);
        right = binop_codegen(left, right, op);
    }
    return store_codegen(unary_expr->node_val, right);
}

value_llvm assignexpr_codegen(struct _ast_node *assign_expr) {
    bool isval_expr = !isassign_op(assign_expr->node_type);
    if (!isval_expr) {
        return assignop_codegen(assign_expr);
    } else {
        // conditional_expression codegen
        return valexpr_codegen(assign_expr);
    }
}

value_llvm initializer_codegen(struct _ast_node *initializer) {
    assert(initializer->node_type == INIT_ASSIGN_EXPR || initializer->node_type == INIT_LIST);
    if (initializer->node_type == INIT_LIST) {
        fprintf(stderr, "error: initializer lists are not handled\n");
        exit(1);
    } else {
        // intializer->node_type == INIT_ASSIGN_EXPR
        struct _ast_node *assign_expr = initializer->children[0];
        return assignexpr_codegen(assign_expr);
    }
}

std::vector<value_llvm> initdecl_list_codegen(struct _ast_node *init_decl_list, struct _ast_node *decl_spec) {
    std::vector<value_llvm> vals;
    if (!init_decl_list) {
        fprintf(stderr, "warning: useless type name in empty declaration\n");
    } else {
        struct _ast_node **init_decl;
        struct _ast_node *declarator;
        struct _ast_node *initializer;
        struct _ast_node *ptr;
        for(init_decl = init_decl_list->children;*init_decl!=NULL;init_decl++) {
            // each child of *init_decl is either declarator or initializer
            declarator = (*init_decl)->children[0];
            initializer = (*init_decl)->children[1];

            ptr = declarator->children[1];
            type_llvm idtype = get_type(decl_spec, ptr);
            value_llvm declarator_value = declarator_codegen(declarator, idtype);
            if (initializer) {
                value_llvm initializer_value = initializer_codegen(initializer);
                if (auto func = llvm::dyn_cast<llvm::Function>(declarator_value.val)) {
                    fprintf(stderr, "error: cannot initialize function declarations\n");
                    exit(1);
                } else if (auto globalvar = llvm::dyn_cast<llvm::GlobalVariable>(declarator_value.val)) {
                    if (auto constval = llvm::dyn_cast<llvm::Constant>(initializer_value.val)) {
                        globalvar->setInitializer(constval);
                        vals.push_back(declarator_value);
                    }
                    else {
                        fprintf(stderr, "error: initializer is not constant for global variable `%s`\n", declarator_value.val->getName().str().c_str());
                        exit(1);
                    }
                } else {
                    // local variable declaration
                    store_codegen(declarator->children[0]->children[0]->node_val, initializer_value);
                    vals.push_back(declarator_value);
                }
            }

        }
    }
    return vals;
}

void declaration_codegen(struct _ast_node *declaration) {
    assert(declaration->node_type == DECLARATION);
    struct _ast_node *decl_spec = declaration->children[0];
    struct _ast_node *init_decl_list = declaration->children[1];
    std::vector<value_llvm> vals = initdecl_list_codegen(init_decl_list, decl_spec);
}

void blkitem_codegen(struct _ast_node *root) {
    // root is child node if blkitem_list
    switch(root->node_type) {
        // declaration
        case DECLARATION:
            declaration_codegen(root);
            break;
        default:
            stmt_codegen(root);
        // statement
    }
}

// callee side code
value_llvm function_call_codegen(struct _ast_node *func_call) {
    assert(func_call->node_type == FUNCTION_CALL);
    char *func_name = func_call->children[0]->node_val;

    function_llvm func_val = NamedFunctions.at(func_name);
    // no outofrange exception has this has already been checked by scope checker phase

    auto *func_llvmval = llvm::dyn_cast<llvm::Function>(func_val.val);
    if (!func_llvmval) {
        fprintf(stderr, "function value not generated by function declaration\n");
        exit(1);
    }
    
    // check for argument mismatch
    struct _ast_node *arg_list = func_call->children[1];   // arg_list->node_type = ARG_EXP_LIST
    int num_args = 0;
    if (arg_list) 
        for(num_args = 0;arg_list->children[num_args] != NULL;num_args++);

    if (num_args != func_llvmval->arg_size()) {
        fprintf(stderr, "error: invalid number arguments passed to `%s`\n", func_name);
        exit(1);
    }
    
    std::vector<value_llvm> ArgsV(num_args);
    for (unsigned int i = 0; i != num_args; ++i) {
        ArgsV[i] = assignexpr_codegen(arg_list->children[i]);       // arg_list->children[i] is a node for assignment_expression
        assert(ArgsV[i].val && "error while generating code for arguments of function call");
        if (func_val.param_tylist[i].Ty->isIntegerTy())
            intcast_type(ArgsV[i], func_val.param_tylist[i]);
    }

    std::vector<llvm::Value *> llvmArgsV(num_args);
    for (unsigned int i = 0; i != num_args; ++i) {
        llvmArgsV[i] = ArgsV[i].val;
    }
    llvm::Value *rawret = Builder.CreateCall(func_llvmval, llvmArgsV, "calltmp");   
    value_llvm retval = value_llvm(rawret, rawret->getType());                      
    intcast_type(retval, func_val.ty);
    return retval;
} 

// get return type of function_decl from DECL node (you cannot get it from function_decl)
function_llvm function_decl_codegen(struct _ast_node *fun_decl, type_llvm retty) {
    if (fun_decl->node_type == FUNCTION_PTRDECL) {
        fprintf(stderr, "error: function declarations are not supported\n");
        exit(1);
    }
    assert(fun_decl->node_type == FUNCTION_DECL);
    struct _ast_node *param_list = fun_decl->children[1];   // node->type == PARAM_LIST
    std::vector<llvm::Type *> paramtypes;
    std::vector<type_llvm> param_tylist;
    if (param_list) {
        // iterate over children of param_list (param_decl nodes), 
        //get their types from declaration specifiers, (first child of param_Decl node)
        struct _ast_node **param_decl;
        struct _ast_node *decl_spec;
        struct _ast_node *declarator;
        struct _ast_node *ptr_node;
        for(param_decl = param_list->children;*param_decl != NULL;param_decl++) {
            if ((*param_decl)->node_type == NODE_TYPE::ELLIPSIS_NODE) {
                fprintf(stderr, "error: support for var-arg list is not there for code generation\n");
                exit(1);
            }
            decl_spec = (*param_decl)->children[0]; // (*param_decl)->children[0] is DECL_SPEC node
            declarator = (*param_decl)->children[1];
            if (declarator)
                ptr_node = declarator->children[1];
            
            type_llvm paramty = get_type(decl_spec, ptr_node);
            paramtypes.push_back(paramty.Ty);
            param_tylist.push_back(paramty);
            ptr_node = NULL;
        }
    }
    llvm::FunctionType *FT = llvm::FunctionType::get(retty.Ty, paramtypes, false); 
    // false parameter indicates that it is not a vararg function, we might handel that later
    // TODO: add support for vararg (check for ellipsis node)
    llvm::Function* F = llvm::Function::Create(FT,
                                         llvm::Function::ExternalLinkage,
                                          fun_decl->children[0]->node_val, // root->children[0]->node_type == ID
                                          TheModule.get());
    // to continue:
    // set names for all the arguments from the argument vector (argv)
    std::vector<std::string> argv = getfun_argv(fun_decl);
    unsigned idx = 0;
    for(auto &Arg : F->args()) {
        // assert(argv[idx]);
        Arg.setName(argv[idx++]);
    }
    function_llvm toret = function_llvm(F, param_tylist, retty);
    NamedFunctions[fun_decl->children[0]->node_val] = toret;
    return toret;
}

// caller side code
void function_def_codegen(struct _ast_node *root) {
    assert(root->node_type == FUNCTION_DEF);
    // first check for existing decl of function
    struct _ast_node *decl_spec = root->children[0];        // decl_spec->node_type == DECL_SPEC
    struct _ast_node *prototype = root->children[1];        // prototype->node_type == DECLARATOR
    struct _ast_node *retptr = prototype->children[1];      // retptr->node_type == PTR or retptr may be null
    struct _ast_node *fun_decl = prototype->children[0];    // fun_decl->node_type == FUNCTION_DECL

    const char *fun_name = fun_decl->children[0]->node_val;
    function_llvm function = (NamedFunctions.find(fun_name) != NamedFunctions.end()) ?
                             NamedFunctions[fun_name] :
                             function_llvm(nullptr, std::vector<type_llvm>(), type_llvm());
    type_llvm rettype = get_type(decl_spec, retptr);

    if (!function.val) {
        // it has not been declared/defined already
        function = function_decl_codegen(fun_decl, rettype); // prototype needs to be generated with named arguments
        if (!function.val) {
            fprintf(stderr, "error generating code for function declaration of `%s`\n", fun_name);exit(1);
        }
        NamedFunctions[fun_name] = function;
    } else {
        // function has either already been declared or defined
        llvm::Function *TheFunction = TheModule->getFunction(fun_name);
        if (!TheFunction->empty()) {
            fprintf(stderr, "log-error: function `%s` is defined more than once", fun_name);
        }
    }
    llvm::Function *TheFunction = llvm::dyn_cast<llvm::Function>(function.val);
    if (!TheFunction) {
        fprintf(stderr, "error generating code for function declaration of `%s`\n", fun_name);
        exit(1);
    }
    // Create a new basic block to start insertion into
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    // Record the function arguments in the named-values map (push a new scope)
    enter_scope(NamedValues);
    assert(NotFoundLabels.empty());
    assert(LabelTable.empty());
    unsigned int i = 0;
    for(auto &Arg : TheFunction->args()) {
        // create an alloca instr for this param
        value_llvm Alloca(CreateEntryBlockAlloca(TheFunction, function.param_tylist[i].Ty, Arg.getName()),
                            function.param_tylist[i]); 

        // store the initial value into the alloca
        Builder.CreateStore(&Arg, Alloca.val);

        // add argument to the symbol table
        add_symbol(NamedValues, Arg.getName(), Alloca);
        i++;
    }
    struct _ast_node *cmpd_stmt = root->children[2]; // cmp_stmt->node_type == CMPND_STMT
    CurrFunction = TheFunction;
    cmpdstmt_codegen(cmpd_stmt, true);
    llvm::BasicBlock *insert_blk = Builder.GetInsertBlock();

    if (strncmp(insert_blk->getName().str().c_str(), "ret_end", 7 * sizeof(char))) {
        if (!rettype.Ty->isVoidTy()) {
            fprintf(stderr, "warning : No return statement at the end in non void function `%s`\nnote: you may ignore this warning if you've ensured that all the possible execution paths have return statement\n", CurrFunction->getName().str().c_str());
            Builder.CreateRet(llvm::UndefValue::get(TheFunction->getReturnType())); // returns unspecified bit pattern
        } else {
            Builder.CreateRetVoid();
        }
    }
    
    exit_scope(NamedValues);
    LabelTable.clear();
    NotFoundLabels.clear();
    CurrFunction = nullptr;
    llvm::BasicBlock *end_blk = Builder.GetInsertBlock();
    if (end_blk->empty()) {
        end_blk->eraseFromParent();
    }
    llvm::verifyFunction(*TheFunction);
}


value_llvm constexpr_codegen(struct _ast_node *const_expr) {
    return valexpr_codegen(const_expr);
}

// ====== statements ======

void stmt_codegen(struct _ast_node *stmt) {
    switch(stmt->node_type) {
        case LABELED_STMT:
            labeledstmt_codegen(stmt);
            break;
        case CASE_STMT:
            casestmt_codegen(stmt);
            break;
        case DEF_STMT:
            defstmt_codegen(stmt);
            break;
        case CMPND_STMT:
            cmpdstmt_codegen(stmt, false);
            break;
        case EXPR_STMT:
            expressionstmt_codegen(stmt);
            break;
        case IF_ELSE_STMT:
        case IF_STMT:
        case SWITCH_STMT:
            selectstmt_codegen(stmt);
            break;
        case WHILE_STMT:
        case DO_WHILE_STMT:
        case FOR_STMT1:
        case FOR_STMT2:
        case FOR_STMT3:
        case FOR_STMT4:
            iterationstmt_codegen(stmt);
            break;
        case GOTO_STMT:
        case CONTINUE_STMT:
        case BREAK_STMT:
        case RETURN_STMT1:
        case RETURN_STMT2:
            jumpstmt_codegen(stmt);
            break;
        default:
            fprintf(stderr, "error: unexpected statement type\n");
            exit(1);
    }
}

// labeled statement

void labeledstmt_codegen(struct _ast_node *label_stmt) {
    struct _ast_node *identifier = label_stmt->children[0];
    struct _ast_node *statement = label_stmt->children[1];

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(TheContext, identifier->node_val, CurrFunction);
    ADD_LABEL(identifier->node_val, LabelTable, bb, NotFoundLabels, true);

    llvm::BasicBlock *insert_blk = Builder.GetInsertBlock();
    if (insert_blk->empty()) {
        insert_blk->replaceAllUsesWith(bb);
        Builder.GetInsertBlock()->eraseFromParent();
    } else Builder.CreateBr(bb);


    Builder.SetInsertPoint(bb);
    stmt_codegen(statement);
}

// case statement

void casestmt_codegen(struct _ast_node *case_stmt) {
    struct _ast_node *const_expr = case_stmt->children[0];
    struct _ast_node *stmt = case_stmt->children[1];
    if (switch_inst.empty()) {
        fprintf(stderr, "error: case statement is not enclosed within switch\n");
        exit(1);
    }

    value_llvm expr_val = constexpr_codegen(const_expr);
    auto const_val = llvm::dyn_cast<llvm::ConstantInt>(expr_val.val);
    
    if (!const_val) {
        fprintf(stderr, "case statement expression must be a constant integer\n");
        exit(1);
    }

    std::string casestmt_label = "case" + const_val->getValue().toString(10, true); // only signed integers are supported
    // push_label(LabelValues) must be pushed by switch statement first
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(TheContext, casestmt_label, CurrFunction);
    int ret = PUSH_LABEL_TOP(casestmt_label, LabelValues, bb);  // need to pop this (in switch_codegen)
    if (ret == PUSH_LABEL_TOP_FAIL) {
        fprintf(stderr, "duplicate case value `%s`", const_val->getValue().toString(10, true).c_str());
        exit(1);
    }
    // ret == PUSH_LABEL_TOP_SUCCESS

    llvm::BasicBlock *insert_blk = Builder.GetInsertBlock();
    if (insert_blk->empty()) {
        insert_blk->replaceAllUsesWith(bb);
        Builder.GetInsertBlock()->eraseFromParent();
    } else Builder.CreateBr(bb);

    Builder.SetInsertPoint(bb);
    stmt_codegen(stmt);

    switch_inst.back()->addCase(const_val, bb);
}

// default_stmt
llvm::BasicBlock *defstmt_codegen(struct _ast_node *defstmt) {
    struct _ast_node *stmt = defstmt->children[0];
    
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(TheContext, "default", CurrFunction);
    int ret = PUSH_LABEL_TOP("default", LabelValues, bb);   // need to pop this (in switch_codegen)
    if (ret == PUSH_LABEL_TOP_FAIL) {
        fprintf(stderr, "multiple default labels in one switch\n");
        exit(1);
    }
    // ret == PUSH_LABEL_TOP_SUCCESS

    llvm::BasicBlock *insert_blk = Builder.GetInsertBlock();
    if (insert_blk->empty()) {
        insert_blk->replaceAllUsesWith(bb);
        Builder.GetInsertBlock()->eraseFromParent();
    } else Builder.CreateBr(bb);
    
    Builder.SetInsertPoint(bb);
    stmt_codegen(stmt);
    return bb;
}

// compound_statement

void cmpdstmt_codegen(struct _ast_node *root, bool is_fundef) {
    assert(root->node_type == CMPND_STMT);
    struct _ast_node *blk_item_list = root->children[0];
    if (!is_fundef) {
        enter_scope(NamedValues);
    }
    if (blk_item_list) {
        struct _ast_node **blkitem;    // (*blk_item)->node_type == DECL or statement(lots of them)
        for(blkitem = blk_item_list->children;*blkitem!=NULL;blkitem++) {
            blkitem_codegen(*blkitem);
        }
    }
    if (!is_fundef) {
        exit_scope(NamedValues);
    }    
}

// expression_statement

void expressionstmt_codegen(struct _ast_node *expr_stmt) {
    struct _ast_node *expr = expr_stmt->children[0];
    if (expr) expression_codegen(expr);
}

// selection_statement

void selectstmt_codegen(struct _ast_node *select_stmt) {
    switch(select_stmt->node_type) {
        case IF_ELSE_STMT:
        case IF_STMT:
            ifelsestmt_codegen(select_stmt);break;
        case SWITCH_STMT:
            switchstmt_codegen(select_stmt);break;
        default:
            fprintf(stderr, "not a select statement\n");exit(1);
    }
}

// if else statement
// for both IF_ELSE_STMT and IF_STMT
void ifelsestmt_codegen(struct _ast_node *ifelse_stmt) {
    assert(ifelse_stmt->node_type == NODE_TYPE::IF_ELSE_STMT 
        || ifelse_stmt->node_type == NODE_TYPE::IF_STMT);

    struct _ast_node *cond = ifelse_stmt->children[0];  // expression
    struct _ast_node *thenstmt = ifelse_stmt->children[1];  // statement
    struct _ast_node *elsestmt = ifelse_stmt->children[2];  // statement

    value_llvm cond_val = expression_codegen(cond);
    assert(cond_val.val && "cannot compute if's condition");

    // deadcode elimination optimization
    auto const_cond_val = llvm::dyn_cast<llvm::Constant>(cond_val.val);
    if (const_cond_val) {
        if (const_cond_val->isZeroValue()) {
            if (!has_labeled_stmt(thenstmt)) {
                if (elsestmt)
                    stmt_codegen(elsestmt);
                return;
            }
        } else {
            // non zero value
            if (!has_labeled_stmt(elsestmt)) {
                stmt_codegen(thenstmt);
                return;
            }
        }
    }
    zero_val = ZERO_VAL;
    cond_val = binop_codegen(cond_val, zero_val, NODE_TYPE::NEQOP, false);

    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(TheContext, "then", CurrFunction);
    llvm::BasicBlock *else_bb;
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "ifcont");

    if (elsestmt)
        else_bb = llvm::BasicBlock::Create(TheContext, "else");
    else
        else_bb = merge_bb;

    Builder.CreateCondBr(cond_val.val, then_bb, else_bb);
    // emit then block
    Builder.SetInsertPoint(then_bb);
    stmt_codegen(thenstmt);
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(merge_bb);
    }

    // emit else block (if elsestmt is null then we push_back the merge_bb)
    CurrFunction->getBasicBlockList().push_back(else_bb);
    if (elsestmt) {
        Builder.SetInsertPoint(else_bb);
        stmt_codegen(elsestmt);
        if (!Builder.GetInsertBlock()->getTerminator()) {
            Builder.CreateBr(merge_bb);
        }

        CurrFunction->getBasicBlockList().push_back(merge_bb);

    }
    Builder.SetInsertPoint(merge_bb);
}

// TODO: finish switch statement code-generation
void switchstmt_codegen(struct _ast_node *switchstmt) {
    assert(switchstmt->node_type == NODE_TYPE::SWITCH_STMT);
    struct _ast_node *switch_expr = switchstmt->children[0];   // expression (constant-expression)
    struct _ast_node *switch_stmt = switchstmt->children[1];   // statement

    value_llvm switchexpr_val = expression_codegen(switch_expr);
    // auto switch_begin = llvm::BasicBlock::Create(TheContext, "switch_begin", CurrFunction);
    auto switch_cont = llvm::BasicBlock::Create(TheContext, "switch_cont");

    // Builder.SetInsertPoint(switch_begin);
    switch_inst.push_back(Builder.CreateSwitch(switchexpr_val.val, switch_cont));
    ENTER_LABEL_BLOCK(LabelValues);     // need to pop these after generating code for switch_stmt

    push_break(switch_cont);
    stmt_codegen(switch_stmt);
    llvm::BasicBlock *def_bb = GET_DEF_FROM_TOP(LabelValues);
    if (def_bb) {
        switch_inst.back()->setDefaultDest(def_bb);
    }
    if (switch_cont->getNumUses() > 0) {
        Builder.CreateBr(switch_cont);
        CurrFunction->getBasicBlockList().push_back(switch_cont);
        Builder.SetInsertPoint(switch_cont);
    }

    switch_inst.pop_back();
    pop_break();
    POP_LABEL_BLOCK(LabelValues);
}

// iteration statement

void iterationstmt_codegen(struct _ast_node *it_stmt) {
    switch(it_stmt->node_type) {
        case WHILE_STMT:
            whilestmt_codegen(it_stmt);break;
        case DO_WHILE_STMT:
            dowhilestmt_codegen(it_stmt);break;
        case FOR_STMT1:
        case FOR_STMT2:
        case FOR_STMT3:
        case FOR_STMT4:
            forstmt_codegen(it_stmt);break;
        default:
            break;
    }   
}

void whilestmt_codegen(struct _ast_node *while_stmt) {
    assert(while_stmt->node_type == NODE_TYPE::WHILE_STMT);
    struct _ast_node *cond = while_stmt->children[0];   // expression
    struct _ast_node *body = while_stmt->children[1];   // statement

    llvm::BasicBlock *cond_bb = llvm::BasicBlock::Create(TheContext, "while_cond", CurrFunction);
    auto br_cond = Builder.CreateBr(cond_bb);

    Builder.SetInsertPoint(cond_bb);
    value_llvm cond_val = expression_codegen(cond);

    // deadcode elimination optimization
    auto const_cond_val = llvm::dyn_cast<llvm::Constant>(cond_val.val);
    if (const_cond_val && !has_labeled_stmt(body)) {
        if (const_cond_val->isZeroValue()) {
            br_cond->eraseFromParent();
            Builder.SetInsertPoint(cond_bb->getPrevNode());
            cond_bb->eraseFromParent();
            return;
        }
    }
    zero_val = ZERO_VAL;

    cond_val = binop_codegen(cond_val, zero_val, NODE_TYPE::NEQOP, false);
    llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(TheContext, "while_body", CurrFunction);
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "while_cont");

    Builder.CreateCondBr(cond_val.val, body_bb, merge_bb);
    
    // emit code for body
    push_continue(body_bb); // these need to be pop'ed appropriately
    push_break(merge_bb);
    Builder.SetInsertPoint(body_bb);
    stmt_codegen(body);
    Builder.CreateBr(cond_bb);
    pop_continue();
    pop_break();

    // emit code for end/merge_bb
    CurrFunction->getBasicBlockList().push_back(merge_bb);
    Builder.SetInsertPoint(merge_bb);
}

void dowhilestmt_codegen(struct _ast_node *dowhile_stmt) {
    assert(dowhile_stmt->node_type == NODE_TYPE::DO_WHILE_STMT);
    struct _ast_node *cond = dowhile_stmt->children[1];   // expression
    struct _ast_node *body = dowhile_stmt->children[0];   // statement

    llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(TheContext, "dowhile_body", CurrFunction);
    llvm::BasicBlock *cond_bb = llvm::BasicBlock::Create(TheContext, "dowhile_cond");
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "dowhile_cont");

    // emit code for body
    push_continue(cond_bb);
    push_break(merge_bb);
    Builder.CreateBr(body_bb);
    Builder.SetInsertPoint(body_bb);
    stmt_codegen(body);
    pop_continue();
    pop_break();

    // emit code for cond
    Builder.CreateBr(cond_bb);
    CurrFunction->getBasicBlockList().push_back(cond_bb);
    Builder.SetInsertPoint(cond_bb);

    value_llvm cond_val = expression_codegen(cond);
    zero_val = ZERO_VAL;
    cond_val = binop_codegen(cond_val, zero_val, NODE_TYPE::NEQOP, false);
    Builder.CreateCondBr(cond_val.val, body_bb, merge_bb);

    // emit code for merge block
    CurrFunction->getBasicBlockList().push_back(merge_bb);
    Builder.SetInsertPoint(merge_bb);
}

void forstmt_codegen(struct _ast_node *for_stmt) {
    fprintf(stderr, "error: for-loops are not supported for code-generation\n");
    exit(1);
}

// jump statement

void jumpstmt_codegen(struct _ast_node *jmp_stmt) {
    switch(jmp_stmt->node_type) {
        case GOTO_STMT:
            goto_codegen(jmp_stmt);
            break;
        case CONTINUE_STMT:
            continue_codegen(jmp_stmt);
            break;
        case BREAK_STMT:
            break_codegen(jmp_stmt);
            break;
        case RETURN_STMT1:
        case RETURN_STMT2:
            return_codegen(jmp_stmt);
            break;
        default:
            fprintf(stderr, "not a jump statement\n");
            exit(1);
    }
}

void goto_codegen(struct _ast_node *goto_stmt) {
    llvm::BasicBlock *goto_end = llvm::BasicBlock::Create(TheContext, "goto_end", CurrFunction);
    llvm::BranchInst *brinst = Builder.CreateBr(Builder.GetInsertBlock());
    Builder.SetInsertPoint(goto_end);
    std::string name = goto_stmt->children[0]->node_val;

    auto bb = LabelTable.find(name);
    if (bb == LabelTable.end()) {
        NotFoundLabels.insert({name, brinst});
    } else {
        llvm::BasicBlock *dest_blk = bb->second;
        brinst->setSuccessor(0, dest_blk);
    }
}

void continue_codegen(struct _ast_node *cont_stmt) {
    if (continue_dest_list.empty()) {
        fprintf(stderr, "error: continue statement not within a loop\n");
        exit(1);
    }

    Builder.CreateBr(continue_dest_list.back());
    llvm::BasicBlock *contend_bb = llvm::BasicBlock::Create(TheContext, "cont_end", CurrFunction);
    Builder.SetInsertPoint(contend_bb);
} 

void break_codegen(struct _ast_node *break_stmt) {
    if (break_dest_list.empty()) {
        fprintf(stderr, "error: break statement not within a loop or switch\n");
        exit(1);
    }

    Builder.CreateBr(break_dest_list.back());
    llvm::BasicBlock *breakend_bb = llvm::BasicBlock::Create(TheContext, "break_end", CurrFunction);
    Builder.SetInsertPoint(breakend_bb);
}

void return_codegen(struct _ast_node *ret_stmt) {
    // RETURN_STMT1 or RETURN_STMT2
    
    struct _ast_node *ret_expr = ret_stmt->children[0];
    if (!CurrFunction) {
        fprintf(stderr, "return statement outside of a function\n");
        exit(1);
    }

    llvm::Type *retty = CurrFunction->getReturnType();
    
    if (retty->isVoidTy() && ret_expr) {
        fprintf(stderr, "`return` with a value, in function `%s` which returns void\n", CurrFunction->getName().str().c_str());
        exit(1);
    } else if (!retty->isVoidTy() && !ret_expr) {
        fprintf(stderr, "`return` with no value, in function `%s` which returns non-void\n", CurrFunction->getName().str().c_str());
        exit(1);
    }

    if (ret_expr) {
        // RETURN_STMT2
        value_llvm retval = expression_codegen(ret_expr);
        intcast_type(retval, retty);
        Builder.CreateRet(retval.val);
    } else {
        // RETURN_STMT1
        Builder.CreateRetVoid();
    }
    
    llvm::BasicBlock *retend_bb = llvm::BasicBlock::Create(TheContext, "ret_end", CurrFunction);
    Builder.SetInsertPoint(retend_bb);
}

// translation-unit

void externaldecl_codegen(struct _ast_node *extern_decl) {
    switch(extern_decl->node_type) {
        case FUNCTION_DEF:
            function_def_codegen(extern_decl);
            break;
        case DECLARATION:
            declaration_codegen(extern_decl);
            break;
        default:
            fprintf(stderr, "unexpected node type\n");
            break;
    }
}

void translationunit_codegen(struct _ast_node *trans_unit) {
    assert(trans_unit->node_type== NODE_TYPE::TRANSLATION_UNIT);
    struct _ast_node **external_declaration;
    enter_scope(NamedValues);
    for(external_declaration = trans_unit->children;*external_declaration;external_declaration++) {
        externaldecl_codegen(*external_declaration);
    }
    exit_scope(NamedValues);
}

void codegen(struct _ast_node *root) {
    translationunit_codegen(root);
}

static void InitializeModule() {
    // Open a module.
    TheModule = std::make_unique<llvm::Module>("llvm ir", TheContext);
}

void print_module(struct _ast_node *root, std::string &out_filename) {
    InitializeModule();
    codegen(root);
    std::error_code err;
    llvm::raw_fd_ostream file(out_filename, err);
    if (err) {
      printf("error: couldn't open file : %s\n", err.message().c_str());
      exit(1);
    }
    TheModule->print(file, nullptr);
}
