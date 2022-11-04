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


#define ZERO_VAL llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), "0", 10))
#define ONE_VAL llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), "1", 10))
// zero_val is llvm::Value*

static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;
static symbol_table NamedValues;

static llvm::Function *CurrFunction;    // put it in all location required (namely inside function definition)
                                        
static label_stack LabelValues;         // only for maintaining case/default statements (which might be nested)
                                        
static label_table LabelTable;          // for only LABELED_STMT (not for case/default)
                                        // search for label from goto statements will only search LabelTable
                                        // you clear label table only if you exit function definition (not neccessarily any cmpn_stmt)

static std::unordered_multimap<std::string, llvm::BranchInst *> NotFoundLabels;

static std::vector<llvm::BasicBlock *> continue_dest_list;
static std::vector<llvm::BasicBlock *> break_dest_list;

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


static llvm::AllocaInst *CreateVariableAlloca(llvm::Type *Ty, const std::string &VarName) {
    return Builder.CreateAlloca(Ty, 0, VarName);
}

llvm::Type *get_type(struct _ast_node *decl_spec, struct _ast_node *ptr) {
    assert(decl_spec->node_type == DECLARATION_SPEC);
    assert(ptr->node_type == PTR);
}

llvm::Type *get_type_decl_spec(struct _ast_node *node) {
    assert(node->node_type == DECLARATION_SPEC);
    // get type from decl spec node
}

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
    struct _ast_node **param_decl;
    struct _ast_node *declarator;
    struct _ast_node *id_decl;
    for(param_decl = param_list->children;*param_decl != NULL;++param_decl) {
        declarator = (*param_decl)->children[1];
        assert(declarator);
        // assuming that we have no function pointer being passed in our language for now we may try to extend this later
        id_decl = declarator->children[0];
        assert(id_decl->node_type == IDENTIFIER_DECL);
        argv.push_back(id_decl->children[0]->node_val);
    }
    return argv;
}

// val1 and val2 are both integer types
void make_common_type(llvm::Value *val1, llvm::Value *val2) {
    auto t1 = llvm::dyn_cast<llvm::IntegerType>(val1->getType());
    auto t2 = llvm::dyn_cast<llvm::IntegerType>(val2->getType());    
    assert(t1);
    assert(t2);

    int bits1 = t1->getBitWidth();
    int bits2 = t2->getBitWidth();

    llvm::Type *common_type;
    if (bits1 < bits2)
        common_type = val2->getType();
    else 
        common_type = val1->getType();
    
    val1 = Builder.CreateSExtOrTrunc(val1, common_type);
    val2 = Builder.CreateSExtOrTrunc(val2, common_type);
}

inline void convert_bool(llvm::Value *val) {
    val = Builder.CreateICmpEQ(val, ZERO_VAL);
    val = Builder.CreateZExtOrBitCast(val, llvm::IntegerType::get(TheContext, sizeof(int)));
}

void cast_type(llvm::Value *val, llvm::Type *Ty) {
    auto t1 = llvm::dyn_cast<llvm::IntegerType>(val->getType());
    auto t2 = llvm::dyn_cast<llvm::IntegerType>(Ty);    
    if (!t1 || !t2) {
        fprintf(stderr, "warning: type mismatch for assignment of `%s`", val->getName().str().c_str());
        return;
    }
    Builder.CreateSExtOrTrunc(val, Ty);
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

llvm::Value *constant_codegen(struct _ast_node *root) {
    assert(root->node_type == I_CONST 
        || root->node_type == F_CONST);
    
    if (root->node_type == I_CONST) {
        return llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), atoi(root->node_val), true));
    }
    if (root->node_type == F_CONST) {
        return llvm::ConstantFP::get(TheContext, llvm::APFloat(atof(root->node_val)));
    }
}

llvm::Value *string_codegen(struct _ast_node *string_node) {
    assert(string_node->node_type == NODE_TYPE::STRING);
    llvm::GlobalVariable *str = Builder.CreateGlobalString(string_node->node_val);
    std::vector<llvm::Value *> indices{ZERO_VAL, ZERO_VAL};
    llvm::Value *ptr = Builder.CreateInBoundsGEP(str, indices);
    return ptr; // assumed to be `signed char*`
}

// id is on stack
llvm::Value *id_codegen(struct _ast_node *root) {
    assert(root->node_type == ID);
    llvm::Value *v = find_symbol(NamedValues, root->node_val);
    if (!v) LogErrorV(root->node_val);
    return Builder.CreateLoad(v, root->node_val);
}

llvm::Value *binop_codegen(struct _ast_node *left, struct _ast_node *right, enum NODE_TYPE op) {
    // do binary operator codegen
    llvm::Value *lhs = valexpr_codegen(left);
    llvm::Value *rhs = valexpr_codegen(right);
    binop_codegen(lhs, rhs, op);
}

llvm::Value *binop_codegen(llvm::Value *lhs, llvm::Value *rhs, enum NODE_TYPE op) {
    if (!lhs || !rhs) return nullptr;
    
    llvm::Value *cmp_ret;
    if (!lhs->getType()->isIntegerTy() || !rhs->getType()->isIntegerTy()) {
        // lhs or rhs is a float type
        switch(op) {
            case MULT:
                return Builder.CreateFMul(lhs, rhs, "multmp");
            case DIV:
                return Builder.CreateFDiv(lhs, rhs, "divtmp");
            case PLUS:
                return Builder.CreateFAdd(lhs, rhs, "addtmp");
            case MINUS:
                return Builder.CreateFSub(lhs, rhs, "subtmp");
            case LESS_THAN:
                // O version of compare returns true only if both lhs and rhs are not NaN and comp holds
                cmp_ret = Builder.CreateFCmpOLT(lhs, rhs, "lttemp");
                break;
            case GREATER_THAN:
                cmp_ret = Builder.CreateFCmpOGT(lhs, rhs, "gttemp");
                break;
            case LESSEQ:
                cmp_ret = Builder.CreateFCmpOLE(lhs, rhs, "letemp");
                break;
            case GREATEREQ:
                cmp_ret = Builder.CreateFCmpOGE(lhs, rhs, "getemp");
                break;
            case EQOP:
                cmp_ret = Builder.CreateFCmpOEQ(lhs, rhs, "eqtemp");
                break;
            case NEQOP:
                cmp_ret = Builder.CreateFCmpONE(lhs, rhs, "netemp");
                break;
            default:
                // binary operators that are not supported
                fprintf(stderr, "error: binary operator `%s` not supported on floats\n", binop_tostr(op));
                exit(1);
                return nullptr; // redundant
        }
        if (!cmp_ret) 
            return nullptr;
        return Builder.CreateZExtOrBitCast(cmp_ret, Builder.getInt32Ty());
    } else {
        // both lhs and rhs are int type
        // before doing any operation on them, need to convert them into same type for any llvm-opcode
        make_common_type(lhs, rhs);
        // currently all operations are `signed int` operations
        // TODO: make extensions to support unsigned int and its operations 
        switch(op) {
            case MULT:
                return Builder.CreateMul(lhs, rhs, "multmp");
            case DIV:
                return Builder.CreateSDiv(lhs, rhs, "divtmp");
            case PLUS:
                return Builder.CreateAdd(lhs, rhs, "addtmp");
            case MINUS:
                return Builder.CreateSub(lhs, rhs, "subtmp");
            case LESS_THAN:
                return Builder.CreateZExtOrBitCast(Builder.CreateICmpSLT(lhs, rhs, "lttemp"),
                                                     Builder.getInt32Ty());
            case GREATER_THAN:
                return Builder.CreateZExtOrBitCast(Builder.CreateICmpSGT(lhs, rhs, "gttemp"), 
                                                    Builder.getInt32Ty());
            case LESSEQ:
                return Builder.CreateZExtOrBitCast(Builder.CreateICmpSLE(lhs, rhs, "letemp"),
                                                    Builder.getInt32Ty());
            case GREATEREQ:
                return Builder.CreateZExtOrBitCast(Builder.CreateICmpSGE(lhs, rhs, "getemp"),
                                                    Builder.getInt32Ty());
            case EQOP:
                return Builder.CreateZExtOrBitCast(Builder.CreateICmpEQ(lhs, rhs, "eqtemp"),
                                                    Builder.getInt32Ty());
            case NEQOP:
                return Builder.CreateZExtOrBitCast(Builder.CreateICmpNE(lhs, rhs, "netemp"),
                                                    Builder.getInt32Ty());
            case MOD:
                return Builder.CreateSRem(lhs, rhs, "modtemp");
            case LEFT_SHIFT:
                return Builder.CreateShl(lhs, rhs, "sltemp");
            case RIGHT_SHIFT:
                return Builder.CreateAShr(lhs, rhs, "srtemp");
            case BITAND:
                return Builder.CreateAnd(lhs, rhs, "andtemp");
            case BITXOR:
                return Builder.CreateXor(lhs, rhs, "xortemp");
            case BITOR:
                return Builder.CreateOr(lhs, rhs, "ortemp");

            // logical operators here, convert lhs/rhs value to 0 or 1 via comparison and then do bitand/bitor
            case LAND:
                convert_bool(lhs);
                convert_bool(rhs);
                return Builder.CreateAnd(lhs, rhs, "andtemp");
            case LOR:
                convert_bool(lhs);
                convert_bool(rhs);   
                return Builder.CreateOr(lhs, rhs, "ortemp");
            default:
                // binop not supported
                fprintf(stderr, "error: binary operator `%s` not supported on integers\n", binop_tostr(op));
                exit(1);
                return nullptr;
        }
    }
}

// identifier declaration node along with its type
llvm::Value *iddecl_codegen(struct _ast_node *root, llvm::Type *Ty) {
    assert(root->node_type == IDENTIFIER_DECL);
    ADD_IDNODE(root, NamedValues);  // NOTE: value added to symbol table varname is nullptr by def
    llvm::AllocaInst *Alloca = CreateVariableAlloca(Ty, root->children[0]->node_val);
    add_symbol(NamedValues, root->children[0]->node_val, Alloca);   // add_idnode needs to be succeeded by these two statements
    return Alloca;
}

// declarator : pointer direct_declarator | direct_declarator;
llvm::Value *declarator_codegen(struct _ast_node *declarator, llvm::Type *idtype) {
    assert(declarator->node_type == DECLARATOR);
    struct _ast_node *direct_declarator = declarator->children[0];    // identifier_decl or function_decl
    
    if (direct_declarator->node_type == IDENTIFIER_DECL)
        return iddecl_codegen(direct_declarator, idtype);
    else 
        // direct_declarator->node_type == FUNCTION_PTRDECL || FUNCTION_DECL
        // in either case just generate code for function declaration
        return function_decl_codegen(direct_declarator, idtype);
}

llvm::Value *preincdecop_codegen(struct _ast_node *unary_expr, enum NODE_TYPE op) {
    llvm::Value *expr_val = unaryexpr_codegen(unary_expr);
    switch (op) {
        case PREINC_OP: {
            llvm::Value *new_val = binop_codegen(expr_val, ONE_VAL, NODE_TYPE::PLUS);
            store_codegen(expr_val, new_val);
            return new_val;
        }
        case PREDEC_OP: {
            llvm::Value *new_val = binop_codegen(expr_val, ONE_VAL, NODE_TYPE::MINUS);
            store_codegen(expr_val, new_val);
            return new_val;
        }
    }
}

llvm::Value *unaryop_codegen(struct _ast_node *unaryexpr, enum NODE_TYPE op) {
    switch (op) {
        case ADDR_OP:
        case DEREF_OP:
            fprintf(stderr, "reference and de-reference operators are not supported\n");
            exit(1);
            return nullptr;
        case UNPLUS:
            return unaryexpr_codegen(unaryexpr);
        case UNMINUS:
            return binop_codegen(ZERO_VAL, unaryexpr_codegen(unaryexpr), NODE_TYPE::MINUS);
        case BIT_COMP:
            return binop_codegen(llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), "-1", 10)), 
                                unaryexpr_codegen(unaryexpr),
                                NODE_TYPE::BITXOR);
        case NOT:
            return binop_codegen(ZERO_VAL, unaryexpr_codegen(unaryexpr), NODE_TYPE::EQOP);
        default:
            fprintf(stderr, "error: unknown unary operator\n");
            exit(1);
            return nullptr;
    }
}

// inc/dec the passed in postfix_expr
llvm::Value *postincdecop_codegen(struct _ast_node *postfix_expr, enum NODE_TYPE op) {
    llvm::Value *expr_val = postfixexpr_codegen(postfix_expr);
    switch (op) {
        case POSTINC_OP: {
            llvm::Value *new_val = binop_codegen(expr_val, ONE_VAL, NODE_TYPE::PLUS);
            store_codegen(expr_val, new_val);
            return expr_val;
        }
        case POSTDEC_OP: {
            llvm::Value *new_val = binop_codegen(expr_val, ONE_VAL, NODE_TYPE::MINUS);
            store_codegen(expr_val, new_val);
            return expr_val;
        }
    }
}

llvm::Value *primaryexpr_codegen(struct _ast_node *primary_expr) {
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

llvm::Value *postfixexpr_codegen(struct _ast_node *postfix_expr) {
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

llvm::Value *unaryexpr_codegen(struct _ast_node *unary_expr) {
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
    
llvm::Value *store_codegen(llvm::Value *to, llvm::Value *from) {
    llvm::Value *toval = find_symbol(NamedValues, to->getName());
    cast_type(to, toval->getType());
    Builder.CreateStore(from, to);
    return from;
}

llvm::Value *expression_codegen(struct _ast_node *expression) {
    assert(expression->node_type == EXPRESSION);
    struct _ast_node **assign_expr;
    std::vector<llvm::Value *> vals;
    for(assign_expr = expression->children;*assign_expr!=NULL;++assign_expr) {
        vals.push_back(assignexpr_codegen(*assign_expr));
    }
    return vals.back(); // value of the last assign_expr is the value of the expression
}

// TODO : complete the implementation
llvm::Value *ternop_codegen(struct _ast_node *expr) {
    assert(expr->node_type == TERNOP);
    struct _ast_node *cond = expr->children[0];         // valexpr_codegen()
    struct _ast_node *true_expr = expr->children[1];    // expression_codegen()
    struct _ast_node *false_expr = expr->children[2];   // valexpr_codegen()

    llvm::Value *cond_val = valexpr_codegen(cond);
    
    // deadcode optimization
    if (auto *const_val = llvm::dyn_cast<llvm::Constant>(cond_val)) {
        if (const_val->isZeroValue()) 
            return valexpr_codegen(false_expr);
        else 
            return expression_codegen(true_expr);
    }

    // TODO : generate code for ternop (see code for if-else)
    // llvm::Value *trueexpr_val = expression_codegen(true_expr);  // may involve assignments or just valexpr
    // llvm::Value *falseexpr_val = valexpr_codegen(false_expr);
    cond_val = binop_codegen(cond_val, ZERO_VAL, NODE_TYPE::NEQOP);

    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(TheContext, "tern_then", CurrFunction);
    llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(TheContext, "tern_else");
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "tern_cont");
    Builder.CreateCondBr(cond_val, then_bb, else_bb);

    // emit code for then branch
    Builder.SetInsertPoint(then_bb);
    llvm::Value *then_val = expression_codegen(true_expr);
    assert(then_val);
    Builder.CreateBr(merge_bb);
    then_bb = Builder.GetInsertBlock();

    // emit code for else branch
    CurrFunction->getBasicBlockList().push_back(else_bb);
    Builder.SetInsertPoint(else_bb);
    llvm::Value *else_val = valexpr_codegen(false_expr);
    assert(else_val);
    Builder.CreateBr(merge_bb);
    else_bb = Builder.GetInsertBlock();

    // emit the merge block
    CurrFunction->getBasicBlockList().push_back(merge_bb);
    Builder.SetInsertPoint(merge_bb);
    make_common_type(then_val, else_val); // then and else expression must be able to cast into some common type

    llvm::PHINode *phi_node = Builder.CreatePHI(then_val->getType(), 2, "merge_phi");
    phi_node->addIncoming(then_val, then_bb);
    phi_node->addIncoming(else_val, else_bb);
    return phi_node;
}

// conditional_expression codegen
llvm::Value *valexpr_codegen(struct _ast_node *valexpr) {
    if (valexpr->node_type == TERNOP)
        return ternop_codegen(valexpr);
    else if (isbin_op(valexpr->node_type)) {
        return binop_codegen(valexpr->children[0], valexpr->children[1], valexpr->node_type);
    } else {
        // else unary_expression
        return unaryexpr_codegen(valexpr);
    }
}

llvm::Value *assignop_codegen(struct _ast_node *assign_expr) {
    struct _ast_node *unary_expr = assign_expr->children[0];
    struct _ast_node *assign_expr1 = assign_expr->children[1];
    llvm::Value *left = unaryexpr_codegen(unary_expr);
    llvm::Value *right = assignexpr_codegen(assign_expr1);
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
    }
    if (op!=ASSIGN)
        right = binop_codegen(left, right, op);
    return store_codegen(left, right);
}

llvm::Value *assignexpr_codegen(struct _ast_node *assign_expr) {
    bool isval_expr = !isassign_op(assign_expr->node_type);
    if (!isval_expr) {
        return assignop_codegen(assign_expr);
    } else {
        // conditional_expression codegen
        return valexpr_codegen(assign_expr);
    }
}

llvm::Value *initializer_codegen(struct _ast_node *initializer) {
    assert(initializer->node_type == INIT_ASSIGN_EXPR || initializer->node_type == INIT_LIST);
    if (initializer->node_type == INIT_LIST) {
        fprintf(stderr, "warning: initializer lists are not handled");
        return nullptr;
    } else {
        // intializer->node_type == INIT_ASSIGN_EXPR
        struct _ast_node *assign_expr = initializer->children[0];
        return assignexpr_codegen(assign_expr);
    }
}

std::vector<llvm::Value *> initdecl_list_codegen(struct _ast_node *init_decl_list, struct _ast_node *decl_spec) {
    std::vector<llvm::Value *> vals;
    if (!init_decl_list) {
        fprintf(stderr, "warning: useless type name in empty declaration");
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
            llvm::Type *idtype = get_type(decl_spec, ptr);
            llvm::Value *declarator_value = declarator_codegen(declarator, idtype);
            // direct_declarator->node_type == FUNCTION_PTRDECL || IDENTIFIER_DECL
            if (initializer) {
                llvm::Value *initializer_value = initializer_codegen(initializer);
                if (auto func = llvm::dyn_cast<llvm::Function>(declarator_value)) {
                    fprintf(stderr, "error: cannot initalize function declarations\n");
                    exit(1);
                } else if (auto globalvar = llvm::dyn_cast<llvm::GlobalVariable>(declarator_value)) {
                    if (auto constval = llvm::dyn_cast<llvm::Constant>(initializer_value)) {
                        globalvar->setInitializer(constval);
                        vals.push_back(declarator_value);
                    }
                    else {
                        fprintf(stderr, "error: initializer element is not constant");
                        exit(1);
                    }
                } else {
                    // local variable declaration
                    Builder.CreateStore(initializer_value, declarator_value);
                    vals.push_back(declarator_value);
                }
            }

        }
    }
    return vals;
}

llvm::Value *declaration_codegen(struct _ast_node *declaration) {
    assert(declaration->node_type == DECLARATION);
    struct _ast_node *decl_spec = declaration->children[0];
    struct _ast_node *init_decl_list = declaration->children[1];
    
    std::vector<llvm::Value *> vals = initdecl_list_codegen(init_decl_list, decl_spec);
    return nullptr; // doesn't really signify failure
}

llvm::Value *blkitem_codegen(struct _ast_node *root) {
    // root is child node if blkitem_list
    switch(root->node_type) {
        // declaration
        case DECLARATION:
            declaration_codegen(root);
            break;
        default:
            stmt_codegen(root);
            break;
        // statement
    }
}

// callee side code
llvm::Value *function_call_codegen(struct _ast_node *func_call) {
    assert(func_call->node_type == FUNCTION_CALL);
    llvm::Function *CalleeF = TheModule->getFunction(func_call->children[0]->node_val);
    if (!CalleeF) 
        return LogErrorV(func_call->node_val);

    // check for argument mismatch
    struct _ast_node *arg_list = func_call->children[1];   // arg_list->node_type = ARG_EXP_LIST
    int num_args = 0;
    if (arg_list) 
        for(num_args = 0;arg_list->children[num_args] != NULL;num_args++);

    if (num_args != CalleeF->arg_size()) {
        fprintf(stderr, "error: invalid number arguments passed to `%s`\n", func_call->children[0]->node_val);
        return nullptr;
    }
    
    std::vector<llvm::Value *> ArgsV(num_args, nullptr);
    for (unsigned int i = 0; i != num_args; ++i) {
        ArgsV[i] = assignexpr_codegen(arg_list->children[i]);  // arg_list->children[i] is a node for assignment_expression
        if (!ArgsV[i])
            return nullptr;
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
} 

// get return type of function_decl from DECL node (you cannot get it from function_decl)
llvm::Function *function_decl_codegen(struct _ast_node *root, llvm::Type *retty) {
    assert(root->node_type == FUNCTION_DECL);
    struct _ast_node *param_list = root->children[1];   // node->type == PARAM_LIST
    std::vector<llvm::Type *> argtypes;
    if (param_list) {
        // iterate over children of param_list (param_decl nodes), 
        //get their types from declaration specifiers, (first child of param_Decl node)
        struct _ast_node **param_decl;
        struct _ast_node *decl_spec;
        for(param_decl = param_list->children;*param_decl != NULL;param_decl++) {
            decl_spec = (*param_decl)->children[0]; // (*param_decl)->children[0] is DECL_SPEC node
            argtypes.push_back(get_type_decl_spec(decl_spec)); 
        }
    }
    llvm::FunctionType *FT = llvm::FunctionType::get(retty, argtypes, false); 
    // false parameter indicates that it is not a vararg

    llvm::Function* F = llvm::Function::Create(FT,
                                         llvm::Function::ExternalLinkage,
                                          root->children[0]->node_val, // root->children[0]->node_type == ID
                                          TheModule.get());
    // to continue:
    // set names for all the arguments from the argument vector (argv)
    std::vector<std::string> argv = getfun_argv(root);

    unsigned idx = 0;
    for(auto &Arg : F->args()) {
        // assert(argv[idx]);
        Arg.setName(argv[idx++]);
    }

    return F;
}

// caller side code
llvm::Function *function_def_codegen(struct _ast_node *root) {
    assert(root->node_type == FUNCTION_DEF);
    // first check for existing decl of function
    struct _ast_node *decl_spec = root->children[0];        // decl_spec->node_type == DECL_SPEC
    struct _ast_node *prototype = root->children[1];        // prototype->node_type == DECLARATOR
    struct _ast_node *retptr = prototype->children[1];      // retptr->node_type == PTR
    struct _ast_node *fun_decl = prototype->children[0];    // fun_decl->node_type == FUNCTION_DECL

    const char *fun_name = fun_decl->children[0]->node_val;
    llvm::Function *TheFunction = TheModule->getFunction(fun_name);
    llvm::Type *rettype = get_type(decl_spec, retptr);
    if (!TheFunction)
        TheFunction = function_decl_codegen(fun_decl, rettype); // prototype needs to be generated with named arguments
    if (!TheFunction) 
        return nullptr;
    if (!TheFunction->empty()) {
        fprintf(stderr, "log-error: function `%s` is defined more than once", fun_name);
    }

    // Create a new basic block to start insertion into
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    // Record the function arguments in the named-values map (push a new scope)
    enter_scope(NamedValues);
    enter_scope(LabelValues);   // pushing the first label_table
    assert(NotFoundLabels.empty());
    for(auto &Arg : TheFunction->args()) {
        // create an alloca instr for this variable
        llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getType(), Arg.getName());

        // store the initial value into the alloca
        Builder.CreateStore(&Arg, Alloca);

        // add argument to the symbol table
        add_symbol(NamedValues, Arg.getName(), Alloca);
    }

    struct _ast_node *cmpd_stmt = root->children[2]; // cmp_stmt->node_type == CMPND_STMT
    cmpdstmt_codegen(cmpd_stmt, true);
    llvm::BasicBlock *insert_blk = Builder.GetInsertBlock();

    // insert block should not be terminated
    assert(!insert_blk->getTerminator());

    if (insert_blk->empty() && insert_blk->use_empty()) {
        // empty basic block with no use
        TheFunction->eraseFromParent();
    } else if (!get_type_decl_spec(decl_spec)->isVoidTy()) {
        fprintf(stderr, "warning : No return statement in a non-void function\n");
        Builder.CreateRet(llvm::UndefValue::get(TheFunction->getReturnType())); // returns unspecified bit pattern
    } else {
        Builder.CreateRetVoid();
    }

    exit_scope(NamedValues);
    exit_scope(LabelValues);    // now LabelValues is empty
    LabelTable.clear();
    NotFoundLabels.clear();
    llvm::verifyFunction(*TheFunction);

    return TheFunction;
}


llvm::Value *constexpr_codegen(struct _ast_node *const_expr) {
    return valexpr_codegen(const_expr);
}

// ====== statements ======

llvm::BasicBlock *stmt_codegen(struct _ast_node *stmt) {
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
            fprintf(stderr, "unexpected node_type\n");
            break;
    }
}

// labeled statement

llvm::BasicBlock *labeledstmt_codegen(struct _ast_node *label_stmt) {
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

    return bb;
}

// case statement

llvm::BasicBlock *casestmt_codegen(struct _ast_node *case_stmt) {
    struct _ast_node *const_expr = case_stmt->children[0];
    struct _ast_node *stmt = case_stmt->children[1];

    llvm::Value *expr_val = constexpr_codegen(const_expr);
    auto const_val = llvm::dyn_cast<llvm::ConstantInt>(expr_val);
    
    if (!const_val) {
        fprintf(stderr, "case statement must be a constant integer\n");
        exit(1);
    }

    std::string casestmt_label = "case" + const_val->getValue().toString(10, true); // only signed integers are supported

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(TheContext, casestmt_label, CurrFunction);
    int ret = PUSH_LABEL_TOP(casestmt_label, LabelValues, bb);  // need to pop this
    if (ret == PUSH_LABEL_TOP_FAIL) {
        fprintf(stderr, "duplicate case value `%s`", const_val->getValue().toString(10, true));
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
    POP_LABEL_BLOCK(LabelValues);
    return bb;
}

// default_stmt
llvm::BasicBlock *defstmt_codegen(struct _ast_node *defstmt) {
    struct _ast_node *stmt = defstmt->children[0];
    
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(TheContext, "default", CurrFunction);
    int ret = PUSH_LABEL_TOP("default", LabelValues, bb);   // need to pop this
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
    POP_LABEL_BLOCK(LabelValues);
    return bb;
}

// compound_statement

void cmpdstmt_codegen(struct _ast_node *root, bool is_fundef) {
    assert(root->node_type == CMPND_STMT);
    struct _ast_node *blk_item_list = root->children[0];
    if (!is_fundef) {
        enter_scope(NamedValues);
        enter_scope(LabelValues);
    }
    if (blk_item_list) {
        struct _ast_node **blkitem;    // (*blk_item)->node_type == DECL or statement(lots of them)
        for(blkitem = blk_item_list->children;*blkitem!=NULL;blkitem++) {
            blkitem_codegen(*blkitem);
        }
    }
    if (!is_fundef) {
        exit_scope(NamedValues);
        exit_scope(LabelValues);
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
            ifelsestmt_codegen(select_stmt);
        case SWITCH_STMT:
            switchstmt_codegen(select_stmt);
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

    llvm::Value *cond_val = expression_codegen(cond);
    assert(cond_val && "cannot compute expression");

    // constant folding optimization
    auto const_cond_val = llvm::dyn_cast<llvm::Constant>(cond_val);
    if (const_cond_val) {
        if (const_cond_val->isZeroValue()) {
            if (!elsestmt) return;
            else
                stmt_codegen(elsestmt);
        } else {
            assert(thenstmt && "then statement must not be null");
            stmt_codegen(thenstmt);
        }
        return;
    }

    cond_val = binop_codegen(cond_val, ZERO_VAL, NODE_TYPE::NEQOP);

    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(TheContext, "then", CurrFunction);
    llvm::BasicBlock *else_bb;
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "ifcont");

    if (elsestmt)
        else_bb = llvm::BasicBlock::Create(TheContext, "else");
    else
        else_bb = merge_bb;

    Builder.CreateCondBr(cond_val, then_bb, else_bb);
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

// switch statement
void switchstmt_codegen(struct _ast_node *switch_stmt) {
    assert(switch_stmt->node_type == NODE_TYPE::SWITCH_STMT);
    struct _ast_node *switch_expr = switch_stmt->children[0];   // expression (constant-expression)
    struct _ast_node *switch_stmt = switch_stmt->children[1];   // statement

    llvm::Value *switchexpr_val = expression_codegen(switch_expr);
    auto switch_end = llvm::BasicBlock::Create(TheContext, "switch_end");
    Builder.CreateSwitch(switchexpr_val, switch_end);

    auto switch_new = llvm::BasicBlock::Create(TheContext, "switch_new", CurrFunction);

    Builder.SetInsertPoint(switch_new);
    // TODO: finish this code generation
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
    llvm::Value *cond_val = expression_codegen(cond);

    // constant folding optimization
    auto const_cond_val = llvm::dyn_cast<llvm::Constant>(cond_val);
    if (const_cond_val) {
        if (const_cond_val->isZeroValue()) {
            br_cond->eraseFromParent();
            Builder.SetInsertPoint(cond_bb->getPrevNode());
            cond_bb->eraseFromParent();
            return;
        }
    }

    cond_val = binop_codegen(cond_val, ZERO_VAL, NODE_TYPE::NEQOP);
    llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(TheContext, "while_body", CurrFunction);
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(TheContext, "while_cont");

    Builder.CreateCondBr(cond_val, body_bb, merge_bb);
    
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

    llvm::Value *cond_val = expression_codegen(cond);
    cond_val = binop_codegen(cond_val, ZERO_VAL, NODE_TYPE::NEQOP);
    Builder.CreateCondBr(cond_val, body_bb, merge_bb);

    // emit code for merge block
    CurrFunction->getBasicBlockList().push_back(merge_bb);
    Builder.SetInsertPoint(merge_bb);
}

void forstmt_codegen(struct _ast_node *for_stmt) {
    fprintf(stderr, "error: for-loops are not supported\n");
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
        fprintf(stderr, "`return` with a value, in function returning void");
        exit(1);
    } else if (!retty->isVoidTy() && !ret_expr) {
        fprintf(stderr, "`return` with no value, in function returning non-void");
        exit(1);
    }

    if (ret_expr) {
        // RETURN_STMT2
        llvm::Value *retval = expression_codegen(ret_expr);
        cast_type(retval, retty);
        Builder.CreateRet(retval);
    } else {
        // RETURN_STMT1
        Builder.CreateRetVoid();
    }

    llvm::BasicBlock *retend_bb = llvm::BasicBlock::Create(TheContext, "ret_end", CurrFunction);
    Builder.SetInsertPoint(retend_bb);
}

// translation-unit

void translationunit_codegen(struct _ast_node *trans_unit) {
    assert(trans_unit->node_type== NODE_TYPE::TRANSLATION_UNIT);
    struct _ast_node **external_declaration;
    for(external_declaration = trans_unit->children;*external_declaration!=NULL;external_declaration++) {
        externaldecl_codegen(*external_declaration);
    }
}

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

void codegen(struct _ast_node *root) {
    translationunit_codegen(root);
}
