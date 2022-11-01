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


#define zero_val llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), "0", 10))
// zero_val is llvm::Value*

static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;
static symbol_table NamedValues;

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
    assert(decl_spec->node_type == DECLSPEC);
    assert(ptr->node_type == PTR);
}

// need to complete this properly
std::string type_to_str(const llvm::Type *Ty);

llvm::Type *get_type_decl_spec(struct _ast_node *node) {
    assert(node->node_type == DECLSPEC);
    // get type from decl spec node
}

llvm::Value *LogErrorV(const char *name) {
    fprintf(stderr, "log-error : unknown variable named `%s`", name);
    return nullptr;
}

llvm::Value *LogErrorB(const char *op, const char *ty1, const char *ty2) {
    fprintf(stderr, "log-error : invalid operands to binary %s (have `%s` and `%s`)", op, ty1, ty2);
    return nullptr;
}

llvm::Value *LogErrorF(const char *name) {
    fprintf(stderr, "log-error : unknown function named `%s`", name);
    return nullptr;
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
    val = Builder.CreateICmpEQ(val, zero_val);
    val = Builder.CreateZExtOrBitCast(val, llvm::IntegerType::get(TheContext, sizeof(int)));
}

// also look for llvm::StringLiteral
llvm::Value *constant_codegen(struct _ast_node *root) {
    assert(root->node_type == I_CONST 
        || root->node_type == F_CONST);
    
    if (root->node_type == I_CONST) {
        return llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), root->node_val, 10));
    }
    if (root->node_type == F_CONST) {
        return llvm::ConstantFP::get(TheContext, llvm::APFloat(atof(root->node_val)));
    }
}

// id is on stack
llvm::Value *id_codegen(struct _ast_node *root) {
    assert(root->node_type == ID);
    llvm::Value *v = find_symbol(NamedValues, root->node_val);
    if (!v) LogErrorV(root->node_val);
    return Builder.CreateLoad(v, root->node_val);
}

llvm::Value *binop_codegen(struct _ast_node *root) {
    assert(root->children[0]);assert(root->children[1]);assert(!root->children[2]);
    llvm::Value *lhs = codegen(root->children[0]);
    llvm::Value *rhs = codegen(root->children[1]);
    if (!lhs || !rhs) return nullptr;
    
    llvm::Value *cmp_ret;
    if (!lhs->getType()->isIntegerTy() || !rhs->getType()->isIntegerTy()) {
        // lhs or rhs is a float type
        switch(root->node_type) {
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
                LogErrorB(root->node_val, type_to_str(lhs->getType()).c_str(), type_to_str(rhs->getType()).c_str());
                return nullptr;
        }
        if (!cmp_ret) 
            return nullptr;
        return Builder.CreateZExtOrBitCast(cmp_ret, Builder.getInt32Ty());
    } else {
        // both lhs and rhs are int type
        // before doing any operation on them, need to convert them into same type for any llvm-opcode
        make_common_type(lhs, rhs);
        // currently all operations are `signed int` operations
        // if you want to make it unsigned then you'd need to pass a symbol table for types
        switch(root->node_type) {
            case MULT:
                return Builder.CreateMul(lhs, rhs, "multmp");
            case DIV:
                return Builder.CreateSDiv(lhs, rhs, "divtmp");
            case PLUS:
                return Builder.CreateAdd(lhs, rhs, "addtmp");
            case MINUS:
                return Builder.CreateSub(lhs, rhs, "subtmp");
            case LESS_THAN:
                return Builder.CreateICmpSLT(lhs, rhs, "lttemp");
            case GREATER_THAN:
                return Builder.CreateICmpSGT(lhs, rhs, "gttemp");
            case LESSEQ:
                return Builder.CreateICmpSLE(lhs, rhs, "letemp");
            case GREATEREQ:
                return Builder.CreateICmpSGE(lhs, rhs, "getemp");
            case EQOP:
                return Builder.CreateICmpEQ(lhs, rhs, "eqtemp");
            case NEQOP:
                return Builder.CreateICmpNE(lhs, rhs, "netemp");
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
                LogErrorB(root->node_val, type_to_str(lhs->getType()).c_str(), type_to_str(rhs->getType()).c_str());
                return nullptr;
        }
    }
}

llvm::Value *blkitem_codegen(struct _ast_node *root) {
    // root is child node if blkitem_list
    switch(root->node_type) {
        // declaration
        case DECL: {
            struct _ast_node *decl_spec = root->children[0];
            struct _ast_node *init_decl_list = root->children[1];
            if (!init_decl_list) {
                fprintf(stderr, "warning: useless type name in empty declaration");
            } else {
                struct _ast_node **init_decl;
                struct _ast_node *declarator;
                struct _ast_node *initializer;
                struct _ast_node *direct_declarator;
                struct _ast_node *ptr;
                for(init_decl = init_decl_list->children;*init_decl!=NULL;init_decl++) {
                    // each child of *init_decl is either declarator or initializer
                    declarator = (*init_decl)->children[0];
                    initializer = (*init_decl)->children[1];

                    direct_declarator = declarator->children[0];    // identifier_decl or function_decl
                    // cannot be a function declarator inside a function itself
                    if (direct_declarator->node_type == FUNCTION_DECL) {
                        fprintf(stderr, "error: function cannot be declared inside any '{' '}'");
                        return nullptr;
                    }
                    // direct_declarator->node_type == IDENTIFIER_DECL

                    ptr = declarator->children[1];  // could be null
                    
                    if (!initializer) {
                        // initializer is null
                    } else {
                        // initializer in not null
                    }
                }
            }
            break;
        }
        // statement
        case LABELED_STMT:
        case CMPND_STMT:
        case EXPR_STMT:
        case IF_ELSE_STMT:
        case IF_STMT:
        case SWITCH_STMT:
        case WHILE_STMT:
        case DO_WHILE_STMT:
        case FOR_STMT1:
        case FOR_STMT2:
        case FOR_STMT3:
        case FOR_STMT4:
        case GOTO_STMT:
        case CONTINUE_STMT:
        case BREAK_STMT:
        case RETURN_STMT1:
        case RETURN_STMT2:
        default:
            break;
    }
}

llvm::Value *cmpd_stmt_codegen(struct _ast_node *root) {
    assert(root->node_type == CMPND_STMT);
    struct _ast_node *blk_item_list = root->children[0];
    enter_scope(NamedValues);   // problem with enter_scope inside function_def_codegen
    if (blk_item_list) {
        struct _ast_node **blkitem;    // (*blk_item)->node_type == DECL or statement(lots of them)
        for(blkitem = blk_item_list->children;*blkitem!=NULL;blkitem++) {
            blkitem_codegen(*blkitem);
        }
    }
    exit_scope(NamedValues);
    return nullptr; // doesn't really have a significance
}


// identifier declaration node along with its type
llvm::Value *id_decl_codegen(struct _ast_node *root, llvm::Type *Ty) {
    assert(root->node_type == IDENTIFIER_DECL);
    ADD_IDNODE(root, NamedValues);
    llvm::AllocaInst *Alloca = CreateVariableAlloca(Ty, root->children[0]->node_val);
    return Alloca;
}

// callee side code
llvm::Value *function_call_codegen(struct _ast_node *root) {
    assert(root->node_type == FUNCTION_CALL);
    llvm::Function *CalleeF = TheModule->getFunction(root->node_val);
    if (!CalleeF) return LogErrorV(root->node_val);

    // check for argument mismatch
    struct _ast_node *arg_list = root->children[1];   // node->type = ARG_EXP_LIST
    int num_args = 0;
    if (arg_list) 
        for(num_args = 0;arg_list->children[num_args] != NULL;num_args++);

    if (num_args != CalleeF->arg_size()) {
        fprintf(stderr, "log-error: invalid number arguments passed");
        return nullptr;
    }
    
    std::vector<llvm::Value *> ArgsV(num_args, nullptr);
    for (unsigned int i = 0; i != num_args; ++i) {
        ArgsV[i] = codegen(arg_list->children[i]);
        if (!ArgsV[i])
            return nullptr;
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
} 

// get return type of function_decl from DECL node (you cannot get it from function_decl)
llvm::Function *function_decl_codegen(struct _ast_node *root, llvm::Type *retty, std::vector<std::string> &argv) {
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
    const char *fun_name = fun_decl->children[0]->node_val;
    llvm::Function *TheFunction = TheModule->getFunction(fun_name);
    llvm::Type *rettype = get_type(decl_spec, retptr);
    if (!TheFunction)
        TheFunction = function_decl_codegen(fun_decl, rettype, argv); // prototype needs to be generated with named arguments
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
    for(auto &Arg : TheFunction->args()) {
        // create an alloca instr for this variable
        llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getType(), Arg.getName());

        // store the initial value into the alloca
        Builder.CreateStore(&Arg, Alloca);

        // add argument to the symbol table
        add_symbol(NamedValues, Arg.getName(), Alloca);
    }

    struct _ast_node *cmpd_stmt = root->children[2]; // cmp_stmt->node_type == CMPND_STMT
    cmpd_stmt_codegen(cmpd_stmt);
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
    llvm::verifyFunction(*TheFunction);

    return TheFunction;
}


