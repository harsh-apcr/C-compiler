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

std::string type_to_str(const llvm::Type *Ty); // need to complete this properly

llvm::Type *get_type_decl_spec(struct _ast_node *node) {
    assert(node->node_type == DECLARATION_SPEC);
    // get type from decl spec node
}

llvm::Value *LogErrorV(const char *name) {
    fprintf(stderr, "log-error : unknown variable named `%s`", name);
    return nullptr;
}

// llvm::Value *LogErrorB(const char *op, const char *ty1, const char *ty2) {
//     fprintf(stderr, "log-error : invalid operands to binary %s (have `%s` and `%s`)", op, ty1, ty2);
//     return nullptr;
// }

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

std::string optostr(enum NODE_TYPE op);

// ======== HELPER FUNCTIONS ========



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
                fprintf(stderr, "error: binary operator `%s` not supported on floats\n", optostr(op));
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
                fprintf(stderr, "error: binary operator `%s` not supported on integers\n", optostr(op));
                exit(1);
                return nullptr;
        }
    }
}

// identifier declaration node along with its type
llvm::Value *iddecl_codegen(struct _ast_node *root, llvm::Type *Ty) {
    assert(root->node_type == IDENTIFIER_DECL);
    ADD_IDNODE(root, NamedValues);
    llvm::AllocaInst *Alloca = CreateVariableAlloca(Ty, root->children[0]->node_val);
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
            fprintf(stderr, "error: unary operator is unknown\n");
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

llvm::Value *string_codegen(struct _ast_node *string_node) {
    assert(string_node->node_type == NODE_TYPE::STRING);
    // TODO: complete string codegen
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
    
    // constant folding optimization
    if (auto *const_val = llvm::dyn_cast<llvm::Constant>(cond_val)) {
        if (const_val->isZeroValue()) 
            return valexpr_codegen(false_expr);
        else 
            return expression_codegen(true_expr);
    }

    // TODO : generate code for ternop (see code for if-else)
    // llvm::Value *trueexpr_val = expression_codegen(true_expr);  // may involve assignments or just valexpr
    // llvm::Value *falseexpr_val = valexpr_codegen(false_expr);

}

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
        // valexpr
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
        case DECLARATION: {
            declaration_codegen(root);
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


