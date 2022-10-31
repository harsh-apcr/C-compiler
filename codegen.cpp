#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <vector>
#include <unordered_map>
#include <string>
#include <cassert>
#include "ast.hpp"

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

#define zero_val llvm::ConstantInt::get(TheContext, llvm::APInt(sizeof(int), "0", 10))
// zero_val is llvm::Value*

static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;
static std::unordered_map<std::string, llvm::AllocaInst*> NamedValues;

// static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
//                                                 const llvm::Twine &VarName,
//                                                 const llvm::Type *Ty) {
//   llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
//                  TheFunction->getEntryBlock().begin());
//   return TmpB.CreateAlloca(Ty, nullptr, VarName);
// }

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
    llvm::Value *v = NamedValues[root->node_val];
    if (!v) LogErrorV(root->node_val);
    return Builder.CreateLoad(v, root->node_val);
}

// need to complete this properly
std::string type_to_str(const llvm::Type *Ty);

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
        fprintf(stderr, "log-error : invalid number arguments passed");
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

llvm::Function *function_decl_codegen(struct _ast_node *root) {
    assert(root->node_type == FUNCTION_DECL);
    
}

/*
Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}
*/








llvm::Value *id_decl_codegen(struct _ast_node *root);



