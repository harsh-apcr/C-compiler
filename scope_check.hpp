#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <vector>
#include <unordered_map>
#include <string>
#include "ast.hpp"
#include <cassert>
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
typedef std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> symbol_table;    // stack of scopes
typedef std::unordered_map<std::string, llvm::BasicBlock*> label_table;
typedef std::vector<label_table> label_stack;

#define ADD_IDNODE(root, sym_table) {assert(root->children[0]->node_type == ID);struct _ast_node *id_node = root->children[0]; \
                                    if (check_scope(sym_table, id_node->node_val)) { \
                                        fprintf(stderr, "error: double declaration of variable/function `%s`\n", id_node->node_val);exit(1); \
                                    } else add_symbol(sym_table, id_node->node_val, nullptr);}

#define ADD_LABEL(name, label_table, bb) { if (check_label(label_table, name)) { \
                                            fprintf(stderr, "label `%s` is defined more than once\n", name);exit(1); \
                                        } else add_label(label_table, name, bb);}

#define PUSH_LABEL(name, label_stack, bb) { if(find_label(label_stack, name)) { \
                                            fprintf(stderr, "label `%s` is defined more than once\n", name);exit(1); \
                                        } else push_label(label_stack, name, bb);}   

#define PUSH_LABEL_TOP_SUCCESS 0
#define PUSH_LABEL_TOP_FAIL 1

inline int PUSH_LABEL_TOP(const std::string &name,label_stack &label_stack, llvm::BasicBlock *bb) {
    if (find_label_top(label_stack, name)) return PUSH_LABEL_TOP_FAIL;
    else {
        push_label(label_stack, name, bb);
        return PUSH_LABEL_TOP_SUCCESS;
    }
}

// symbol-table prototypes
void enter_scope(symbol_table& sym_table);
llvm::AllocaInst* find_symbol(const symbol_table& sym_table, const std::string& symbol);
bool find_symbol_bool(const symbol_table& sym_table, const std::string& symbol);
void add_symbol(symbol_table& sym_table, const std::string& symbol, llvm::AllocaInst* alloca);
bool check_scope(const symbol_table& sym_table, const std::string& symbol);
void exit_scope(symbol_table& sym_table);

// label-table prototypes
bool check_label(const label_table &label_table, const std::string &symbol);
void add_label(label_table &label_table, const std::string& symbol, llvm::BasicBlock *bb);

// label-stack prototypes
inline void enter_scope(label_stack& label_stack); // while entering compound_stmt
inline llvm::BasicBlock *find_label_top(const label_stack &label_stack, const std::string &label_name);
llvm::BasicBlock *find_label(const label_stack &label_stack, const std::string &label_name);
inline void push_label(label_stack &label_stack, const std::string &symbol, llvm::BasicBlock *bb); // push label on top of the stack
inline void exit_scope(label_stack &label_stack);  // while exiting compound_stmt
