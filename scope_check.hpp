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
typedef std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> symbol_table;    // stack of scopes
typedef std::unordered_map<std::string, llvm::Value*> label_table;

#define ADD_IDNODE(root, sym_table) {assert(root->children[0]->node_type == ID);struct _ast_node *id_node = root->children[0]; \
                                    if (check_scope(sym_table, id_node->node_val)) { \
                                        fprintf(stderr, "error: double declaration of variable/function `%s`\n", id_node->node_val);exit(1); \
                                    } else add_symbol(sym_table, id_node->node_val, nullptr);}

void enter_scope(symbol_table& sym_table);
llvm::AllocaInst* find_symbol(const symbol_table& sym_table, const std::string& symbol);
bool find_symbol_bool(const symbol_table& sym_table, const std::string& symbol);
void add_symbol(symbol_table& sym_table, const std::string& symbol, llvm::AllocaInst* alloca);
bool check_scope(const symbol_table& sym_table, const std::string& symbol);
void exit_scope(symbol_table& sym_table);