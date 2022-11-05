#ifndef __DECLARATION_CODEGEN_H
#define __DECLARATION_CODEGEN_H
#include "ast.hpp"

value_llvm declarator_codegen(struct _ast_node *declarator, type_llvm idtype);
value_llvm initializer_codegen(struct _ast_node *initializer);
std::vector<value_llvm> initdecl_list_codegen(struct _ast_node *init_decl_list, struct _ast_node *decl_spec);
function_llvm function_decl_codegen(struct _ast_node *fun_decl, type_llvm retty);


#endif