#ifndef __DECLARATION_CODEGEN_H
#define __DECLARATION_CODEGEN_H
#include "ast.hpp"

void declaration_codegen(struct _ast_node *declaration);
void function_def_codegen(struct _ast_node *root);
void translationunit_codegen(struct _ast_node *trans_unit);
void externaldecl_codegen(struct _ast_node *extern_decl);

#endif