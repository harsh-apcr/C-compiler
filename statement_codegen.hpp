#ifndef __STATEMENT_CODEGEN_
#define __STATEMENT_CODEGEN_
#include "ast.hpp"

void stmt_codegen(struct _ast_node *stmt);

void labeledstmt_codegen(struct _ast_node *label_stmt);
void casestmt_codegen(struct _ast_node *case_stmt);
llvm::BasicBlock *defstmt_codegen(struct _ast_node *defstmt);

void cmpdstmt_codegen(struct _ast_node *root, bool is_fundef);
void expressionstmt_codegen(struct _ast_node *expr_stmt);

void selectstmt_codegen(struct _ast_node *select_stmt);
void ifelsestmt_codegen(struct _ast_node *ifelse_stmt);
void switchstmt_codegen(struct _ast_node *switch_stmt);

void iterationstmt_codegen(struct _ast_node *it_stmt);
void whilestmt_codegen(struct _ast_node *while_stmt);
void dowhilestmt_codegen(struct _ast_node *dowhile_stmt);
void forstmt_codegen(struct _ast_node *for_stmt);

void jumpstmt_codegen(struct _ast_node *jmp_stmt);
void goto_codegen(struct _ast_node *goto_stmt);
void continue_codegen(struct _ast_node *cont_stmt);
void break_codegen(struct _ast_node *break_stmt);
void return_codegen(struct _ast_node *ret_stmt);

#endif
