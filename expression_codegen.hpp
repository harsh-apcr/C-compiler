#ifndef __EXPRESSION_CODEGEN_H
#define __EXPRESSION_CODEGEN_H
#include "ast.hpp"

// declarations of all the usefull prototypes

value_llvm constant_codegen(struct _ast_node *constant);

value_llvm string_codegen(struct _ast_node *string_node);

value_llvm id_codegen(struct _ast_node *id);

value_llvm binop_codegen(value_llvm &lhs, value_llvm &rhs, enum NODE_TYPE op, bool i1toi32);
value_llvm binop_codegen(struct _ast_node *left, struct _ast_node *right, enum NODE_TYPE op);

value_llvm preincdecop_codegen(struct _ast_node *unary_expr, enum NODE_TYPE op);
value_llvm unaryop_codegen(struct _ast_node *unaryexpr, enum NODE_TYPE op);
value_llvm postincdecop_codegen(struct _ast_node *postfix_expr, enum NODE_TYPE op);

value_llvm primaryexpr_codegen(struct _ast_node *primary_expr);
value_llvm postfixexpr_codegen(struct _ast_node *postfix_expr);
value_llvm unaryexpr_codegen(struct _ast_node *unary_expr);

value_llvm expression_codegen(struct _ast_node *expression);
value_llvm ternop_codegen(struct _ast_node *expr);
value_llvm valexpr_codegen(struct _ast_node *valexpr);
value_llvm assignop_codegen(struct _ast_node *assign_expr);
value_llvm assignexpr_codegen(struct _ast_node *assign_expr);

value_llvm function_call_codegen(struct _ast_node *func_call);
value_llvm constexpr_codegen(struct _ast_node *const_expr);

#endif