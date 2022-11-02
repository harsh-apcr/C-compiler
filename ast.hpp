#ifndef _AST_H__
#define _AST_H__

#include "llvm/IR/Value.h"

enum NODE_TYPE {
    ID, I_CONST, F_CONST, STRING, 
    ARRAY_ACCESS,FUNCTION_CALL,POSTINC_OP,POSTDEC_OP,
    ARG_EXPR_LIST, INIT_ASSIGN_EXPR, INIT_LIST, DECLARATOR,
    PREINC_OP,PREDEC_OP,ADDR_OP,DEREF_OP,UNPLUS,UNMINUS,BIT_COMP,NOT,
    MULT,DIV,MOD,PLUS,MINUS,LEFT_SHIFT,RIGHT_SHIFT,LESS_THAN,GREATER_THAN,
    LESSEQ,GREATEREQ,EQOP,NEQOP,BITAND, BITXOR, BITOR,LAND, LOR, TERNOP,
    ASSIGN, MULASSIGN, DIVASSIGN, MODASSIGN, ADDASSIGN, SUBASSIGN,LEFTASSIGN,
    RIGHTASSIGN, BITANDASSIGN, BITXORASSIGN, BITORASSIGN, EXPRESSION, DECLARATION,
    DECLARATION_SPEC, INIT_DECL_LIST, INIT_DECL, TYPE_SIGNED, TYPE_UNSIGNED, TYPE_BOOL, 
    TYPE_VOID, TYPE_CHAR, TYPE_SHORT, TYPE_INT, TYPE_LONG, TYPE_FLOAT, TYPE_DOUBLE, RETURN_STMT2,
    TYPE_QUAL,IDENTIFIER_DECL, FUNCTION_DECL,PTR,TYPE_QUAL_LIST, ELLIPSIS_NODE, RETURN_STMT1,
    PARAM_LIST, PARAM_DECL, LABELED_STMT, CASE_STMT, DEF_STMT, CMPND_STMT, BLK_ITEM_LIST,
    EXPR_STMT,TRANSLATION_UNIT, FUNCTION_DEF, IF_ELSE_STMT, IF_STMT, SWITCH_STMT, BREAK_STMT,
    WHILE_STMT, DO_WHILE_STMT, FOR_STMT1, FOR_STMT2, FOR_STMT3, FOR_STMT4, GOTO_STMT, CONTINUE_STMT, FUNCTION_PTRDECL
};

struct _ast_node {
    enum NODE_TYPE node_type;
    char *node_val;


    struct _ast_node** children;     // children node ptrs are placed contiguously in memory and this list is NULL terminated 
    int children_capacity;           // capacity of children array
    int children_size;               // size of the children array (excluding the null pointer)
};

struct _ast_node* ast_node_create(enum NODE_TYPE node_type, const char *node_val, struct _ast_node** children);


void ast_destroy(struct _ast_node* root);
void dump_ast(struct _ast_node* root);
void add_children(struct _ast_node *root, struct _ast_node* child);
void scope_checking(struct _ast_node *root);
llvm::Value *codegen(struct _ast_node* root); 

#endif  // _AST_H__