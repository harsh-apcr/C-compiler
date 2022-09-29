#ifndef _AST_H__
#define _AST_H__

enum NODE_TYPE {
    IDENTIFIER,
    INT_CONST,FLOAT_CONST,
    ARRAY_ACCESS,FUNCTION_CALL,POSTINC_OP,POSTDEC_OP,
    ARG_EXPR_LIST,
    PREINC_OP,PREDEC_OP,ADDR_OP,DEREF_OP,UNPLUS,UNMINUS,BIT_COMP,NOT,
    MULT,DIV,MOD,PLUS,MINUS,LEFT_SHIFT,RIGHT_SHIFT,LESS_THAN,GREATER_THAN,LESSEQ,GREATEREQ,EQOP,NEQOP,BITAND, BITXOR, BITOR,
    LAND, LOR, TERNOP,
    ASSIGN, MULASSIGN, DIVASSIGN, MODASSIGN, ADDASSIGN, SUBASSIGN,LEFTASSIGN, RIGHTASSIGN, BITANDASSIGN, BITXORASSIGN, BITORASSIGN,
    ASSIGN_COMMA, DECL, 
    DECLSPEC, INIT_DECL_LIST, INIT_DECL,
    TYPE_VOID, TYPE_CHAR, TYPE_SHORT, TYPE_INT, TYPE_LONG, TYPE_FLOAT, TYPE_DOUBLE, TYPE_SIGNED, TYPE_UNSIGNED, TYPE_BOOL,
    TYPE_QUAL_CONST,
    IDENTIFIER_DECL, FUNCTION_DECL,
    PTR,
    TYPE_QUAL_LIST,
    ELLIPSIS,
    PARAM_LIST, PARAM_DECL, 
    LABELED_STMT, CASE_STMT, DEF_STMT, CMPND_STMT, 
    BLK_ITEM_LIST, EXPR_STMT,
    TRANSLATION_UNIT, FUNCTION_DEF
};

struct _ast_node {
    enum NODE_TYPE node_type;
    char *node_val;
    struct _ast_node** children;     // children node ptrs are placed contiguously in memory and this list is NULL terminated 
    int children_capacity;
    int children_size;
};

struct _ast_node* ast_node_create(enum NODE_TYPE node_type, char *node_val, struct _ast_node* children[]);


void ast_destroy(struct _ast_node* root);
void dump_ast(struct _ast_node* root);
void add_children(struct _ast_node *root, struct _ast_node* child);



#endif  // _AST_H__