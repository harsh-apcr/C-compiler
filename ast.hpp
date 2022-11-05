#ifndef _AST_H__
#define _AST_H__

#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"

enum NODE_TYPE {
    ID, I_CONST, F_CONST, STRING, TYPE_QUAL_CONST, TYPE_QUAL_RESTRICT, TYPE_SPEC_UCHAR, TYPE_SPEC_SCHAR, 
    ARRAY_ACCESS,FUNCTION_CALL,POSTINC_OP,POSTDEC_OP, TYPE_QUAL_VOLATILE, TYPE_SPEC_ULONGLONG, TYPE_SPEC_SSHORT,
    ARG_EXPR_LIST, INIT_ASSIGN_EXPR, INIT_LIST, DECLARATOR, TYPE_QUAL_ATOMIC, TYPE_SPEC_LONGLONG,
    PREINC_OP,PREDEC_OP,ADDR_OP,DEREF_OP,UNPLUS,UNMINUS,BIT_COMP,NOT, TYPE_SPEC_USHORT, TYPE_SPEC_SINT,
    MULT,DIV,MOD,PLUS,MINUS,LEFT_SHIFT,RIGHT_SHIFT,LESS_THAN,GREATER_THAN, TYPE_SPEC_ULONG, TYPE_SPEC_SHORTINT,
    LESSEQ,GREATEREQ,EQOP,NEQOP,BITAND, BITXOR, BITOR,LAND, LOR, TERNOP, TYPE_SPEC_DOUBLE, TYPE_SPEC_UINT,
    ASSIGN, MULASSIGN, DIVASSIGN, MODASSIGN, ADDASSIGN, SUBASSIGN,LEFTASSIGN, TYPE_SPEC_EMPTY, TYPE_SPEC_INTLONG,
    RIGHTASSIGN, BITANDASSIGN, BITXORASSIGN, BITORASSIGN, EXPRESSION, DECLARATION,  RETURN_STMT2, TYPE_SPEC_SLONG,
    DECLARATION_SPEC, INIT_DECL_LIST, INIT_DECL, TYPE_SPEC_SIGNED, TYPE_SPEC_UNSIGNED, TYPE_SPEC_BOOL, 
    TYPE_SPEC_VOID, TYPE_SPEC_CHAR, TYPE_SPEC_SHORT, TYPE_SPEC_INT, TYPE_SPEC_LONG, TYPE_SPEC_FLOAT, TYPE_SPECIFIER,
    IDENTIFIER_DECL, FUNCTION_DECL,PTR,TYPE_QUAL_LIST, ELLIPSIS_NODE, RETURN_STMT1, FUNCTION_PTRDECL, 
    PARAM_LIST, PARAM_DECL, LABELED_STMT, CASE_STMT, DEF_STMT, CMPND_STMT, BLK_ITEM_LIST, TYPE_SPEC_LONGDOUBLE,
    EXPR_STMT,TRANSLATION_UNIT, FUNCTION_DEF, IF_ELSE_STMT, IF_STMT, SWITCH_STMT, BREAK_STMT, TYPE_SPEC_INTLONGLONG,
    WHILE_STMT, DO_WHILE_STMT, FOR_STMT1, FOR_STMT2, FOR_STMT3, FOR_STMT4, GOTO_STMT, CONTINUE_STMT
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
void codegen(struct _ast_node* root); 
void print_module(struct _ast_node* root, std::string &out_filename);

class type_llvm {
    public:
    llvm::Type *Ty;
    bool is_signed;
    type_llvm(llvm::Type *Ty=nullptr, bool is_signed=true) : Ty(Ty), is_signed(is_signed) {}
};

class value_llvm {
public:
    llvm::Value *val;
    type_llvm ty;
    value_llvm(llvm::Value *val=nullptr, type_llvm ty=type_llvm()) : val(val), ty(ty) {}
};

class function_llvm : public value_llvm {
public:
    std::vector<type_llvm> param_tylist;
    function_llvm(llvm::Function *func, std::vector<type_llvm> &param_tylist, type_llvm retty) 
        : value_llvm(func, retty), param_tylist(param_tylist) {}
    function_llvm(llvm::Function *func, std::vector<type_llvm> &&param_tylist, type_llvm retty) 
        : value_llvm(func, retty), param_tylist(param_tylist) {}
    function_llvm() : value_llvm(nullptr, type_llvm()), param_tylist() {}
};

void add_typespec(struct _ast_node *decl_spec, struct _ast_node *new_typespec);
void add_typequal(struct _ast_node *typequal_list, struct _ast_node *typequal);
std::string get_nodestr(enum NODE_TYPE nt);
bool is_signed(struct _ast_node *typespec);

#endif  // _AST_H__