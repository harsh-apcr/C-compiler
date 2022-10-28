#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <vector>
#include <unordered_map>
#include "llvm/IR/Value.h"
#include <string>
#include "ast.hpp"
#include <cassert>

/*
struct _ast_node **children must not be NULL
const char *node_val must not be null
*/
struct _ast_node* ast_node_create(enum NODE_TYPE node_type, const char *node_val, struct _ast_node** children) {
    assert(node_val);
    assert(children);
    struct _ast_node* new_node = (struct _ast_node*) malloc(sizeof(struct _ast_node));
    new_node->node_type = node_type;
    new_node->node_val = node_val ? strdup(node_val) : NULL;  // dynamically allocated
    unsigned int i;
    for(i = 0;children[i]!=NULL;i++) {
        children[i];
    }
    // children[i] == NULL
    // i is size of children array
    new_node->children_size = i;
    new_node->children_capacity = 1;
    while ((i >> new_node->children_capacity) != 0) {
        new_node->children_capacity++;
    }
    new_node->children_capacity = (1 << new_node->children_capacity);
    new_node->children = (struct _ast_node **) malloc(new_node->children_capacity * sizeof(struct _ast_node*)); 
    memcpy(new_node->children, children, new_node->children_size * sizeof(struct _ast_node*));
    return new_node;
}

/*
root must be non null
*/
void ast_destroy(struct _ast_node* root) {
    assert(root);
    struct _ast_node **curr_child = root->children;
    while (*curr_child != NULL) {
        ast_destroy(*curr_child);
        curr_child++;
    }
    free(root->node_val);
    free(root->children);
    free(root);
}

/*
root must be non null
depth >= 0 
*/
void dump_ast_at_depth(struct _ast_node *root, int depth) {
    assert(root);
    assert(depth >= 0);
    int d;
    for(d = 0;d != depth;d++) {
        if (d == depth-1) printf("|---");
        else printf("    ");
    }
    printf("%s\n", root->node_val);
    struct _ast_node **curr_child = root->children;
    while (*curr_child != NULL) {
        dump_ast_at_depth(*curr_child, depth + 1);
        curr_child++;
    }
}

/*
root must be non null
*/
void dump_ast(struct _ast_node *root) {
    dump_ast_at_depth(root, 0);
}

/*
root must be non null
*/
void add_children(struct _ast_node *root, struct _ast_node* child) {
    assert(root);
    assert(child);
    if (root->children_size + 1 < root->children_capacity) {
        root->children[root->children_size++] = child;
        root->children[root->children_size] = NULL; 
    } else {
        // root->children_size + 1 == root->children_capacity
        // TODO : grow the array
        root->children_capacity *= 2;
        struct _ast_node **temp = (struct _ast_node **) malloc(root->children_capacity * sizeof(struct _ast_node*));
        memcpy(temp, root->children, root->children_size * sizeof(struct _ast_node*));
        temp[root->children_size++] = child;
        temp[root->children_size] = NULL;
        free(root->children);
        root->children = temp; 
    }
}

typedef std::vector<std::unordered_map<std::string, llvm::Value*>> symbol_table;    // stack of scopes
typedef std::unordered_map<std::string, llvm::Value*> label_table;

void enter_scope(symbol_table& sym_table) {
    std::unordered_map<std::string, llvm::Value*> new_scope; 
    sym_table.push_back(new_scope);
}

llvm::Value* find_symbol(const symbol_table& sym_table, const std::string& symbol) {

    if (sym_table.empty()) return nullptr;
    auto rend = sym_table.rend();
    auto rbegin = sym_table.rbegin(); // initial top level symbol table
    
    for(auto scope_itr = rbegin;scope_itr != rend;scope_itr++) {
        if (scope_itr->find(symbol) != scope_itr->end()) {
            // found symbol, return its value
            return scope_itr->at(symbol);
        }
    }
    // scope_itr == rend (one element past top level scope) => symbol doesn't exist in sym_table
    return nullptr;
}

// push symbol to the top scope
void add_symbol(symbol_table& sym_table, const std::string& symbol, llvm::Value* value) {
    sym_table.back().at(symbol) = value;
}

// add a label to your label_table
void add_label(label_table &label_table, const std::string& symbol) {
    label_table.at(symbol) = nullptr;
}

// checks if symbol is defined in current (top) scope, allows check for double declarations
bool check_scope(const symbol_table& sym_table, const std::string& symbol) {
    return (sym_table.back().find(symbol) != sym_table.back().end()); 
}

// check if the label has already been defined
bool check_label(const label_table &label_table, const std::string &symbol) {
    return (label_table.find(symbol) != label_table.end());
}

void exit_scope(symbol_table& sym_table) {
    if (sym_table.empty()) {
        fprintf(stderr, "exiting a scope without ever entering one!\n");
        exit(1);
    } else {
        sym_table.pop_back();
    }
}

void scope_checker(symbol_table &sym_table,label_table &label_table, struct _ast_node *root) {
    if (root == NULL) return;
    else {
        switch(root->node_type) {
            case ID:    // not in a declaration context
                if (find_symbol(sym_table, root->node_val) == nullptr) {
                    fprintf(stderr, "undeclared variable %s\n");
                    exit(1);
                }
                break;
            // scope check all their child nodes
            case ARRAY_ACCESS:
            case FUNCTION_CALL:
            case POSTINC_OP:
            case POSTDEC_OP:
            case ARG_EXPR_LIST:
            case PREINC_OP:
            case PREDEC_OP:
            case MULT:
            case DIV:
            case MOD:
            case PLUS:
            case MINUS:
            case LEFT_SHIFT:
            case RIGHT_SHIFT:
            case LESS_THAN:
            case GREATER_THAN:
            case LESSEQ:
            case GREATEREQ:
            case EQOP:
            case NEQOP:
            case BITAND:
            case BITXOR:
            case BITOR:
            case LAND:
            case LOR:
            case TERNOP:
            case ASSIGN_COMMA:                      // handel assignment_expression carefully!
            case DECL:                              // handel init_declarator_list carefully!
            case INIT_DECL_LIST:                    // handel init_declatator carefully!
            case INIT_DECL:                         // handel declarator carefully!
            case DECLARATOR:                        // pointer needs to be matched at default, check direct_declarator carefully!
            case INIT_ASSIGN_EXPR:                  // assignment_expression doesn't involve declaring/defining any variable 
            case INIT_LIST:
            case CASE_STMT:
            case DEF_STMT:
            case BLK_ITEM_LIST:
            case EXPR_STMT:
            case IF_ELSE_STMT:
            case IF_STMT:
            case SWITCH_STMT:
            case WHILE_STMT:
            case DO_WHILE_STMT:
            case FOR_STMT1:
            case FOR_STMT2:
            case FOR_STMT3:
            case FOR_STMT4:
            case RETURN_STMT2:
                struct _ast_node** child = root->children;
                for(;child!=NULL;child++) {
                    scope_checker(sym_table, label_table, *child);
                }
                break;
            
            case IDENTIFIER_DECL:
                struct _ast_node *id_node = root->children[0];
                if (check_scope(sym_table, id_node->node_val)) {
                    fprintf(stderr, "double declaration of variable %s\n", id_node->node_val);
                    exit(1);
                } else {
                    // no prev declaration of symbol in current scope
                    add_symbol(sym_table, id_node->node_val, nullptr);
                }
                break;
            case FUNCTION_DECL:
                struct _ast_node* direct_declarator_node = root->children[0];
                scope_checker(sym_table, label_table, direct_declarator_node);
                // don't need to worry about parameter_type_list (they won't affect symbol table at all)
                break;

            case LABELED_STMT:
                struct _ast_node *id_node = root->children[0];  // get the label identifier
                if (check_label(label_table, id_node->node_val)) {
                    fprintf(stderr, "double definition of label %s\n", id_node->node_val);
                    exit(1);
                } else {
                    // no prev declaration of symbol in current scope
                    add_label(label_table, id_node->node_val);
                }
                struct _ast_node *statement = root->children[1];
                scope_checker(sym_table, label_table, statement);
                break;

            // new scope starts here
            case CMPND_STMT:
                enter_scope(sym_table);
                if (root->children[0] != NULL) {
                    scope_checker(sym_table, label_table, root->children[0]);
                    // root->children[0] is a node of type block_item_list
                }
                exit_scope(sym_table);
                break;
            case GOTO_STMT:
                struct _ast_node *id_node = root->children[0];  // get the label identifier
                if (!check_label(label_table, id_node->node_val)) {
                    // label is not defined before
                    fprintf(stderr, "error: label `%s` used but not defined\n", id_node->node_val);
                    exit(1);
                }
                // label has been defined before
                break;
            case TRANSLATION_UNIT:
                label_table.clear();    // clear the label/symbol tables
                sym_table.clear();
                enter_scope(sym_table); // enter a new scope (external scope)
                struct _ast_node** child = root->children;
                for(;child!=NULL;child++) {
                    scope_checker(sym_table, label_table, *child);
                }
                exit_scope(sym_table);
                break;
            // read after line num 563

            // constants
            case I_CONST:
            case F_CONST:
            case STRING:
            case DECLSPEC:
                break;

            default:
                /* unary operators, assignment operators, type qualifiers/specifiers, type qualifier lists, 
                   pointers, ellipsis_node, continue, break, empty return stmt, 
                    (all the nodes with no variable child)
                
                */
            
        }
    }
}

void scope_checking(struct _ast_node *root) {
    symbol_table sym_table;
    label_table label_table;    // one label_table inside a function scope
    // label must only be declared inside functions
    scope_checker(sym_table, label_table, root);
}