#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <string>
#include "ast.hpp"
#include <cassert>


/* ============ ============ ============ ============  ============ */
/* ============ PART1 : ABSTRACT SYNTAX TREE GENERATION ============ */
/* ============ ============ ============ ============  ============ */

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
    for(i = 0;children[i]!=NULL;i++);
    // children[i] == NULL
    // i is size of children array
    new_node->children_size = i;
    new_node->children_capacity = 1;
    if (i == 0) {
        new_node->children = (struct _ast_node **) malloc(sizeof(struct _ast_node*)); 
        memcpy(new_node->children, children, sizeof(struct _ast_node*));
    } else {
        // i > 0
        while ((i >> new_node->children_capacity) != 0) {
            new_node->children_capacity++;
        }
        new_node->children_capacity = (1 << new_node->children_capacity);
        new_node->children = (struct _ast_node **) malloc(new_node->children_capacity * sizeof(struct _ast_node*)); 
        memcpy(new_node->children, children, new_node->children_size * sizeof(struct _ast_node*));
    }
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

void raise_type_error() {
    fprintf(stderr, "invalid combination of type specifiers\n");
    exit(1);
}

std::string get_nodeval(enum NODE_TYPE nt) {
    switch (nt) {
        case NODE_TYPE::TYPE_SPEC_SINT:
            return "signed int";
        case NODE_TYPE::TYPE_SPEC_UINT:
            return "unsigned int";
        case NODE_TYPE::TYPE_SPEC_UCHAR:
            return "unsigned char";
        case NODE_TYPE::TYPE_SPEC_SCHAR:
            return "signed char";
        case NODE_TYPE::TYPE_SPEC_USHORT:
            return "unsigned short";
        case NODE_TYPE::TYPE_SPEC_SSHORT:
            return "signed short";
        case NODE_TYPE::TYPE_SPEC_SHORTINT:
            return "short int";
        case NODE_TYPE::TYPE_SPEC_INTLONG:
            return "long int";
        case NODE_TYPE::TYPE_SPEC_LONGLONG:
            return "long long";
        case NODE_TYPE::TYPE_SPEC_ULONGLONG:
            return "unsigned long long";
        case NODE_TYPE::TYPE_SPEC_INTLONGLONG:
            return "long long int";
        case NODE_TYPE::TYPE_SPEC_ULONG:
            return "unsigned long";
        case NODE_TYPE::TYPE_SPEC_LONGDOUBLE:
            return "long double";
        case NODE_TYPE::TYPE_SPEC_VOID:  
            return "void";
        case NODE_TYPE::TYPE_SPEC_CHAR:  
            return "char";
        case NODE_TYPE::TYPE_SPEC_SHORT: 
            return "short";
        case NODE_TYPE::TYPE_SPEC_INT:   
            return "int";
        case NODE_TYPE::TYPE_SPEC_LONG:  
            return "long";
        case NODE_TYPE::TYPE_SPEC_FLOAT: 
            return "float";
        case NODE_TYPE::TYPE_SPEC_DOUBLE:
            return "double";
        case NODE_TYPE::TYPE_SPEC_SIGNED:
            return "signed";
        case NODE_TYPE::TYPE_SPEC_UNSIGNED: 
            return "unsigned";
        case NODE_TYPE::TYPE_SPEC_BOOL: 
            return "bool";
        default:
            return "unknown type";
    }
}

char *getc_str(std::string &&str) {
    char *retstr = (char*)malloc((str.length() + 1) * sizeof(char));
    memcpy(retstr, str.c_str(), (str.length()+1) * sizeof(char));
    return retstr;
}

void add_typespec(struct _ast_node *old_typespec, struct _ast_node *new_typespec) {
    assert(new_typespec->node_type != NODE_TYPE::TYPE_SPEC_EMPTY);
    enum NODE_TYPE new_type;
    switch(old_typespec->children[0]->node_type) {
        case NODE_TYPE::TYPE_SPEC_EMPTY:
            switch (new_typespec->node_type) {
                case NODE_TYPE::TYPE_SPEC_SIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_SINT;break;
                case NODE_TYPE::TYPE_SPEC_UNSIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_UINT;break;
                default:
                    new_type = new_typespec->node_type;
            }
            break;
        case NODE_TYPE::TYPE_SPEC_CHAR:
            switch(new_typespec->node_type) {
                case NODE_TYPE::TYPE_SPEC_UNSIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_UCHAR;break;
                case NODE_TYPE::TYPE_SPEC_SIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_SCHAR;break;
                default:
                    raise_type_error();
            }
            break;
        case NODE_TYPE::TYPE_SPEC_SHORT:
            switch (new_typespec->node_type) {
                case NODE_TYPE::TYPE_SPEC_UNSIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_USHORT;break;
                case NODE_TYPE::TYPE_SPEC_SIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_SSHORT;break;
                case NODE_TYPE::TYPE_SPEC_INT:
                    new_type = NODE_TYPE::TYPE_SPEC_SHORTINT;break;
                default:
                    raise_type_error();
            }
            break;
        case NODE_TYPE::TYPE_SPEC_INT:
            switch (new_typespec->node_type) {
                case NODE_TYPE::TYPE_SPEC_SIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_SINT;break;
                case NODE_TYPE::TYPE_SPEC_UNSIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_UINT;break;
                case NODE_TYPE::TYPE_SPEC_SHORT:
                    new_type = NODE_TYPE::TYPE_SPEC_SHORTINT;break;
                case NODE_TYPE::TYPE_SPEC_LONG:
                    new_type = NODE_TYPE::TYPE_SPEC_INTLONG;break;
                default:
                    raise_type_error();
            }
            break;
        case NODE_TYPE::TYPE_SPEC_LONG:
            switch(new_typespec->node_type) {
                case NODE_TYPE::TYPE_SPEC_SIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_SLONG;break;
                case NODE_TYPE::TYPE_SPEC_UNSIGNED:
                    new_type = NODE_TYPE::TYPE_SPEC_ULONG;break;
                case NODE_TYPE::TYPE_SPEC_INT:
                    new_type = NODE_TYPE::TYPE_SPEC_INTLONG;break;
                case NODE_TYPE::TYPE_SPEC_LONG:
                    new_type = NODE_TYPE::TYPE_SPEC_LONGLONG;break;
                case NODE_TYPE::TYPE_SPEC_DOUBLE:
                    new_type = NODE_TYPE::TYPE_SPEC_LONGDOUBLE;break;
                default:
                    raise_type_error();
            }
            break;
        case NODE_TYPE::TYPE_SPEC_DOUBLE:
            switch (new_typespec->node_type) {
                case NODE_TYPE::TYPE_SPEC_LONG:
                    new_type = NODE_TYPE::TYPE_SPEC_LONGDOUBLE;break;
                default:
                    raise_type_error();
            }
            break;
        case NODE_TYPE::TYPE_SPEC_INTLONG:
            switch (new_typespec->node_type) {
            case NODE_TYPE::TYPE_SPEC_LONG:
                new_type=NODE_TYPE::TYPE_SPEC_INTLONGLONG;break;
            default:
                raise_type_error();
            }
            break;
        default:
            raise_type_error();
    }
    new_typespec->node_type = new_type;
    free(new_typespec->node_val);   // free the old node_val
    new_typespec->node_val = getc_str(get_nodeval(new_type));
    ast_destroy(old_typespec->children[0]);
    assert(!old_typespec->children[1]);
    old_typespec->children[0] = new_typespec;
}


bool check_consistency_typequal(struct _ast_node *typequal_list, struct _ast_node *typequal) {
    struct _ast_node **typespec;
    enum NODE_TYPE type_tocheck = typequal->node_type;
    for(typespec = typequal_list->children;*typespec!=NULL;typespec++) {
        if ((*typespec)->node_type == type_tocheck)
            return false;   // this type is already present, no need to add
    }
    return true; // typequal->node_type is the new type being added
}

void add_typequal(struct _ast_node *typequal_list, struct _ast_node *typequal) {
    assert(typequal_list->node_type == NODE_TYPE::TYPE_QUAL_LIST);
    if (check_consistency_typequal(typequal_list, typequal)) {
        add_children(typequal_list, typequal);
    } else {
        ast_destroy(typequal);
    }
    // result of adding multiple type qualifier is same as having a single type qualifier (no errors)
}


/* ============ ============ ============ ==========  ========== */
/* ============ PART 2 : SYMBOL TABLE IMPLEMENTATION ============ */
/* ============ ============ ============ ==========  ========== */

// see scope_check.cpp

/* ============ ============ ============ ==========  ========== */
/* ============ PART 3 : LLVM IR CODE GENERATION ============ */
/* ============ ============ ============ ==========  ========== */

// see codegen.cpp

/* ============ ============ ============ ==========  ========== */
/* ============ PART 4 : LOCAL OPTIMIZATIONS ============ */
/* ============ ============ ============ ==========  ========== */

// see optimize.cpp