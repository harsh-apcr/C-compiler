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
