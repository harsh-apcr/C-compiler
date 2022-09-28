#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ast.h"

struct _ast_node* ast_node_create(enum NODE_TYPE node_type, char *node_val, struct _ast_node* children[]) {
    struct _ast_node* new_node = malloc(sizeof(struct _ast_node));
    new_node->node_type = node_type;
    new_node->node_val = strdup(node_val);  // dynamically allocated
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
    new_node->children = malloc(new_node->children_capacity * sizeof(struct _ast_node*)); 
    for(i = 0;children[i]!=NULL;i++) {
        new_node->children[i] = children[i];
    }
    new_node->children[i] = NULL;
}

void ast_destroy(struct _ast_node* root) {
    struct _ast_node *curr_child = root->children;
    while (curr_child != NULL) {
        ast_destory(curr_child);
        curr_child++;
    }
    free(root->node_val);
    free(root->children);
    free(root);
}


void dump_ast_at_depth(struct _ast_node *root, int depth) {
    int d;
    for(d = 0;d != depth;d++) {
        printf("\t");
    }
    printf("%s\n", root->node_val);
    struct _ast_node *curr_child = root->children;
    while (curr_child != NULL) {
        dump_ast_at_depth(curr_child, depth + 1);
        curr_child++;
    }
}

void dump_ast(struct _ast_node *root) {
    dump_ast_at_depth(root, 0);
}

void add_children(struct _ast_node *root, struct _ast_node* child) {
    if (root->children_size < root->children_capacity) {
        root->children[root->children_size++] = child;
        root->children[root->children_size] = NULL; 
    } else {
        // root->children_size == root->children_capacity
        // TODO : grow the array
    }
}






