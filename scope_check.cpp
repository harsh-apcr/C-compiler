#include "scope_check.hpp"

void enter_scope(symbol_table& sym_table) {
    std::unordered_map<std::string, value_llvm> new_scope; 
    sym_table.push_back(new_scope);
}

value_llvm find_symbol(const symbol_table& sym_table, const std::string& symbol) {
    if (sym_table.empty()) return value_llvm(nullptr, nullptr);
    auto rend = sym_table.rend();
    auto rbegin = sym_table.rbegin(); // initial top level symbol table
    for(auto scope_itr = rbegin;scope_itr != rend;scope_itr++) {
        if (scope_itr->find(symbol) != scope_itr->end()) {
            // found symbol, return its value
            return scope_itr->at(symbol);
        }
    }
    // scope_itr == rend (one element past top level scope) => symbol doesn't exist in sym_table
    return value_llvm(nullptr, nullptr);
}

bool find_symbol_bool(const symbol_table& sym_table, const std::string& symbol) {
    if (sym_table.empty()) return false;
    auto rend = sym_table.rend();
    auto rbegin = sym_table.rbegin(); // initial top level symbol table
    for(auto scope_itr = rbegin;scope_itr != rend;scope_itr++) {
        if (scope_itr->find(symbol) != scope_itr->end()) {
            // found symbol, return its value
            //return scope_itr->at(symbol);
            return true;
        }
    }
    // scope_itr == rend (one element past top level scope) => symbol doesn't exist in sym_table
    return false;
}

// push symbol to the top scope
void add_symbol(symbol_table& sym_table, const std::string& symbol, value_llvm alloca) {
    sym_table.back().insert({symbol, alloca});
}

// add a label to your label_table (before adding a label, it is assumed that a check is performed first)
void add_label(label_table &label_table, const std::string& symbol, llvm::BasicBlock *bb,
                 std::unordered_multimap<std::string, llvm::BranchInst *> &notfound_labels,
                 bool cgen) {

    if (notfound_labels.find(symbol) != notfound_labels.end()) {
        // goto to this label has been checked previously
        if (cgen) {
            auto range = notfound_labels.equal_range(symbol);
            for(auto it = range.first;it!=range.second;it++) {
                assert(it->second); // it->second is brinst (these are not null for codegen)
                it->second->setSuccessor(0, bb);
            }
        }
        notfound_labels.erase(symbol);
    }
    label_table.insert({symbol, bb});
}

// checks if symbol is defined in current (top) scope, allows check for double declarations
bool check_scope(const symbol_table& sym_table, const std::string& symbol) {
    return (sym_table.back().find(symbol) != sym_table.back().end()); 
}

bool check_label(const label_table &label_table, const std::string &symbol) {
    return (label_table.find(symbol) != label_table.end());
}

// check if the label has already been defined
bool check_label(const label_table &label_table,
                 const std::string &symbol,
                 std::unordered_multimap<std::string, llvm::BranchInst *> &notfound_labels,
                 llvm::BranchInst *brinst = nullptr) {
    bool is_defined = (label_table.find(symbol) != label_table.end());
    if (!is_defined)
        notfound_labels.insert({symbol, brinst});
    return is_defined;
}

void exit_scope(symbol_table& sym_table) {
    if (sym_table.empty()) {
        fprintf(stderr, "error: exiting a scope without ever entering one\n");
        exit(1);
    } else {
        sym_table.pop_back();
    }
}

void print_sym_table(symbol_table& sym_table) {
    printf("\n---printing symbol table (top-level to bottom-level)---\n");
    for(auto scope = sym_table.rbegin();scope!=sym_table.rend();scope++) {
        printf("scope-level\n");
        for(auto var : *scope) {
            printf("symbol : %s\n", var.first.c_str());
        }
    }
}

// TODO: fix these errors
void scope_checker(symbol_table &sym_table,label_table &label_table,
                 std::unordered_multimap<std::string, llvm::BranchInst *> &notfound_labels,
                  struct _ast_node *root, bool is_fun_def) {
    if (root == NULL) return;
    else {
        switch(root->node_type) {
            case ID:    // not in a declaration context
                if (!find_symbol_bool(sym_table, root->node_val)) {
                    fprintf(stderr, "error: undeclared variable or function `%s`\n", root->node_val);
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
            case EXPRESSION:                        // handel assignment_expression carefully!
            case DECLARATION:                       // handel init_declarator_list carefully!
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
            case RETURN_STMT2: {
                for(struct _ast_node** child = root->children;*child!=NULL;child++) {
                    scope_checker(sym_table, label_table, notfound_labels, *child, is_fun_def);
                }
              //  printf("exiting node-val : %s\n", root->node_val);
                break;
            }
            case FUNCTION_PTRDECL: {
                // root->children[0]->type == ID
                ADD_IDNODE(root, sym_table);
                break;
            }
            case IDENTIFIER_DECL: {
                // root->children[0]->type == ID
                ADD_IDNODE(root, sym_table)
                break;
            }
            case FUNCTION_DECL: {
                const char *fun_name = root->children[0]->node_val;  // root->children[0]->type == ID
                if (!is_fun_def)  {  // not a function definition (just a declaration)
                    ADD_IDNODE(root, sym_table)
                }
                else  { 
                    // it is a function definition
                    // check if it has already been declared or not                    
                    if (!check_scope(sym_table, fun_name)) 
                        ADD_IDNODE(root, sym_table)
                    // otherwise it is already present in the symbol table
                    struct _ast_node* param_type_list = root->children[1];
                    printf("entered scope -function_decl\n");
                    enter_scope(sym_table);
                    if (param_type_list != NULL)
                        scope_checker(sym_table, label_table, notfound_labels, param_type_list, true);   // handel this case
                }
                break;
            }

            case PARAM_LIST: {
                if (is_fun_def) {
                    for(struct _ast_node** child = root->children;*child!=NULL;child++) {
                        scope_checker(sym_table, label_table, notfound_labels, *child, is_fun_def);
                    }
                }
                break;
            }

            case PARAM_DECL: {
                if (is_fun_def) {
                    if (root->children[1] == NULL) {
                        // root->children[1] is a declarator node
                        fprintf(stderr, "error: unnamed prototyped parameters not allowed when body is present\n");
                        exit(1);
                    } else {
                        // declarator node is not null
                        scope_checker(sym_table, label_table, notfound_labels, root->children[1], true);
                    }
                }
                break;
            }
            case LABELED_STMT: {
                ADD_LABEL(root->children[0]->node_val, label_table, nullptr, notfound_labels, false);
                struct _ast_node *statement = root->children[1];
                scope_checker(sym_table, label_table, notfound_labels, statement, false);
                break;
            }
            // new scope starts here
            case CMPND_STMT: {
                if (!is_fun_def) { printf("entered scope -cmpd_stmt\n");enter_scope(sym_table);}
                if (root->children[0] != NULL) {
                    scope_checker(sym_table, label_table, notfound_labels, root->children[0], false);
                    // root->children[0] is a node of type block_item_list
                }
                printf("exiting scope -cmpd_stmt\n");
                exit_scope(sym_table);
                break;
            }
            case GOTO_STMT: {
                struct _ast_node *id_node = root->children[0];  // get the label identifier
                check_label(label_table, id_node->node_val, notfound_labels);
                break;
            }
            case TRANSLATION_UNIT: {
                assert(label_table.empty());
                assert(notfound_labels.empty());
                assert(sym_table.empty());
                printf("entered scope -translation_unit\n");
                enter_scope(sym_table); // enter a new scope (external scope)
                for(struct _ast_node** child = root->children;*child!=NULL;child++) {
                    scope_checker(sym_table, label_table, notfound_labels, *child, false);
                }
                assert(label_table.empty());
                assert(notfound_labels.empty());
                printf("exiting scope - translation_unit\n");
                exit_scope(sym_table);
                break;
            }
            // read after line num 563
            case FUNCTION_DEF: {
                // there are no labels initially
                assert(label_table.empty());
                assert(notfound_labels.empty());
                struct _ast_node* decl_spec = root->children[0];
                struct _ast_node* declarator = root->children[1];
                struct _ast_node* cmpd_stmt = root->children[2];
                // no checks needed for node decl_spec
                scope_checker(sym_table, label_table, notfound_labels, declarator, true);
                scope_checker(sym_table, label_table, notfound_labels, cmpd_stmt, true); 
                label_table.clear();
                if (!notfound_labels.empty()) {
                    fprintf(stderr, "error: label `%s` is used but not defined\n", notfound_labels.begin()->first.c_str());
                    exit(1);
                }
                break;
            }
            default:
                /* unary operators, assignment operators, type qualifiers/specifiers, type qualifier lists, 
                   pointers, ellipsis_node, continue, break, empty return stmt, constants, string, decl_spec
                    (all the nodes with no variable child)
                */
               break;
            
        }
    }
}


void scope_checking(struct _ast_node *root) {
    assert(root->node_type == TRANSLATION_UNIT);
    symbol_table sym_table;
    label_table label_table;    // one label_table inside a function scope
    // label must only be declared inside functions

    std::unordered_multimap<std::string, llvm::BranchInst *> notfound_labels;
    scope_checker(sym_table, label_table, notfound_labels, root, false);
}

inline void enter_scope(label_stack& label_stack) {
    label_table new_table;
    label_stack.push_back(new_table);
}

inline llvm::BasicBlock *find_label_top(const label_stack &label_stack, const std::string &label_name) {
    label_table top_table = label_stack.back();
    return top_table[label_name];   // if label_name doesn't exist as a key then returns nullptr
}

inline void push_label(label_stack &label_stack, const std::string &symbol, llvm::BasicBlock *bb) {
    label_stack.back()[symbol] = bb;
}

inline void exit_scope(label_stack &label_stack) {
    label_stack.pop_back();
}

llvm::BasicBlock *find_label(const label_stack &label_stack, const std::string &label_name) {
    if (label_stack.empty()) return nullptr;
    auto rend = label_stack.rend();
    auto rbegin = label_stack.rbegin(); // initial top level symbol table
    for(auto scope_itr = rbegin;scope_itr != rend;scope_itr++) {
        if (scope_itr->find(label_name) != scope_itr->end()) {
            // found symbol, return its value
            return scope_itr->at(label_name);
        }
    }
    // scope_itr == rend (one element past top level scope) => symbol doesn't exist in sym_table
    return nullptr;
}
