#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "c.tab.hpp"
#include "ast.hpp"

extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;

static void usage()
{
  printf("Usage: cc <prog.c>\n");
}

int
main(int argc, char **argv)
{
  if (argc != 2) {
    usage();
    exit(1);
  }
  
  char const *filename = argv[1];
  printf("%s\n", filename);
  yyin = fopen(filename, "r");
  assert(yyin);
  int ret = yyparse();
  printf("retv = %d\n", ret);

  if (ret == 0) {
    struct _ast_node *root_node = get_ast_root();
    scope_checking(root_node);
    printf("Do you want to see the AST representation (y/n)? ");
    char c;
    scanf("%c", &c);
    if (c == 'y') {
      printf("-------- ABSTRACT SYNTAX TREE -------\n");
      dump_ast(root_node);
      ast_destroy(root_node);   // free the memory
    }
  }
  exit(0);
}
