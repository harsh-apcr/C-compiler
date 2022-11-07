#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "c.tab.hpp"
#include "ast.hpp"
#include <string>
#include "llvm/Support/raw_ostream.h"

extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;

static void usage()
{
  printf("Usage: ./cc <prog.c>  -dumpast\n       ./cc <prog.c>  -scopechk\n       ./cc <prog.c>  -emit-ir [<outfile.ll>]\n");
}

int
main(int argc, char **argv)
{
  if (argc != 3 && argc != 4) {
    usage();
    exit(1);
  }

  char const *filename = argv[1];
  printf("%s\n", filename);
  yyin = fopen(filename, "r");
  assert(yyin);
  int ret = yyparse();
  if (ret != 0) {
    fprintf(stderr, "error: syntax error retv = %d\n", ret);
    exit(1);
  }
  struct _ast_node *root_node = get_ast_root();
  if (!strcmp(argv[2], "-dumpast")) {
    printf("-------- ABSTRACT SYNTAX TREE -------\n");
    dump_ast(root_node);
    exit(0);
  }
  scope_checking(root_node);
  if (!strcmp(argv[2], "-scopechk"))
    exit(0);

  if (!strcmp(argv[2], "-emit-ir")) {
    std::string out_filename;
    if (argv[3] == NULL)
      out_filename = "a.ll";
    else 
      out_filename = argv[3];
    print_module(root_node, out_filename);
    ast_destroy(root_node);   // free the memory
    exit(0);
  }
  fprintf(stderr, "invalid input flag\n");
  exit(1);

  
}
