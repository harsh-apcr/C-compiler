# cc
C compiler

The C language grammar (c.y and c.l files) have been taken from:

http://www.quut.com/c/ANSI-C-grammar-y-2011.html

## Instructions on how to use this library
1. Open the terminal and cd into the source code directory
2. type make to compile all the source code
3. run the binary cc which takes a C file as an argument and (if parsing succeeds) it prints an AST on the console
    * usage : ./cc \<prog\>.c
4. NOTE : this program doesn't exhaustively support all C constructs and only supports `function_definition`, `expressions` and related parts of the grammar
5. Three sample programs are also provided (in examples directory) to test the AST implementation


An example test1.c file is provided to test the program
```C
void empty() { }
void simple_arith() {
  (10 - 10/3) << 3 | (23+8*12) & 1024;
}
void simple_arith_with_arg(int d) {
  (d > d/2) || (d >= 100) && (d < 99);
}
int printf(char const *format, ...);
```

After compiling the source code on running ./cc examples/test1.c it produces the following output (after typing 'y' on the prompt)

```
retv = 0
-------- ABSTRACT SYNTAX TREE -------
translation_unit
|---function_def
    |---void
    |---declarator
        |---function_decl
            |---identifier_decl
                |---empty
    |---cmpnd_stmt
|---function_def
    |---void
    |---declarator
        |---function_decl
            |---identifier_decl
                |---simple_arith
    |---cmpnd_stmt
        |---blk_item_list
            |---expr_stmt
                |---(|)
                    |---(<<)
                        |---(-)
                            |---10
                            |---(/)
                                |---10
                                |---3
                        |---3
                    |---(&)
                        |---(+)
                            |---23
                            |---(*)
                                |---8
                                |---12
                        |---1024
|---function_def
    |---void
    |---declarator
        |---function_decl
            |---identifier_decl
                |---simple_arith_with_arg
            |---param_list
                |---param_decl
                    |---int
                    |---declarator
                        |---identifier_decl
                            |---d
    |---cmpnd_stmt
        |---blk_item_list
            |---expr_stmt
                |---(||)
                    |---(>)
                        |---d
                        |---(/)
                            |---d
                            |---2
                    |---(&&)
                        |---(>=)
                            |---d
                            |---100
                        |---(<)
                            |---d
                            |---99
|---declaration
    |---int
    |---init_decl_list
        |---init_decl
            |---declarator
                |---function_decl
                    |---identifier_decl
                        |---printf
                    |---param_list
                        |---param_decl
                            |---const
                                |---char
                            |---declarator
                                |---pointer(*)
                                    |---type_qual_list
                                |---identifier_decl
                                    |---format
                        |---(var_args)...

```
