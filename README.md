# cc
C compiler

The C language grammar (c.y and c.l files) have been taken from:

http://www.quut.com/c/ANSI-C-grammar-y-2011.html

## Instructions on how to use this library
1. Open the terminal and cd into the source code directory
2. type make to compile all the source code
3. run the binary cc which has the following usage (\<outfile.ll\> is optional and if not provided default filename would be `a.ll`)
    * Usage: ./cc <\prog.c\>  -dumpast
    *        ./cc <\prog.c\>  -scopechk
    *        ./cc <\prog.c\>  -emit-ir [<outfile.ll>]
4. NOTE : this program doesn't exhaustively support all C constructs but supports most of them and you'll get appropriate warnings/error for invalid/unsupported programs
5. some sample programs are also provided (in examples directory) to test the AST implementation
6. their corresponding sample output is in `out/` directory

## Supported Types

* All the integer types (i.e `int`, `long int`, `short int` etc.) are supported, if some combination of type specifiers is not supported you'll get the appropriate error message
* Floating point types are also supported (i.e `float` and `double`)
* Implicit type conversion among `int` types, `float` types and across `int` and `float` types are supported (using llvm instructions such as `sitofp` or `fptoui` etc.)
* Storage type specifiers are not supported (they're not part of the language)
* Type qualifiers are not supported (they're just ignored)

## Supported C constructs
* All the declarations/expressions are supported
* Among the control-flow statements, except for-loop all the C statements are supported (i.e. `if-else`, `while`, `do-while`, `switch statements`, `case and default statements`, `labeled statements and gotos`, `break`, `continue`, `return` etc.)
* Gracefull exiting of invalid C program is supported (i.e. proper error mentioning during codegen/scope checking)
* Any construct that is not supported you'll get an appropriate error message for it

## Supported Optimization

* Some basic local optimizations like local-constant-folding, local-deadcode removal are supported
* local-constant folding is supported for logical and/or operations only
* dead-code removal is supported for `if-else`, `while` statements (removal of statement code that is unreachable)
* REMARK on deadcode removal :
  * for example : if condition of `if` stmt is true but `else` block has a label inside it then both `then` as well as `else` statement codes are generated, this is because there can be a goto in the program that branches to those labels, similarly for `while` statements