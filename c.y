%{
#include <cstdio>
#include <iostream>
using namespace std;

#include "ast/ast.h"
// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;
 
void yyerror(const char *s);
typedef struct _ast_node ast_node

ast_node *root;

%}
%union{
	ast_node *node;char *sval;
}	
%token <sval> IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token <sval> PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token <sval> AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token <sval> SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token <sval> XOR_ASSIGN OR_ASSIGN
%token <sval> TYPEDEF_NAME ENUMERATION_CONSTANT

%token <sval> TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token <sval> CONST RESTRICT VOLATILE
%token <sval> BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token <sval> COMPLEX IMAGINARY 
%token <sval> STRUCT UNION ENUM ELLIPSIS

%token <sval> CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token <sval> ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%type <node> primary_expression constant enumeration_constant string generic_selection
%type <node> generic_assoc_list generic_association postfix_expression
%type <node> argument_expression_list unary_expression unary_operator cast_expression multiplicative_expression
%type <node> additive_expression shift_expression relational_expression equality_expression and_expression
%type <node> exclusive_or_expression inclusive_or_expression logical_and_expression logical_or_expression
%type <node> conditional_expression assignment_expression assignment_operator expression constant_expression
%type <node> declaration declaration_specifiers init_declarator_list init_declarator storage_class_specifier
%type <node> type_specifier struct_or_union_specifier struct_or_union struct_declaration_list struct_declaration
%type <node> specifier_qualifier_list struct_declarator_list struct_declarator enum_specifier enumerator_list
%type <node> enumerator atomic_type_specifier type_qualifier function_specifier alignment_specifier
%type <node> declarator direct_declarator pointer type_qualifier_list parameter_type_list parameter_list
%type <node> parameter_declaration identifier_list type_name abstract_declarator direct_abstract_declarator
%type <node> initializer initializer_list designation designator_list designator static_assert_declaration statement
%type <node> labeled_statement compound_statement block_item_list block_item expression_statement selection_statement
%type <node> iteration_statement jump_statement new_start translation_unit external_declaration function_definition declaration_list


%start translation_unit
%%

primary_expression
	: IDENTIFIER			{ $$ = ast_node_create(IDENTIFIER, $1, {NULL});}
	| constant
	| string
	| '(' expression ')'	{ $$ = $2; }
	| generic_selection
	;

constant
	: I_CONSTANT		/* includes character_constant */	{ $$ = ast_node_create(INT_CONST, $1, {NULL}); }
	| F_CONSTANT											{ $$ = ast_node_create(FLOAT_CONST, $1, {NULL}); }
//	| ENUMERATION_CONSTANT	/* after it has been defined as such */
	;
/*
enumeration_constant		// before it has been defined as such
	: IDENTIFIER
	;

string
	: STRING_LITERAL
	| FUNC_NAME
	;

generic_selection
	: GENERIC '(' assignment_expression ',' generic_assoc_list ')'
	;

generic_assoc_list
	: generic_association
	| generic_assoc_list ',' generic_association
	;

generic_association
	: type_name ':' assignment_expression
	| DEFAULT ':' assignment_expression
	;
*/
postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'			{ $$ = ast_node_create(ARRAY_ACCESS, "[]", {$1, $3, NULL}); }
	| postfix_expression '(' ')'					{ $$ = ast_node_create(FUNCTION_CALL, "()", {$1, NULL});}
	| postfix_expression '(' argument_expression_list ')'	{ $$ = ast_node_create(FUNCTION_CALL, "()", {$1, $3, NULL});}
/*	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER	*/
	| postfix_expression INC_OP						{ $$ = ast_node_create(POSTINC_OP, "++", {$1, NULL})}
	| postfix_expression DEC_OP						{ $$ = ast_node_create(POSTDEC_OP, "--", {$1, NULL})}
/*	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}' */
	;

argument_expression_list
	: assignment_expression							{ $$ = ast_node_create(ARG_EXPR_LIST, "ARG_EXPR_LIST", {$1, NULL});}
	| argument_expression_list ',' assignment_expression	{ add_children($1, $3); $$ = $1; }
	;

unary_expression
	: postfix_expression
	| INC_OP unary_expression						{ $$ = ast_node_create(PREINC_OP, "++", {$2, NULL});}
	| DEC_OP unary_expression						{ $$ = ast_node_create(PREDEC_OP, "--", {$2, NULL});}
	| unary_operator cast_expression				{ add_children($1, $2); $$ = $1; }
/*	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	| ALIGNOF '(' type_name ')'	*/
	;

unary_operator
	: '&'											{ $$ = ast_node_create(ADDR_OP, "&", {NULL});}
	| '*'											{ $$ = ast_node_create(DEREF_OP, "*", {NULL});}
	| '+'											{ $$ = ast_node_create(UNPLUS, "+", {NULL});}
	| '-'											{ $$ = ast_node_create(UNMINUS, "-", {NULL});}
	| '~'											{ $$ = ast_node_create(BIT_COMP, "~", {NULL});}
	| '!'											{ $$ = ast_node_create(NOT, "!", {NULL});}
	;

cast_expression
	: unary_expression
/*	| '(' type_name ')' cast_expression	*/
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression				{ $$ = ast_node_create(MULT, "*", {$1, $3, NULL});}
	| multiplicative_expression '/' cast_expression				{ $$ = ast_node_create(DIV, "/", {$1, $3, NULL});}
	| multiplicative_expression '%' cast_expression				{ $$ = ast_node_create(MOD, "\%", {$1, $3, NULL});}
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression			{ $$ = ast_node_create(PLUS, "+", {$1, $3, NULL});}
	| additive_expression '-' multiplicative_expression			{ $$ = ast_node_create(MINUS, "-", {$1, $3, NULL});}
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression				{ $$ = ast_node_create(LEFT_SHIFT, "<<", {$1, $3, NULL})};
	| shift_expression RIGHT_OP additive_expression				{ $$ = ast_node_create(RIGHT_SHIFT, ">>", {$1, $3, NULL})};
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression				{ $$ = ast_node_create(LESS_THAN, "<", {$1, $3, NULL});}
	| relational_expression '>' shift_expression				{ $$ = ast_node_create(GREATER_THAN, ">", {$1, $3, NULL});}
	| relational_expression LE_OP shift_expression				{ $$ = ast_node_create(LESSEQ, "<=", {$1, $3, NULL});}
	| relational_expression GE_OP shift_expression				{ $$ = ast_node_create(GREATEREQ, ">=", {$1, $3, NULL});}
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression			{ $$ = ast_node_create(EQOP, "==", {$1, $3, NULL}); }
	| equality_expression NE_OP relational_expression			{ $$ = ast_node_create(NEQOP, "!=", {$1, $3, NULL}); }
	;

and_expression
	: equality_expression										
	| and_expression '&' equality_expression					{ $$ = ast_node_create(BITAND, "&", {$1, $3, NULL}); }
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression				{ $$ = ast_node_create(BITXOR, "^", {$1, $3, NULL}); }
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression		{ $$ = ast_node_create(BITOR, "|", {$1, $3, NULL}); }
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression		{ $$ = ast_node_create(LAND, "&&", {$1, $3, NULL});}
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression		{ $$ = ast_node_create(LOR, "&&", {$1, $3, NULL});}
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression	{ $$ = ast_node_create(TERNOP, "?:", {$1, $3, $5}); }
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression		{ add_children($2, $1); add_children($2, $3); $$ = $2;}
	;

assignment_operator
	: '='														{ $$ = ast_node_create(ASSIGN, "=", {NULL});}
	| MUL_ASSIGN												{ $$ = ast_node_create(MULASSIGN, "*=", {NULL});}
	| DIV_ASSIGN												{ $$ = ast_node_create(DIVASSIGN, "/=", {NULL});}
	| MOD_ASSIGN												{ $$ = ast_node_create(MODASSIGN, "\%=", {NULL});}
	| ADD_ASSIGN												{ $$ = ast_node_create(ADDASSIGN, "+=", {NULL});}
	| SUB_ASSIGN												{ $$ = ast_node_create(SUBASSIGN, "-=", {NULL});}
	| LEFT_ASSIGN												{ $$ = ast_node_create(LEFTASSIGN, "<<=", {NULL});}
	| RIGHT_ASSIGN												{ $$ = ast_node_create(RIGHTASSIGN, ">>=", {NULL});}
	| AND_ASSIGN												{ $$ = ast_node_create(BITANDASSIGN, "&=", {NULL});}
	| XOR_ASSIGN												{ $$ = ast_node_create(BITXORASSIGN, "^=", {NULL});}
	| OR_ASSIGN													{ $$ = ast_node_create(BITORASSIGN, "|=", {NULL});}	
	;

expression
	: assignment_expression
	| expression ',' assignment_expression						{ $$ = ast_node_create(ASSIGNCOMMA, ",", {$1, $3, NULL});}
	;

/*
constant_expression
	: conditional_expression	// with constraints
	;
*/
declaration
	: declaration_specifiers ';'								{ $$ = ast_node_create(DECLARATION, ";", {$1, NULL});}
	| declaration_specifiers init_declarator_list ';'			{ $$ = ast_node_create(DECLARATION, ";", {$1, $2, NULL}); }
//	| static_assert_declaration
	;

type_name
	: declaration_specifiers pointer
	| declaration_specifiers
	;

declaration_specifiers
	:/* storage_class_specifier declaration_specifiers
	| storage_class_specifier
	*/
	| type_specifier declaration_specifiers		{ add_children($2, $1); $$ = $2;}
	| type_specifier							{ ast_node_create(DECLSPEC, "DECLSPEC", {$1, NULL});}
	| type_qualifier declaration_specifiers		{ add_children($2, $1); $$ = $2;}
	| type_qualifier							{ ast_node_create(DECLSPEC, "DECLSPEC", {$1, NULL});}
	/*| function_specifier declaration_specifiers
	| function_specifier
	| alignment_specifier declaration_specifiers
	| alignment_specifier */
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator '=' initializer
	| declarator
	;
/*
storage_class_specifier
	: TYPEDEF	// identifiers must be flagged as TYPEDEF_NAME
	| EXTERN
	| STATIC
	| THREAD_LOCAL
	| AUTO
	| REGISTER
	;
*/
type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| BOOL
/*	| COMPLEX
	| IMAGINARY	  	// non-mandated extension
	| atomic_type_specifier
	| struct_or_union_specifier
	| enum_specifier
	| TYPEDEF_NAME	// after it has been defined as such */
	;

/*
struct_or_union_specifier
	: struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';'	// for anonymous struct/union
	| specifier_qualifier_list struct_declarator_list ';'
	| static_assert_declaration
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: ':' constant_expression
	| declarator ':' constant_expression
	| declarator
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator	// identifiers must be flagged as ENUMERATION_CONSTANT
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;

atomic_type_specifier
	: ATOMIC '(' type_name ')'
	;
*/

type_qualifier
	: CONST /*
	| RESTRICT
	| VOLATILE
	| ATOMIC */
	;
/*
function_specifier
	: INLINE
	| NORETURN
	;

alignment_specifier
	: ALIGNAS '(' type_name ')'
	| ALIGNAS '(' constant_expression ')'
	;
*/
declarator
	: pointer direct_declarator
	| direct_declarator
	;

direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']'
	| direct_declarator '(' parameter_type_list ')'
	| direct_declarator '(' ')'
	| direct_declarator '(' identifier_list ')'
	;

pointer
	: '*' type_qualifier_list pointer
	| '*' type_qualifier_list
	| '*' pointer
	| '*'
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;


parameter_type_list
	: parameter_list ',' ELLIPSIS
	| parameter_list
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;
/*
identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

type_name
	: specifier_qualifier_list abstract_declarator
	| specifier_qualifier_list
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' '*' ']'
	| '[' STATIC type_qualifier_list assignment_expression ']'
	| '[' STATIC assignment_expression ']'
	| '[' type_qualifier_list STATIC assignment_expression ']'
	| '[' type_qualifier_list assignment_expression ']'
	| '[' type_qualifier_list ']'
	| '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' '*' ']'
	| direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list ']'
	| direct_abstract_declarator '[' assignment_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	| assignment_expression
	;

initializer_list
	: designation initializer
	| initializer
	| initializer_list ',' designation initializer
	| initializer_list ',' initializer
	;

designation
	: designator_list '='
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: '[' constant_expression ']'
	| '.' IDENTIFIER
	;

static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	;
*/
statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: IDENTIFIER ':' statement
	| CASE constant_expression ':' statement
	| DEFAULT ':' statement
	;

compound_statement
	: '{' '}'
	| '{'  block_item_list '}'
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| statement
	;

expression_statement
	: ';'
	| expression ';'
	;
/*
selection_statement
	: IF '(' expression ')' statement ELSE statement
	| IF '(' expression ')' statement
	| SWITCH '(' expression ')' statement
	;

iteration_statement
	: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement
	| FOR '(' declaration expression_statement expression ')' statement
	;

jump_statement
	: GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN expression ';'
	;
*/

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	;
/*
declaration_list
	: declaration
	| declaration_list declaration
	;
*/
%%
#include <stdio.h>

void yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", s);
}
