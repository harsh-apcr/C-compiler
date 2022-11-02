%code top {
#include <cstdio>
#include <iostream>
#include <cstring>
using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;
void yyerror(const char *s);

}

%code requires {
#include "ast.hpp"
struct _ast_node* get_ast_root();
extern struct _ast_node *root;
}

%code {
#include "ast.hpp"
struct _ast_node* temp[5];
struct _ast_node *root;
}

%union{
	struct _ast_node *node;char *sval;
}

%token <sval> IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token <sval> PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token <sval> AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token <sval> SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token <sval> XOR_ASSIGN OR_ASSIGN
%token <sval> TYPEDEF_NAME ENUMERATION_CONSTANT

%token <sval> TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token <sval> CONST RESTRICT VOLATILE ATOMIC
%token <sval> BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token <sval> COMPLEX IMAGINARY 
%token <sval> STRUCT UNION ENUM ELLIPSIS

%token <sval> CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token <sval> ALIGNAS ALIGNOF GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%type <node> constant string
%type <node> unary_operator assignment_operator
%type <node> declaration declaration_specifiers init_declarator_list init_declarator initializer initializer_list
%type <node> type_specifier type_qualifier
%type <node> declarator direct_declarator pointer type_qualifier_list parameter_type_list parameter_list
%type <node> parameter_declaration
%type <node> statement labeled_statement compound_statement block_item_list block_item expression_statement
%type <node> translation_unit external_declaration function_definition selection_statement iteration_statement jump_statement

%type <node> primary_expression argument_expression_list unary_expression cast_expression postfix_expression exclusive_or_expression 
%type <node> multiplicative_expression additive_expression shift_expression relational_expression equality_expression and_expression
%type <node> inclusive_or_expression logical_and_expression logical_or_expression conditional_expression assignment_expression expression constant_expression


%start translation_unit
%%

primary_expression
	: IDENTIFIER			{ temp[0] = NULL; $$ = ast_node_create(ID, $1, temp);}
	| constant
	| string
	| '(' expression ')'	{ $$ = $2; }
//	| generic_selection
	;

constant
	: I_CONSTANT		/* includes character_constant */	{ temp[0] = NULL; $$ = ast_node_create(I_CONST, $1, temp); }
	| F_CONSTANT											{ temp[0] = NULL; $$ = ast_node_create(F_CONST, $1, temp); }
//	| ENUMERATION_CONSTANT	/* after it has been defined as such */
	;
/*
enumeration_constant		// before it has been defined as such
	: IDENTIFIER
	;
*/
string
	: STRING_LITERAL										{ temp[0] = NULL; $$ = ast_node_create(STRING, $1, temp); }
//	| FUNC_NAME
	;
/*
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
	| postfix_expression '[' expression ']'					{ temp[0] = $1;temp[1] = $3;temp[2] = NULL; $$ = ast_node_create(ARRAY_ACCESS, "[]", temp); }
	| postfix_expression '(' ')'							{ temp[0] = $1;temp[1] = NULL; $$ = ast_node_create(FUNCTION_CALL, "()", temp);}
	| postfix_expression '(' argument_expression_list ')'	{ temp[0] = $1;temp[1] = $3;temp[2] = NULL;$$ = ast_node_create(FUNCTION_CALL, "()", temp);}
/*	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER	*/
	| postfix_expression INC_OP								{ temp[0] = $1;temp[1] = NULL;$$ = ast_node_create(POSTINC_OP, "++(post)", temp);}
	| postfix_expression DEC_OP								{ temp[0] = $1;temp[1] = NULL;$$ = ast_node_create(POSTDEC_OP, "-- (post)", temp);}
/*	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}' */
	;

argument_expression_list
	: assignment_expression							{ temp[0] = $1;temp[1] = NULL;$$ = ast_node_create(ARG_EXPR_LIST, "arg_expr_list", temp);}
	| argument_expression_list ',' assignment_expression	{ add_children($1, $3); $$ = $1; }
	;

unary_expression
	: postfix_expression
	| INC_OP unary_expression						{ temp[0] = $2;temp[1] = NULL;$$ = ast_node_create(PREINC_OP, "(pre)++", temp);}
	| DEC_OP unary_expression						{ temp[0] = $2;temp[1] = NULL;$$ = ast_node_create(PREDEC_OP, "(pre)--", temp);}
	| unary_operator cast_expression				{ add_children($1, $2); $$ = $1; }
/*	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	| ALIGNOF '(' type_name ')'	*/
	;

unary_operator
	: '&'											{ temp[0]=NULL;$$ = ast_node_create(ADDR_OP, "(&)", temp);}
	| '*'											{ temp[0]=NULL;$$ = ast_node_create(DEREF_OP, "(*)", temp);}
	| '+'											{ temp[0]=NULL;$$ = ast_node_create(UNPLUS, "(+)", temp);}
	| '-'											{ temp[0]=NULL;$$ = ast_node_create(UNMINUS, "(-)", temp);}
	| '~'											{ temp[0]=NULL;$$ = ast_node_create(BIT_COMP, "(~)",temp);}
	| '!'											{ temp[0]=NULL;$$ = ast_node_create(NOT, "(!)", temp);}
	;

cast_expression
	: unary_expression
/*	| '(' type_name ')' cast_expression	*/
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(MULT, "(*)", temp);}
	| multiplicative_expression '/' cast_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(DIV, 	"(/)", temp);}
	| multiplicative_expression '%' cast_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(MOD, 	"(\%)", temp);}
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression			{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(PLUS, "(+)", temp);}
	| additive_expression '-' multiplicative_expression			{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(MINUS,"(-)", temp);}
	;
shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(LEFT_SHIFT, "(<<)", temp);};
	| shift_expression RIGHT_OP additive_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(RIGHT_SHIFT,"(>>)", temp);};
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(LESS_THAN, "(<)", temp);}
	| relational_expression '>' shift_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(GREATER_THAN, "(>)", temp);}
	| relational_expression LE_OP shift_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(LESSEQ, "(<=)", temp);}
	| relational_expression GE_OP shift_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(GREATEREQ, "(>=)", temp);}
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression			{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(EQOP, "(==)", temp); }
	| equality_expression NE_OP relational_expression			{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(NEQOP, "(!=)", temp); }
	;

and_expression
	: equality_expression										
	| and_expression '&' equality_expression					{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(BITAND, "(&)", temp); }
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression				{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(BITXOR, "(^)", temp); }
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression		{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(BITOR, "(|)", temp); }
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression		{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(LAND, "(&&)", temp);}
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression		{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(LOR, "(||)", temp);}
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression	{ temp[0]=$1;temp[1]=$3;temp[2]=$5;temp[3]=NULL;$$ = ast_node_create(TERNOP, "(? :)", temp); }
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression		{ add_children($2, $1); add_children($2, $3); $$ = $2;}
	;

assignment_operator
	: '='														{ temp[0]=NULL;$$ = ast_node_create(ASSIGN, "(=)", temp);}
	| MUL_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(MULASSIGN, "(*=)", temp);}
	| DIV_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(DIVASSIGN, "(/=)", temp);}
	| MOD_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(MODASSIGN, "(\%=)",temp);}
	| ADD_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(ADDASSIGN, "(+=)", temp);}
	| SUB_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(SUBASSIGN, "(-=)", temp);}
	| LEFT_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(LEFTASSIGN, "(<<=)", temp);}
	| RIGHT_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(RIGHTASSIGN, "(>>=)",temp);}
	| AND_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(BITANDASSIGN, "(&=)",temp);}
	| XOR_ASSIGN												{ temp[0]=NULL;$$ = ast_node_create(BITXORASSIGN, "(^=)",temp);}
	| OR_ASSIGN													{ temp[0]=NULL;$$ = ast_node_create(BITORASSIGN, "(|=)", temp);}	
	;

expression
	: assignment_expression
	| expression ',' assignment_expression						{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(ASSIGN_COMMA, "assign_comma", temp);}
	;


constant_expression
	: conditional_expression	/* with constraints */
	;

declaration
	: declaration_specifiers ';'								{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(DECL, "declaration", temp);}
	| declaration_specifiers init_declarator_list ';'			{ temp[0]=$1;temp[1]=$2;temp[2]=NULL;$$ = ast_node_create(DECL, "declaration", temp); }
//	| static_assert_declaration
	;

/*type_name
	: declaration_specifiers pointer
	| declaration_specifiers
	;
*/
declaration_specifiers
	/* storage_class_specifier declaration_specifiers
	| storage_class_specifier
	*/
	: type_specifier declaration_specifiers		{ add_children($2, $1); $$ = $2;}
	| type_specifier							{ temp[0]=$1;temp[1]=NULL;ast_node_create(DECLSPEC, "declaration_spec", temp);}
	| type_qualifier declaration_specifiers		{ add_children($2, $1); $$ = $2;}
	| type_qualifier							{ temp[0]=$1;temp[1]=NULL;ast_node_create(DECLSPEC, "declaration_spec", temp);}
	/*| function_specifier declaration_specifiers
	| function_specifier
	| alignment_specifier declaration_specifiers
	| alignment_specifier */
	;

init_declarator_list
	: init_declarator								{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(INIT_DECL_LIST, "init_decl_list", temp);}
	| init_declarator_list ',' init_declarator		{ add_children($1, $3); $$ = $1;}
	;

init_declarator
	: declarator '=' initializer					{ temp[0]=$1;temp[1]=$3;temp[2]=NULL;$$ = ast_node_create(INIT_DECL, "init_decl", temp);}
	| declarator									{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(INIT_DECL, "init_decl", temp);}
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
	: VOID											{ temp[0]=NULL;$$ = ast_node_create(TYPE_VOID, "void", temp);}
	| CHAR											{ temp[0]=NULL;$$ = ast_node_create(TYPE_CHAR, "char", temp);}
	| SHORT											{ temp[0]=NULL;$$ = ast_node_create(TYPE_SHORT, "short", temp);}
	| INT											{ temp[0]=NULL;$$ = ast_node_create(TYPE_INT, "int", temp);}
	| LONG											{ temp[0]=NULL;$$ = ast_node_create(TYPE_LONG, "long", temp);}
	| FLOAT											{ temp[0]=NULL;$$ = ast_node_create(TYPE_FLOAT, "float", temp);}
	| DOUBLE										{ temp[0]=NULL;$$ = ast_node_create(TYPE_DOUBLE, "double", temp);}
	| SIGNED										{ temp[0]=NULL;$$ = ast_node_create(TYPE_SIGNED, "signed", temp);}
	| UNSIGNED										{ temp[0]=NULL;$$ = ast_node_create(TYPE_UNSIGNED, "unsigned", temp);}
	| BOOL											{ temp[0]=NULL;$$ = ast_node_create(TYPE_BOOL, "bool", temp);}
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
	: CONST 				{ temp[0]=NULL;$$ = ast_node_create(TYPE_QUAL, $1, temp);}
	| RESTRICT				{ temp[0]=NULL;$$ = ast_node_create(TYPE_QUAL, $1, temp);}		
	| VOLATILE				{ temp[0]=NULL;$$ = ast_node_create(TYPE_QUAL, $1, temp);}
	| ATOMIC				{ temp[0]=NULL;$$ = ast_node_create(TYPE_QUAL, $1, temp);}
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
	: pointer direct_declarator			{ temp[0]=$2;temp[1]=$1;temp[2]=NULL;$$ = ast_node_create(DECLARATOR, "declarator", temp); }
	| direct_declarator					{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(DECLARATOR, "declarator", temp); }
	;

direct_declarator
	: IDENTIFIER						{ temp[0]=NULL;temp[1]=ast_node_create(ID, $1, temp);temp[2]=NULL;$$ = ast_node_create(IDENTIFIER_DECL, "identifier_decl", temp+1); }
	| '(' '*' IDENTIFIER ')' '(' parameter_type_list ')'			{ temp[0]=NULL;temp[1]=ast_node_create(ID, $3, temp);temp[2]=$6;temp[3]=NULL;$$ = ast_node_create(FUNCTION_PTRDECL, "function_ptrdecl", temp+1); }
	| '(' '*' IDENTIFIER ')' '(' ')'								{ temp[0]=NULL;temp[1]=ast_node_create(ID, $3, temp);temp[2]=NULL;$$ = ast_node_create(FUNCTION_PTRDECL, "function_ptrdecl", temp+1); }
	// we might need to support array of function pointers separately
/*	| direct_declarator '[' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']' */
	| IDENTIFIER '(' parameter_type_list ')'		{ temp[0]=NULL;temp[1]=ast_node_create(ID, $1, temp);temp[2]=$3;temp[3]=NULL;$$ = ast_node_create(FUNCTION_DECL, "function_decl", temp+1);}
	| IDENTIFIER '(' ')'							{ temp[0]=NULL;temp[1]=ast_node_create(ID, $1, temp);temp[2]=NULL;$$ = ast_node_create(FUNCTION_DECL, "function_decl", temp+1);}
//	| direct_declarator '(' identifier_list ')'
	;

pointer
	: '*' type_qualifier_list pointer		{ add_children($3, $2); $$ = $3; }
	| '*' type_qualifier_list				{ temp[0]=$2;temp[1]=NULL;$$ = ast_node_create(PTR, "pointer", temp); }
	| '*' pointer							{ temp[0]=NULL;add_children($2, ast_node_create(TYPE_QUAL_LIST, "type_qual_list", temp)); $$ = $2; }
	| '*'									{ temp[0]=NULL;temp[1]=ast_node_create(TYPE_QUAL_LIST, "type_qual_list", temp);temp[2]=NULL;$$ = ast_node_create(PTR, "pointer", temp+1); }
	;

type_qualifier_list
	: type_qualifier						{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(TYPE_QUAL_LIST, "type_qual_list", temp); }
	| type_qualifier_list type_qualifier	{ add_children($1, $2); $$ = $1; }
	;


parameter_type_list							
	: parameter_list ',' ELLIPSIS				{ temp[0]=NULL;add_children($1, ast_node_create(ELLIPSIS_NODE, "(var_args)...", temp)); $$ = $1; }
	| parameter_list
	;

parameter_list
	: parameter_declaration						{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(PARAM_LIST, "param_list", temp); }
	| parameter_list ',' parameter_declaration	{ add_children($1, $3); $$ = $1; }
	;

parameter_declaration
	: declaration_specifiers declarator			{ temp[0]=$1;temp[1]=$2;temp[2]=NULL;$$ = ast_node_create(PARAM_DECL, "param_decl", temp); }
/*	| declaration_specifiers abstract_declarator */
	| declaration_specifiers					{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(PARAM_DECL, "param_decl", temp);}
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
*/
initializer
	: '{' initializer_list '}'				{ $$ = $2; }
	| '{' initializer_list ',' '}'			{ $$ = $2; }
	| assignment_expression					{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(INIT_ASSIGN_EXPR, "init_assign_expr", temp); }
	;

initializer_list
//	: designation initializer
	: initializer							{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(INIT_LIST, "init_list", temp); }
//	| initializer_list ',' designation initializer
	| initializer_list ',' initializer		{ add_children($1, $3); $$ = $1; }
	;
/*
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
	: IDENTIFIER ':' statement						{ temp[0]=NULL;temp[1]=ast_node_create(ID, $1, temp);temp[2]=$3;temp[3]=NULL; $$ = ast_node_create(LABELED_STMT, "labeled_stmt", temp+1); }
	| CASE constant_expression ':' statement		{ temp[0]=$2;temp[1]=$4;temp[2]=NULL;$$ = ast_node_create(CASE_STMT, "case_stmt", temp); }
	| DEFAULT ':' statement							{ temp[0]=$3;temp[1]=NULL;$$ = ast_node_create(DEF_STMT, "default_stmt", temp); }
	;

compound_statement
	: '{' '}'										{ temp[0]=NULL;$$ = ast_node_create(CMPND_STMT, "cmpnd_stmt", temp); }
	| '{'  block_item_list '}'						{ temp[0]=$2;temp[1]=NULL;$$ = ast_node_create(CMPND_STMT, "cmpnd_stmt", temp); }
	;

block_item_list
	: block_item									{ temp[0]=$1;temp[1]=NULL; $$ = ast_node_create(BLK_ITEM_LIST, "blk_item_list", temp); }
	| block_item_list block_item					{ add_children($1, $2); $$ = $1;}
	;

block_item
	: declaration
	| statement	
	;

expression_statement
	: ';'											{ temp[0]=NULL;$$ = ast_node_create(EXPR_STMT, "expr_stmt", temp); }
	| expression ';'								{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(EXPR_STMT, "expr_stmt", temp); }
	;

selection_statement
	: IF '(' expression ')' statement ELSE statement		{ temp[0]=$3;temp[1]=$5;temp[2]=$7;temp[3]=NULL;$$=ast_node_create(IF_ELSE_STMT, "if-else", temp); }
	| IF '(' expression ')' statement						{ temp[0]=$3;temp[1]=$5;temp[3]=NULL;$$=ast_node_create(IF_STMT, "if", temp); }
	| SWITCH '(' expression ')' statement					{ temp[0]=$3;temp[1]=$5;temp[3]=NULL;$$=ast_node_create(SWITCH_STMT, "switch", temp); }
	;

iteration_statement
	: WHILE '(' expression ')' statement											{ temp[0]=$3;temp[1]=$5;temp[2]=NULL;$$=ast_node_create(WHILE_STMT, "while-loop", temp); }
	| DO statement WHILE '(' expression ')' ';'										{ temp[0]=$2;temp[1]=$5;temp[2]=NULL;$$=ast_node_create(DO_WHILE_STMT, "do-while-loop", temp); }
	| FOR '(' expression_statement expression_statement ')' statement				{ temp[0]=$3;temp[1]=$4;temp[2]=$6;temp[3]=NULL;$$ = ast_node_create(FOR_STMT1, "for-loop-1", temp); }
	| FOR '(' expression_statement expression_statement expression ')' statement	{ temp[0]=$3;temp[1]=$4;temp[2]=$5;temp[3]=$7;temp[4]=NULL;$$ = ast_node_create(FOR_STMT2, "for-loop-2", temp); }
	| FOR '(' declaration expression_statement ')' statement						{ temp[0]=$3;temp[1]=$4;temp[2]=$6;temp[3]=NULL;$$ = ast_node_create(FOR_STMT3, "for-loop-3", temp); }
	| FOR '(' declaration expression_statement expression ')' statement				{ temp[0]=$3;temp[1]=$4;temp[2]=$5;temp[3]=$7;temp[4]=NULL;$$ = ast_node_create(FOR_STMT4, "for-loop-4", temp); }
	;

jump_statement
	: GOTO IDENTIFIER ';'							{ temp[0]=NULL;temp[1]=ast_node_create(ID, $2, temp);temp[2]=NULL;$$=ast_node_create(GOTO_STMT, "goto_stmt", temp+1); }
	| CONTINUE ';'									{ temp[0]=NULL;$$=ast_node_create(CONTINUE_STMT, "continue_stmt", temp); }
	| BREAK ';'										{ temp[0]=NULL;$$=ast_node_create(BREAK_STMT, "break_stmt", temp); }
	| RETURN ';'									{ temp[0]=NULL;$$=ast_node_create(RETURN_STMT1, "return_stmt1", temp); }	
	| RETURN expression ';'							{ temp[0]=$2;temp[1]=NULL;$$=ast_node_create(RETURN_STMT2, "return_stmt2", temp); }
	;


translation_unit
	: external_declaration							{ temp[0]=$1;temp[1]=NULL;$$ = ast_node_create(TRANSLATION_UNIT, "translation_unit", temp);root=$$; }
	| translation_unit external_declaration			{ add_children($1, $2); $$ = $1; }
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	/* : declaration_specifiers declarator declaration_list compound_statement	// old style definition */
	: declaration_specifiers declarator compound_statement							{ temp[0]=$1;temp[1]=$2;temp[2]=$3;temp[3]=NULL;$$ = ast_node_create(FUNCTION_DEF, "function_def", temp);}
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

struct _ast_node* get_ast_root() {
	return root;
}
