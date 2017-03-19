/* File parser.mly, by Alex Everett & Jack Trute */

%{

open Functions;;

%}

%token <int> INT
%token <string> STRING
%token <string>  SVAR IVAR BVAR LVAR
%token <string> INPUT
%token <string> COUNT
%token <bool> TRUE
%token <bool> FALSE
%token START FINISH
%token PLACE DELETE
%token IF THEN ELSE END
%token FOR COLON DO STOP
%token PRINT
%token VAR
%token SEMICOLON LPAREN RPAREN
%token EQUAL
%token PLUS MINUS TIMES DIVIDE MOD
%token CONS
%token LESS GREATER ISEQUAL NOT OR AND
%right OR
%right AND
%left ISEQUAL
%left LESS GREATER
%right CONS
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD
%nonassoc NOT UMINUS

%start main             
%type <Functions.mainTree> main
%type <Functions.body> body
%type <Functions.statement> statement
%type <Functions.forDo> for_do_done
%type <Functions.ifElse> if_then_else_fi
%type <Functions.action> action
%type <Functions.operation> operation
%type <Functions.intAction> int_action
%type <Functions.int_var> intOrIVar
%type <Functions.strAction> str_action
%type <Functions.str_var> strOrSVar
%type <Functions.boolAction> bool_action
%type <Functions.bool_var> boolOrBVar
%type <Functions.setAction> set_action
%type <string> set
%type <Functions.decAction> dec_action
%type <Functions.mutAction> mut_action
%type <Functions.print> print_action

%%

main:
 | START body FINISH 			    { Body $2 }
;

body:
 | statement                    	{ SingleStatement $1 }
 | statement body               	{ MultiStatement ($1, $2) }
;

statement:
 | for_do_done               		{ ForStatement $1 }
 | if_then_else_fi                 	{ IfStatement $1 }   
 | action SEMICOLON		   		    { ActionStatement $1 }
;

for_do_done:
 | FOR SVAR COLON set DO body STOP 	{ ForEach ($2, $4, $6) }
 | FOR bool_action DO body STOP 	{ ForBool ($2, $4) }
;

if_then_else_fi:
 | IF bool_action THEN body END    	     { If ($2, $4) }                           
 | IF bool_action THEN body ELSE body END { IfElse ($2, $4, $6) } 	   
;

action:
 | operation 						{ Operation $1 }
 | dec_action 						{ DecAction $1 }
 | mut_action 						{ MutAction $1 }
 | print_action 					{ PrintAction $1 }
;

operation:
 | set_action						{ SetAction $1 }
 | int_action						{ IntAction $1 }
 | str_action 						{ StrAction $1 }
 | bool_action 						{ BoolAction $1 }
;

int_action:
 | LPAREN int_action RPAREN 		{ $2 }
 | intOrIVar 					    { IntOrVar $1 }
 | int_action PLUS int_action 	    { Plus ($1, $3) }
 | int_action MINUS int_action 	    { Minus ($1, $3) }
 | int_action TIMES int_action       { Times ($1, $3) }
 | int_action DIVIDE int_action      { Divide ($1, $3) }
 | int_action MOD int_action  	    { Mod ($1, $3) }
 | MINUS int_action %prec UMINUS    { Uminus $2 }
;

intOrIVar:
 | INT 								{ Int $1 }
 | IVAR 							{ IntVar $1 }
;

str_action:
 | strOrSVar 					    { StrOrVar $1 }	
 | str_action CONS strOrSVar 		{ Cat ($1, $3) }
;

strOrSVar:
 | STRING 							{ Str $1 }
 | SVAR 							{ StrVar $1 }
;

bool_action:
 | LPAREN bool_action RPAREN 		{ $2 }
 | boolOrBVar 						{ BoolOrVar $1 }
 | int_action LESS int_action 		{ Les ($1, $3) }
 | int_action GREATER int_action			{ Grt ($1, $3) }
 | int_action ISEQUAL int_action			{ IntEq ($1, $3) }
 | str_action ISEQUAL str_action 		{ StrEq ($1, $3) }
 | bool_action ISEQUAL bool_action 		{ BlEq ($1, $3) }
 | bool_action AND bool_action 		{ And ($1, $3) }
 | bool_action OR bool_action 		{ Or ($1, $3) }
 | NOT bool_action 					{ Not $2 }
;

boolOrBVar:
 | TRUE								{ Bool $1 }
 | FALSE							{ Bool $1 }
 | BVAR 							{ BoolVar $1 }
;

set_action:
 | set 								{ Set $1 }
 | set PLACE strOrSVar				{ SetAdd ($1, $3) }
 | set DELETE strOrSVar				{ SetRem ($1, $3) }
 ;

set:
 | INPUT 							{ $1 }
 | LVAR 							{ $1 }
;

dec_action:
 | VAR LVAR 						{ LVarDec $2 }
 | VAR IVAR 						{ IVarDec ($2, IntOrVar (Int 0)) }
 | VAR IVAR EQUAL int_action 		{ IVarDec ($2, $4) }
 | VAR SVAR  						{ SVarDec ($2, StrOrVar (Str "")) }
 | VAR SVAR EQUAL str_action 		{ SVarDec ($2, $4) }
 | VAR BVAR 						{ BVarDec ($2, BoolOrVar (Bool false)) }
 | VAR BVAR EQUAL bool_action 		{ BVarDec ($2, $4) }
;

mut_action:
 | LVAR EQUAL set_action			{ SetMut ($1, $3) }
 | IVAR EQUAL int_action			{ IntMut ($1, $3) }
 | SVAR EQUAL str_action 			{ StrMut ($1, $3) }
 | BVAR EQUAL bool_action 			{ BlMut ($1, $3) }
;

print_action:
 | PRINT operation 					{ Print $2 }
;
