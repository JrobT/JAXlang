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
%token EOL LPAREN RPAREN
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
%type <Functions.str_cmd> str_cmd
%type <Functions.def_string> str_variable
%type <Functions.int_cmd> int_cmd
%type <Functions.def_int> int_variable
%type <Functions.bool_cmd> bool_cmd
%type <Functions.def_bool> bool_variable
%type <Functions.setAction> set_action
%type <string> set
%type <Functions.decAction> dec_action
%type <Functions.mutAction> mut_action
%type <Functions.print> print_cmd

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
 | action EOL		   		    { ActionStatement $1 }
;

for_do_done:
 | FOR SVAR COLON set DO body STOP 	{ ForEach ($2, $4, $6) }
 | FOR bool_cmd DO body STOP 	    { ForBool ($2, $4) }
;

if_then_else_fi:
 | IF bool_cmd THEN body END    	        { If ($2, $4) }                           
 | IF bool_cmd THEN body ELSE body END      { IfElse ($2, $4, $6) } 	   
;

action:
 | operation 						{ Operation $1 }
 | dec_action 						{ DecAction $1 }
 | mut_action 						{ MutAction $1 }
 | print_cmd 					    { PrintAction $1 }
;

operation:
 | set_action						{ SetAction $1 }
 | int_cmd					    	{ IntCmd $1 }
 | str_cmd 						    { StrCmd $1 }
 | bool_cmd 						{ BoolCmd $1 }
;

int_cmd:
 | LPAREN int_cmd RPAREN 		{ $2 }
 | int_variable 					    { IntOrVar $1 }
 | int_cmd PLUS int_cmd 	    { Plus ($1, $3) }
 | int_cmd MINUS int_cmd 	    { Minus ($1, $3) }
 | int_cmd TIMES int_cmd       { Times ($1, $3) }
 | int_cmd DIVIDE int_cmd      { Divide ($1, $3) }
 | int_cmd MOD int_cmd  	    { Mod ($1, $3) }
 ;

int_variable:
 | INT 								{ Int $1 }
 | IVAR 							{ Int_Idf $1 }
;

str_cmd:
 | str_variable 					    { StrOrVar $1 }	
 | str_cmd CONS str_variable 		{ Cat ($1, $3) }
;

str_variable:
 | STRING 							{ Str $1 }
 | SVAR 							{ Str_Idf $1 }
;

bool_cmd:
 | LPAREN bool_cmd RPAREN 		{ $2 }
 | bool_variable 						{ BoolOrVar $1 }
 | int_cmd LESS int_cmd 		{ Les ($1, $3) }
 | int_cmd GREATER int_cmd			{ Grt ($1, $3) }
 | int_cmd ISEQUAL int_cmd			{ IntEq ($1, $3) }
 | str_cmd ISEQUAL str_cmd 		{ StrEq ($1, $3) }
 | bool_cmd ISEQUAL bool_cmd 		{ BlEq ($1, $3) }
 | bool_cmd AND bool_cmd 		{ And ($1, $3) }
 | bool_cmd OR bool_cmd 		{ Or ($1, $3) }
;

bool_variable:
 | TRUE								{ Bool $1 }
 | FALSE							{ Bool $1 }
 | BVAR 							{ Bool_Idf $1 }
;

set_action:
 | set 								{ Set $1 }
 | set PLACE str_variable				{ SetAdd ($1, $3) }
 | set DELETE str_variable				{ SetRem ($1, $3) }
 ;

set:
 | INPUT 							{ $1 }
 | LVAR 							{ $1 }
;

dec_action:
 | VAR LVAR 						{ LVarDec $2 }
 | VAR IVAR 						{ IVarDec ($2, IntOrVar (Int 0)) }
 | VAR IVAR EQUAL int_cmd 		{ IVarDec ($2, $4) }
 | VAR SVAR  						{ SVarDec ($2, StrOrVar (Str "")) }
 | VAR SVAR EQUAL str_cmd 		{ SVarDec ($2, $4) }
 | VAR BVAR 						{ BVarDec ($2, BoolOrVar (Bool false)) }
 | VAR BVAR EQUAL bool_cmd 		{ BVarDec ($2, $4) }
;

mut_action:
 | LVAR EQUAL set_action			{ SetMut ($1, $3) }
 | IVAR EQUAL int_cmd			{ IntMut ($1, $3) }
 | SVAR EQUAL str_cmd 			    { StrMut ($1, $3) }
 | BVAR EQUAL bool_cmd 			{ BlMut ($1, $3) }
;

print_cmd:
 | PRINT operation 					{ Print $2 }
;
