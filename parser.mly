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

%start parser_main

%type <Functions.functions_main> parser_main
%type <Functions.body> body
%type <Functions.statement> statement
%type <Functions.forDo> for_do_statement
%type <Functions.ifElse> if_else_then
%type <Functions.line> line
%type <Functions.operation> operation
%type <Functions.intAction> int_action
%type <Functions.parseMyInt> parsedInt
%type <Functions.strAction> str_action
%type <Functions.parseMyStr> parsedStr
%type <Functions.boolAction> bool_action
%type <Functions.parseMyBool> parsedBool
%type <Functions.setStuff> set_line
%type <string> set
%type <Functions.decline> dec_line
%type <Functions.mutline> mut_line
%type <Functions.print> print_action

%%

parser_main:
 | START body FINISH 				{ Body $2 }
;

body:
 | statement                    	{ SingleStatement $1 }
 | statement body               	{ Multiline ($1, $2) }
;

statement:
 | for_do_statement                 { ForStatement $1 }
 | if_else_then                     { IfStatement $1 }   
 | line SEMICOLON		   		    { LineStatement $1 }
;

for_do_statement:
 | FOR SVAR COLON set DO body STOP 	{ ForEach ($2, $4, $6) }
 | FOR bool_action DO body STOP 	    { TrueOrFalse ($2, $4) }
;

if_else_then:
 | IF bool_action THEN body END    	        { If ($2, $4) }                           
 | IF bool_action THEN body ELSE body END     { IfElse ($2, $4, $6) } 	   
;

line:
 | operation 						{ Operation $1 }
 | dec_line 						{ Decline $1 }
 | mut_line 						{ Mutline $1 }
 | print_action 					    { Printline $1 }
;

operation:
 | set_line						    { SetStuff $1 }
 | int_action						{ IntAction $1 }
 | str_action 						{ StrAction $1 }
 | bool_action 						{ BoolAction $1 }
;

int_action:
 | LPAREN int_action RPAREN 		    { $2 }
 | parsedInt 					    { IntWithName $1 }
 | int_action PLUS int_action 	        { Plus ($1, $3) }
 | int_action MINUS int_action 	        { Minus ($1, $3) }
 | int_action TIMES int_action          { Times ($1, $3) }
 | int_action DIVIDE int_action         { Divide ($1, $3) }
 | int_action MOD int_action  	        { Mod ($1, $3) }
 | UMINUS int_action %prec UMINUS     { Uminus $2 }
;

parsedInt:
 | INT 								{ Int $1 }
 | IVAR 							{ IntVar $1 }
;

str_action:
 | parsedStr 					    { StrWithName $1 }	
 | str_action CONS parsedStr 		    { Cons ($1, $3) }
;

parsedStr:
 | STRING 							{ Str $1 }
 | SVAR 							{ StrVar $1 }
;

bool_action:
 | LPAREN bool_action RPAREN 		        { $2 }
 | parsedBool 						    { BoolWithName $1 }
 | int_action LESS int_action 		        { Less ($1, $3) }
 | int_action GREATER int_action			{ Greater ($1, $3) }
 | int_action ISEQUAL int_action			{ IntEql ($1, $3) }
 | str_action ISEQUAL str_action 		    { StrEql ($1, $3) }
 | bool_action ISEQUAL bool_action 		    { BlEql ($1, $3) }
 | bool_action OR bool_action 		        { Or ($1, $3) }
 | bool_action AND bool_action 		        { And ($1, $3) }
 | NOT bool_action 					    { Not $2 }
;

parsedBool:
 | TRUE								    { Bool $1 }
 | FALSE							    { Bool $1 }
 | BVAR 							    { BoolVar $1 }
;

set_line:
 | set 								    { Set $1 }
 | set PLACE parsedStr				    { SetPlace ($1, $3) }
 | set DELETE parsedStr				    { SetDelete ($1, $3) }
 ;

set:
 | INPUT 							    { $1 }
 | LVAR 							    { $1 }
;

dec_line:
 | VAR LVAR 						    { LVarDec $2 }
 | VAR IVAR 						    { IVarDec ($2, IntWithName (Int 0)) }
 | VAR IVAR EQUAL int_action 		        { IVarDec ($2, $4) }
 | VAR SVAR  						    { SVarDec ($2, StrWithName (Str "")) }
 | VAR SVAR EQUAL str_action 		        { SVarDec ($2, $4) }
 | VAR BVAR 						    { BVarDec ($2, BoolWithName (Bool false)) }
 | VAR BVAR EQUAL bool_action 		    { BVarDec ($2, $4) }
;

mut_line:
 | LVAR EQUAL set_line			        { SetMut ($1, $3) }
 | IVAR EQUAL int_action			        { IntMut ($1, $3) }
 | SVAR EQUAL str_action 			        { StrMut ($1, $3) }
 | BVAR EQUAL bool_action 			    { BlMut ($1, $3) }
;

print_action:
 | PRINT operation 					    { Print $2 }
;
