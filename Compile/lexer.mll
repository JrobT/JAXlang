(* File lexer.mll, by Alex Everett & Jack Trute *)

{

open Parser         (* parser.mli *)
exception EOF       (* End_of_file exception *)

}

let blank = [' ''\t''\n']
let digits = ['0'-'9']+ as lxm
let possibleSymbols = ['a'-'z''A'-'Z''0'-'9']
let possibleStrings = ['"']([' ']*possibleSymbols*)*['"']
let comment = "**" [^'|''*']* "**"

rule lexer_main = parse
    | possibleStrings as str_id                 { STRING(List.nth (Str.split_delim (Str.regexp "\"") str_id) 1) }
    | blank                                     { lexer_main lexbuf }
    | digits                                    { INT (int_of_string lxm) }
    | comment                                   { lexer_main lexbuf }
    | "start"                                   { START }
    | "fi"                                      { FINISH }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | ';'                                       { EOL }
    | '+'                                       { PLUS }
    | '-'                                       { MINUS }
    | '*'                                       { TIMES }
    | '/'                                       { DIVIDE }
    | '%'                                       { MOD }
    | '='                                       { EQUAL }
    | '&'                                       { CONS }
    | "true"                                    { TRUE true }
    | "false"                                   { FALSE false }
    | "=="                                      { ISEQUAL }
    | '<'                                       { LESS }
    | '>'                                       { GREATER }
    | "||"                                      { OR }
    | "&&"                                      { AND }
    | '!'                                       { NOT }
    | "if"                                      { IF }
    | "then"                                    { THEN }    
    | "else"                                    { ELSE }
    | "end"                                     { END }
    | "for"                                     { FOR }
    | ':'                                       { COLON } (*for each ; IN*)
    | "do"                                      { DO }
    | "loop"                                    { STOP }
    | "print"                                   { PRINT }
    | "var"                                     { VAR }
    | "_empty"                                  { STRING ""} (*empty string*)
    | '#'possibleSymbols+ as int_var_id         { IVAR( int_var_id ) }
    | '@'possibleSymbols+ as str_var_id         { SVAR( str_var_id ) }
    | '?'possibleSymbols+ as bl_var_id          { BVAR( bl_var_id ) }
    | '$'possibleSymbols+  as set               { LVAR set }
    | "$in"['0'-'9']+ as input                  { INPUT input  }
    | "_output_count" as output_stuff           { COUNT output_stuff }
    | "place"                                   { PLACE }
    | "del"                                     { DELETE }
    | eof                                       { raise EOF }
