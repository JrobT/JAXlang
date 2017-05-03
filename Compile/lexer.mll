(* File lexer.mll, by Alex Everett & Jack Trute *)

{

open Parser         (* parser.mli *)
exception EOF       (* End_of_file exception *)

}

let blank = [' ''\t''\n']               (* skip all blank characters *)
let digits = ['0'-'9']+ as lxm          (* parse all the digits *)
let possibleSymbols = ['a'-'z''A'-'Z''0'-'9']
let possibleStrings = ['"']([' ']*possibleSymbols*)*['"'] (*speech marks around string*)
let comment = "**" [^'|''*']* "**"

rule lexer_main = parse
    | possibleStrings as str                    { STRING(List.nth (Str.split_delim (Str.regexp "\"") str) 1) }                          (* remove characters *)
    | blank                                     { lexer_main lexbuf }
    | digits                                    { INT (int_of_string lxm) }
    | comment                                   { lexer_main lexbuf }
    | "start"                                   { START }
    | "fi"                                      { FINISH }
    | "_output_count" as output                 { NUMBER output } (*Doesn't work yet*) 
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | ';'                                       { EOL }
    | '+'                                       { PLUS }
    | '-'                                       { MINUS }
    | '*'                                       { TIMES }
    | '/'                                       { DIVIDE }
    | '='                                       { EQUAL }
    | '&'                                       { CONS }
    | "true"                                    { TRUE true }
    | "false"                                   { FALSE false }
    | "=="                                      { ISEQUAL }
    | '<'                                       { LESS }
    | '>'                                       { GREATER }
    | "||"                                      { OR }
    | "&&"                                      { AND }
    | "if"                                      { IF }
    | "then"                                    { THEN }    
    | "else"                                    { ELSE }
    | "end"                                     { END }
    | "for"                                     { FOR }
    | "in"                                      { IN } (*for each ; IN*)
    | "do"                                      { DO }
    | "loop"                                    { STOP }
    | "print"                                   { PRINT }
    | "var"                                     { VAR }
    | ':'                                       { STRING ""} (*empty string*)
    | "$in"['0'-'9']+ as input                  { INPUT input  }
    | "place"                                   { PLACE }
    | "del"                                     { DELETE }
    | '@'possibleSymbols+ as varStr             { SVAR( varStr ) }
    | '#'possibleSymbols+ as varInt             { IVAR( varInt ) }
    | '?'possibleSymbols+ as varBool            { BVAR( varBool ) }
    | '&'possibleSymbols+ as varSet             { LVAR varSet }
    | eof                                       { raise EOF }
