type token =
  | INT of (int)
  | STRING of (string)
  | SVAR of (string)
  | IVAR of (string)
  | BVAR of (string)
  | LVAR of (string)
  | INPUT of (string)
  | COUNT of (string)
  | TRUE of (bool)
  | FALSE of (bool)
  | START
  | FINISH
  | PLACE
  | DELETE
  | IF
  | THEN
  | ELSE
  | END
  | FOR
  | COLON
  | DO
  | STOP
  | PRINT
  | VAR
  | SEMICOLON
  | LPAREN
  | RPAREN
  | EQUAL
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | CONS
  | LESS
  | GREATER
  | ISEQUAL
  | NOT
  | OR
  | AND

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Functions.mainTree
