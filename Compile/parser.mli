type token =
  | INT of (int)
  | STRING of (string)
  | SVAR of (string)
  | IVAR of (string)
  | BVAR of (string)
  | LVAR of (string)
  | INPUT of (string)
  | NUMBER of (string)
  | TRUE of (bool)
  | FALSE of (bool)
  | START
  | FINISH
  | FOR
  | IN
  | DO
  | STOP
  | PLACE
  | DELETE
  | IF
  | THEN
  | ELSE
  | END
  | PRINT
  | VAR
  | EOL
  | LPAREN
  | RPAREN
  | EQUAL
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | CONS
  | LESS
  | GREATER
  | ISEQUAL
  | NOT
  | OR
  | AND

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Functions.main
