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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

open Functions;;

# 50 "parser.ml"
let yytransl_const = [|
  267 (* START *);
  268 (* FINISH *);
  269 (* PLACE *);
  270 (* DELETE *);
  271 (* IF *);
  272 (* THEN *);
  273 (* ELSE *);
  274 (* END *);
  275 (* FOR *);
  276 (* COLON *);
  277 (* DO *);
  278 (* STOP *);
  279 (* PRINT *);
  280 (* VAR *);
  281 (* SEMICOLON *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* EQUAL *);
  285 (* PLUS *);
  286 (* MINUS *);
  287 (* TIMES *);
  288 (* DIVIDE *);
  289 (* MOD *);
  290 (* CONS *);
  291 (* LESS *);
  292 (* GREATER *);
  293 (* ISEQUAL *);
  294 (* NOT *);
  295 (* OR *);
  296 (* AND *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* STRING *);
  259 (* SVAR *);
  260 (* IVAR *);
  261 (* BVAR *);
  262 (* LVAR *);
  263 (* INPUT *);
  264 (* COUNT *);
  265 (* TRUE *);
  266 (* FALSE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\006\000\006\000\007\000\007\000\
\007\000\007\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\009\000\009\000\010\000\010\000\011\000\011\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\014\000\014\000\014\000\
\015\000\015\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\017\000\017\000\017\000\017\000\018\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\001\000\001\000\002\000\007\000\005\000\
\005\000\007\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\003\000\003\000\003\000\003\000\
\003\000\002\000\001\000\001\000\001\000\003\000\001\000\001\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\001\000\001\000\001\000\001\000\003\000\003\000\
\001\000\001\000\002\000\002\000\004\000\002\000\004\000\002\000\
\004\000\003\000\003\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\063\000\027\000\031\000\000\000\000\000\
\000\000\000\000\049\000\043\000\044\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\005\000\
\000\000\011\000\000\000\020\000\000\000\029\000\000\000\034\000\
\015\000\000\000\012\000\013\000\014\000\000\000\000\000\000\000\
\000\000\032\000\028\000\045\000\000\000\000\000\000\000\000\000\
\000\000\050\000\062\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\026\000\042\000\001\000\003\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\058\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\033\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\000\000\000\000\030\000\000\000\039\000\000\000\000\000\
\047\000\048\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\008\000\000\000\000\000\010\000\007\000"

let yydgoto = "\002\000\
\004\000\021\000\022\000\023\000\024\000\025\000\026\000\045\000\
\028\000\046\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000"

let yysindex = "\011\000\
\006\255\000\000\117\255\000\000\000\000\000\000\249\254\001\255\
\010\255\022\255\000\000\000\000\000\000\157\255\187\255\147\255\
\079\000\157\255\000\255\157\255\041\255\117\255\000\000\000\000\
\056\255\000\000\037\000\000\000\225\254\000\000\037\255\000\000\
\000\000\065\255\000\000\000\000\000\000\101\255\000\255\157\255\
\131\255\000\000\000\000\000\000\037\000\225\254\004\255\039\255\
\244\254\000\000\000\000\055\255\058\255\062\255\000\000\173\255\
\069\255\000\255\000\000\000\000\000\000\000\000\000\000\000\255\
\000\255\000\255\000\255\000\255\000\255\000\255\000\255\101\255\
\101\255\157\255\157\255\157\255\101\255\101\255\061\255\149\255\
\037\255\000\000\117\255\131\255\117\255\101\255\000\255\157\255\
\000\000\000\000\048\000\098\255\098\255\064\255\064\255\000\000\
\149\255\149\255\149\255\000\000\061\255\000\000\037\255\226\254\
\000\000\000\000\127\255\093\255\094\255\061\255\149\255\037\255\
\117\255\000\000\117\255\000\000\121\255\113\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\233\254\139\255\
\008\255\009\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\255\000\000\000\000\
\000\000\000\000\138\255\000\000\140\255\000\000\159\255\000\000\
\000\000\161\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\000\000\000\000\000\000\168\255\169\255\174\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\176\255\186\255\
\189\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\225\255\247\255\191\255\208\255\000\000\
\033\255\254\255\018\000\000\000\025\000\000\000\073\255\086\255\
\000\000\000\000\000\000\000\000\000\000\190\255\194\255\207\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\242\255\000\000\000\000\000\000\000\000\126\000\253\255\
\000\000\002\000\056\000\017\000\000\000\105\000\114\000\000\000\
\000\000\000\000"

let yytablesize = 341
let yytable = "\027\000\
\005\000\032\000\072\000\043\000\029\000\073\000\074\000\062\000\
\085\000\076\000\032\000\001\000\027\000\032\000\056\000\059\000\
\003\000\029\000\027\000\083\000\038\000\050\000\050\000\029\000\
\074\000\058\000\075\000\076\000\039\000\019\000\047\000\049\000\
\045\000\050\000\057\000\080\000\060\000\040\000\032\000\079\000\
\074\000\032\000\075\000\076\000\045\000\002\000\045\000\045\000\
\035\000\041\000\002\000\002\000\061\000\035\000\091\000\002\000\
\081\000\035\000\084\000\035\000\092\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\107\000\035\000\109\000\035\000\
\035\000\074\000\101\000\075\000\076\000\077\000\078\000\027\000\
\063\000\027\000\086\000\111\000\029\000\087\000\029\000\110\000\
\041\000\088\000\102\000\103\000\104\000\041\000\072\000\090\000\
\068\000\041\000\117\000\041\000\118\000\040\000\006\000\042\000\
\112\000\074\000\040\000\075\000\076\000\027\000\040\000\027\000\
\040\000\115\000\029\000\116\000\029\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\040\000\012\000\013\000\100\000\
\066\000\067\000\068\000\014\000\105\000\106\000\120\000\015\000\
\050\000\011\000\119\000\016\000\017\000\051\000\018\000\113\000\
\114\000\082\000\019\000\005\000\006\000\042\000\043\000\044\000\
\050\000\011\000\020\000\012\000\013\000\005\000\006\000\042\000\
\043\000\044\000\016\000\028\000\017\000\012\000\013\000\028\000\
\028\000\028\000\028\000\028\000\018\000\028\000\028\000\028\000\
\019\000\064\000\065\000\066\000\067\000\068\000\018\000\018\000\
\020\000\046\000\019\000\005\000\006\000\048\000\043\000\044\000\
\054\000\052\000\020\000\012\000\013\000\108\000\056\000\089\000\
\060\000\064\000\065\000\066\000\067\000\068\000\023\000\069\000\
\070\000\071\000\059\000\023\000\018\000\061\000\055\000\023\000\
\019\000\023\000\053\000\023\000\023\000\023\000\023\000\024\000\
\020\000\023\000\023\000\023\000\024\000\023\000\023\000\057\000\
\024\000\000\000\024\000\000\000\024\000\024\000\024\000\024\000\
\021\000\000\000\024\000\024\000\024\000\021\000\024\000\024\000\
\000\000\021\000\000\000\021\000\000\000\021\000\021\000\000\000\
\000\000\000\000\000\000\021\000\021\000\021\000\022\000\021\000\
\021\000\000\000\000\000\022\000\000\000\036\000\000\000\022\000\
\000\000\022\000\036\000\022\000\022\000\000\000\036\000\000\000\
\036\000\022\000\022\000\022\000\000\000\022\000\022\000\000\000\
\000\000\037\000\036\000\000\000\036\000\036\000\037\000\000\000\
\038\000\000\000\037\000\000\000\037\000\038\000\000\000\000\000\
\000\000\038\000\000\000\038\000\000\000\000\000\037\000\000\000\
\037\000\037\000\000\000\000\000\000\000\038\000\000\000\038\000\
\038\000\064\000\065\000\066\000\067\000\068\000\000\000\069\000\
\070\000\071\000\089\000\000\000\064\000\065\000\066\000\067\000\
\068\000\052\000\053\000\054\000\055\000"

let yycheck = "\003\000\
\001\001\025\001\034\001\004\001\003\000\037\001\037\001\022\000\
\021\001\040\001\034\001\001\000\016\000\037\001\018\000\019\000\
\011\001\016\000\022\000\016\001\028\001\013\001\014\001\022\000\
\037\001\026\001\039\001\040\001\028\001\030\001\014\000\015\000\
\025\001\025\001\018\000\039\000\020\000\028\001\034\001\038\000\
\037\001\037\001\039\001\040\001\037\001\012\001\039\001\040\001\
\016\001\028\001\017\001\018\001\012\001\021\001\058\000\022\001\
\040\000\025\001\020\001\027\001\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\083\000\037\001\085\000\039\001\
\040\001\037\001\073\000\039\001\040\001\013\001\014\001\083\000\
\025\001\085\000\028\001\087\000\083\000\028\001\085\000\086\000\
\016\001\028\001\074\000\075\000\076\000\021\001\034\001\027\001\
\033\001\025\001\113\000\027\001\115\000\016\001\002\001\003\001\
\088\000\037\001\021\001\039\001\040\001\113\000\025\001\115\000\
\027\001\021\001\113\000\022\001\115\000\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\039\001\009\001\010\001\072\000\
\031\001\032\001\033\001\015\001\077\000\078\000\022\001\019\001\
\006\001\007\001\018\001\023\001\024\001\016\000\026\001\017\001\
\018\001\041\000\030\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\038\001\009\001\010\001\001\001\002\001\003\001\
\004\001\005\001\025\001\025\001\025\001\009\001\010\001\029\001\
\030\001\031\001\032\001\033\001\026\001\035\001\036\001\037\001\
\030\001\029\001\030\001\031\001\032\001\033\001\026\001\025\001\
\038\001\025\001\030\001\001\001\002\001\003\001\004\001\005\001\
\025\001\025\001\038\001\009\001\010\001\084\000\025\001\027\001\
\025\001\029\001\030\001\031\001\032\001\033\001\016\001\035\001\
\036\001\037\001\025\001\021\001\026\001\025\001\025\001\025\001\
\030\001\027\001\025\001\029\001\030\001\031\001\032\001\016\001\
\038\001\035\001\036\001\037\001\021\001\039\001\040\001\025\001\
\025\001\255\255\027\001\255\255\029\001\030\001\031\001\032\001\
\016\001\255\255\035\001\036\001\037\001\021\001\039\001\040\001\
\255\255\025\001\255\255\027\001\255\255\029\001\030\001\255\255\
\255\255\255\255\255\255\035\001\036\001\037\001\016\001\039\001\
\040\001\255\255\255\255\021\001\255\255\016\001\255\255\025\001\
\255\255\027\001\021\001\029\001\030\001\255\255\025\001\255\255\
\027\001\035\001\036\001\037\001\255\255\039\001\040\001\255\255\
\255\255\016\001\037\001\255\255\039\001\040\001\021\001\255\255\
\016\001\255\255\025\001\255\255\027\001\021\001\255\255\255\255\
\255\255\025\001\255\255\027\001\255\255\255\255\037\001\255\255\
\039\001\040\001\255\255\255\255\255\255\037\001\255\255\039\001\
\040\001\029\001\030\001\031\001\032\001\033\001\255\255\035\001\
\036\001\037\001\027\001\255\255\029\001\030\001\031\001\032\001\
\033\001\003\001\004\001\005\001\006\001"

let yynames_const = "\
  START\000\
  FINISH\000\
  PLACE\000\
  DELETE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  END\000\
  FOR\000\
  COLON\000\
  DO\000\
  STOP\000\
  PRINT\000\
  VAR\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  EQUAL\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  CONS\000\
  LESS\000\
  GREATER\000\
  ISEQUAL\000\
  NOT\000\
  OR\000\
  AND\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  SVAR\000\
  IVAR\000\
  BVAR\000\
  LVAR\000\
  INPUT\000\
  COUNT\000\
  TRUE\000\
  FALSE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 60 "parser.mly"
                            ( Body _2 )
# 322 "parser.ml"
               : Functions.mainTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.statement) in
    Obj.repr(
# 64 "parser.mly"
                                 ( SingleStatement _1 )
# 329 "parser.ml"
               : Functions.body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Functions.statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.body) in
    Obj.repr(
# 65 "parser.mly"
                                 ( MultiStatement (_1, _2) )
# 337 "parser.ml"
               : Functions.body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.forDo) in
    Obj.repr(
# 69 "parser.mly"
                               ( ForStatement _1 )
# 344 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.ifElse) in
    Obj.repr(
# 70 "parser.mly"
                                    ( IfStatement _1 )
# 351 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Functions.action) in
    Obj.repr(
# 71 "parser.mly"
                              ( ActionStatement _1 )
# 358 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 75 "parser.mly"
                                    ( ForEach (_2, _4, _6) )
# 367 "parser.ml"
               : Functions.forDo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Functions.boolAction) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 76 "parser.mly"
                                 ( ForBool (_2, _4) )
# 375 "parser.ml"
               : Functions.forDo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Functions.boolAction) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 80 "parser.mly"
                                         ( If (_2, _4) )
# 383 "parser.ml"
               : Functions.ifElse))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Functions.boolAction) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Functions.body) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 81 "parser.mly"
                                          ( IfElse (_2, _4, _6) )
# 392 "parser.ml"
               : Functions.ifElse))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.operation) in
    Obj.repr(
# 85 "parser.mly"
                   ( Operation _1 )
# 399 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.decAction) in
    Obj.repr(
# 86 "parser.mly"
                    ( DecAction _1 )
# 406 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.mutAction) in
    Obj.repr(
# 87 "parser.mly"
                    ( MutAction _1 )
# 413 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.print) in
    Obj.repr(
# 88 "parser.mly"
                     ( PrintAction _1 )
# 420 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.setAction) in
    Obj.repr(
# 92 "parser.mly"
                   ( SetAction _1 )
# 427 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 93 "parser.mly"
                   ( IntAction _1 )
# 434 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.strAction) in
    Obj.repr(
# 94 "parser.mly"
                    ( StrAction _1 )
# 441 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 95 "parser.mly"
                     ( BoolAction _1 )
# 448 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.intAction) in
    Obj.repr(
# 99 "parser.mly"
                              ( _2 )
# 455 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_var) in
    Obj.repr(
# 100 "parser.mly"
                      ( IntOrVar _1 )
# 462 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 101 "parser.mly"
                                   ( Plus (_1, _3) )
# 470 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 102 "parser.mly"
                                    ( Minus (_1, _3) )
# 478 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 103 "parser.mly"
                                     ( Times (_1, _3) )
# 486 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 104 "parser.mly"
                                     ( Divide (_1, _3) )
# 494 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 105 "parser.mly"
                                   ( Mod (_1, _3) )
# 502 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 106 "parser.mly"
                                    ( Uminus _2 )
# 509 "parser.ml"
               : Functions.intAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 110 "parser.mly"
               ( Int _1 )
# 516 "parser.ml"
               : Functions.int_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
               ( IntVar _1 )
# 523 "parser.ml"
               : Functions.int_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_var) in
    Obj.repr(
# 115 "parser.mly"
                      ( StrOrVar _1 )
# 530 "parser.ml"
               : Functions.strAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.strAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_var) in
    Obj.repr(
# 116 "parser.mly"
                               ( Cat (_1, _3) )
# 538 "parser.ml"
               : Functions.strAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                 ( Str _1 )
# 545 "parser.ml"
               : Functions.str_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
               ( StrVar _1 )
# 552 "parser.ml"
               : Functions.str_var))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.boolAction) in
    Obj.repr(
# 125 "parser.mly"
                               ( _2 )
# 559 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_var) in
    Obj.repr(
# 126 "parser.mly"
                    ( BoolOrVar _1 )
# 566 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 127 "parser.mly"
                                ( Les (_1, _3) )
# 574 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 128 "parser.mly"
                                   ( Grt (_1, _3) )
# 582 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.intAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 129 "parser.mly"
                                   ( IntEq (_1, _3) )
# 590 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.strAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.strAction) in
    Obj.repr(
# 130 "parser.mly"
                                   ( StrEq (_1, _3) )
# 598 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.boolAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 131 "parser.mly"
                                     ( BlEq (_1, _3) )
# 606 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.boolAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 132 "parser.mly"
                                 ( And (_1, _3) )
# 614 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.boolAction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 133 "parser.mly"
                                ( Or (_1, _3) )
# 622 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 134 "parser.mly"
                        ( Not _2 )
# 629 "parser.ml"
               : Functions.boolAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 138 "parser.mly"
               ( Bool _1 )
# 636 "parser.ml"
               : Functions.bool_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 139 "parser.mly"
               ( Bool _1 )
# 643 "parser.ml"
               : Functions.bool_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 140 "parser.mly"
               ( BoolVar _1 )
# 650 "parser.ml"
               : Functions.bool_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 144 "parser.mly"
               ( Set _1 )
# 657 "parser.ml"
               : Functions.setAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_var) in
    Obj.repr(
# 145 "parser.mly"
                          ( SetAdd (_1, _3) )
# 665 "parser.ml"
               : Functions.setAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_var) in
    Obj.repr(
# 146 "parser.mly"
                           ( SetRem (_1, _3) )
# 673 "parser.ml"
               : Functions.setAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 150 "parser.mly"
                ( _1 )
# 680 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 151 "parser.mly"
               ( _1 )
# 687 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
                  ( LVarDec _2 )
# 694 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                  ( IVarDec (_2, IntOrVar (Int 0)) )
# 701 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 157 "parser.mly"
                               ( IVarDec (_2, _4) )
# 709 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                   ( SVarDec (_2, StrOrVar (Str "")) )
# 716 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.strAction) in
    Obj.repr(
# 159 "parser.mly"
                               ( SVarDec (_2, _4) )
# 724 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "parser.mly"
                  ( BVarDec (_2, BoolOrVar (Bool false)) )
# 731 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 161 "parser.mly"
                                ( BVarDec (_2, _4) )
# 739 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.setAction) in
    Obj.repr(
# 165 "parser.mly"
                           ( SetMut (_1, _3) )
# 747 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.intAction) in
    Obj.repr(
# 166 "parser.mly"
                           ( IntMut (_1, _3) )
# 755 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.strAction) in
    Obj.repr(
# 167 "parser.mly"
                            ( StrMut (_1, _3) )
# 763 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.boolAction) in
    Obj.repr(
# 168 "parser.mly"
                             ( BlMut (_1, _3) )
# 771 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.operation) in
    Obj.repr(
# 172 "parser.mly"
                        ( Print _2 )
# 778 "parser.ml"
               : Functions.print))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Functions.mainTree)
