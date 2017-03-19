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
  | EOL
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
  281 (* EOL *);
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
\007\000\007\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\008\000\008\000\009\000\009\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\013\000\013\000\013\000\014\000\014\000\014\000\015\000\015\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\017\000\
\017\000\017\000\017\000\018\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\001\000\001\000\002\000\007\000\005\000\
\005\000\007\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\003\000\003\000\003\000\003\000\
\003\000\001\000\001\000\001\000\003\000\001\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\001\000\
\002\000\002\000\004\000\002\000\004\000\002\000\004\000\003\000\
\003\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\061\000\026\000\030\000\000\000\000\000\
\000\000\000\000\047\000\041\000\042\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\000\000\011\000\
\000\000\028\000\000\000\020\000\000\000\033\000\015\000\000\000\
\012\000\013\000\014\000\000\000\000\000\000\000\000\000\031\000\
\027\000\043\000\000\000\000\000\000\000\000\000\000\000\048\000\
\060\000\000\000\000\000\000\000\049\000\000\000\000\000\001\000\
\003\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\056\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\032\000\029\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\038\000\000\000\000\000\045\000\046\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\008\000\
\000\000\000\000\010\000\007\000"

let yydgoto = "\002\000\
\004\000\019\000\020\000\021\000\022\000\023\000\024\000\043\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000"

let yysindex = "\003\000\
\018\255\000\000\017\255\000\000\000\000\000\000\003\255\011\255\
\043\255\045\255\000\000\000\000\000\000\126\255\136\255\116\255\
\051\000\126\255\063\255\017\255\000\000\000\000\030\255\000\000\
\231\254\000\000\011\000\000\000\224\254\000\000\000\000\035\255\
\000\000\000\000\000\000\086\255\002\255\126\255\127\255\000\000\
\000\000\000\000\231\254\011\000\014\255\057\255\111\255\000\000\
\000\000\053\255\055\255\077\255\000\000\002\000\254\254\000\000\
\000\000\000\000\086\255\086\255\002\255\002\255\002\255\002\255\
\002\255\002\255\002\255\002\255\126\255\126\255\126\255\086\255\
\086\255\109\255\002\255\020\000\224\254\000\000\017\255\127\255\
\017\255\086\255\002\255\126\255\000\000\000\000\000\000\109\255\
\195\255\195\255\058\255\058\255\000\000\020\000\020\000\020\000\
\000\000\224\254\005\255\000\000\000\000\223\255\176\255\090\255\
\125\255\109\255\020\000\224\254\017\255\000\000\017\255\000\000\
\138\255\132\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\255\239\255\
\019\255\099\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\092\255\000\000\000\000\000\000\000\000\
\144\255\000\000\146\255\000\000\148\255\000\000\000\000\154\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\255\000\000\000\000\
\000\000\161\255\163\255\165\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\255\000\000\171\255\178\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\255\
\162\255\179\255\128\255\145\255\000\000\196\255\204\255\221\255\
\000\000\001\000\224\255\000\000\000\000\000\000\000\000\000\000\
\000\000\180\255\182\255\185\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\237\255\000\000\000\000\000\000\000\000\195\000\253\255\
\199\255\032\000\000\000\252\255\000\000\174\000\142\000\000\000\
\000\000\000\000"

let yytablesize = 313
let yytable = "\025\000\
\057\000\087\000\005\000\001\000\069\000\041\000\070\000\071\000\
\059\000\045\000\047\000\060\000\025\000\055\000\100\000\101\000\
\025\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\086\000\012\000\013\000\075\000\003\000\079\000\036\000\014\000\
\074\000\077\000\069\000\015\000\070\000\071\000\037\000\016\000\
\017\000\069\000\018\000\043\000\071\000\044\000\044\000\072\000\
\073\000\054\000\069\000\031\000\070\000\071\000\058\000\043\000\
\088\000\043\000\043\000\103\000\031\000\105\000\037\000\031\000\
\097\000\098\000\099\000\037\000\076\000\044\000\038\000\037\000\
\039\000\037\000\056\000\025\000\080\000\025\000\106\000\108\000\
\082\000\031\000\083\000\037\000\031\000\037\000\037\000\006\000\
\040\000\113\000\065\000\114\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\044\000\044\000\044\000\002\000\
\084\000\025\000\102\000\025\000\002\000\002\000\111\000\048\000\
\048\000\002\000\107\000\044\000\005\000\006\000\040\000\041\000\
\042\000\048\000\011\000\048\000\012\000\013\000\005\000\006\000\
\040\000\041\000\042\000\081\000\048\000\011\000\012\000\013\000\
\005\000\006\000\046\000\041\000\042\000\018\000\059\000\023\000\
\012\000\013\000\112\000\069\000\023\000\070\000\071\000\018\000\
\023\000\116\000\023\000\115\000\023\000\023\000\023\000\023\000\
\024\000\018\000\023\000\023\000\023\000\024\000\023\000\023\000\
\017\000\024\000\016\000\024\000\018\000\024\000\024\000\024\000\
\024\000\021\000\044\000\024\000\024\000\024\000\021\000\024\000\
\024\000\052\000\021\000\050\000\021\000\054\000\021\000\021\000\
\109\000\110\000\022\000\057\000\021\000\021\000\021\000\022\000\
\021\000\021\000\059\000\022\000\053\000\022\000\051\000\022\000\
\022\000\055\000\049\000\034\000\078\000\022\000\022\000\022\000\
\034\000\022\000\022\000\035\000\034\000\104\000\034\000\000\000\
\035\000\063\000\064\000\065\000\035\000\000\000\035\000\000\000\
\034\000\000\000\034\000\034\000\036\000\000\000\000\000\039\000\
\035\000\036\000\035\000\035\000\039\000\036\000\000\000\036\000\
\039\000\085\000\039\000\061\000\062\000\063\000\064\000\065\000\
\000\000\036\000\000\000\036\000\036\000\000\000\039\000\027\000\
\000\000\000\000\000\000\027\000\027\000\027\000\027\000\027\000\
\040\000\027\000\027\000\027\000\000\000\040\000\000\000\000\000\
\000\000\040\000\000\000\040\000\085\000\000\000\061\000\062\000\
\063\000\064\000\065\000\000\000\066\000\067\000\068\000\061\000\
\062\000\063\000\064\000\065\000\000\000\066\000\067\000\068\000\
\061\000\062\000\063\000\064\000\065\000\050\000\051\000\052\000\
\053\000"

let yycheck = "\003\000\
\020\000\059\000\001\001\001\000\037\001\004\001\039\001\040\001\
\034\001\014\000\015\000\037\001\016\000\018\000\072\000\073\000\
\020\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\027\001\009\001\010\001\026\001\011\001\016\001\028\001\015\001\
\036\000\038\000\037\001\019\001\039\001\040\001\028\001\023\001\
\024\001\037\001\026\001\025\001\040\001\014\000\015\000\013\001\
\014\001\018\000\037\001\025\001\039\001\040\001\025\001\037\001\
\060\000\039\001\040\001\079\000\034\001\081\000\016\001\037\001\
\069\000\070\000\071\000\021\001\037\000\038\000\028\001\025\001\
\028\001\027\001\012\001\079\000\020\001\081\000\082\000\084\000\
\028\001\034\001\028\001\037\001\037\001\039\001\040\001\002\001\
\003\001\109\000\033\001\111\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\012\001\
\028\001\109\000\075\000\111\000\017\001\018\001\021\001\013\001\
\014\001\022\001\083\000\084\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\025\001\009\001\010\001\001\001\002\001\
\003\001\004\001\005\001\021\001\006\001\007\001\009\001\010\001\
\001\001\002\001\003\001\004\001\005\001\026\001\034\001\016\001\
\009\001\010\001\022\001\037\001\021\001\039\001\040\001\026\001\
\025\001\022\001\027\001\018\001\029\001\030\001\031\001\032\001\
\016\001\026\001\035\001\036\001\037\001\021\001\039\001\040\001\
\025\001\025\001\025\001\027\001\025\001\029\001\030\001\031\001\
\032\001\016\001\025\001\035\001\036\001\037\001\021\001\039\001\
\040\001\025\001\025\001\025\001\027\001\025\001\029\001\030\001\
\017\001\018\001\016\001\025\001\035\001\036\001\037\001\021\001\
\039\001\040\001\025\001\025\001\025\001\027\001\025\001\029\001\
\030\001\025\001\016\000\016\001\039\000\035\001\036\001\037\001\
\021\001\039\001\040\001\016\001\025\001\080\000\027\001\255\255\
\021\001\031\001\032\001\033\001\025\001\255\255\027\001\255\255\
\037\001\255\255\039\001\040\001\016\001\255\255\255\255\016\001\
\037\001\021\001\039\001\040\001\021\001\025\001\255\255\027\001\
\025\001\027\001\027\001\029\001\030\001\031\001\032\001\033\001\
\255\255\037\001\255\255\039\001\040\001\255\255\039\001\025\001\
\255\255\255\255\255\255\029\001\030\001\031\001\032\001\033\001\
\016\001\035\001\036\001\037\001\255\255\021\001\255\255\255\255\
\255\255\025\001\255\255\027\001\027\001\255\255\029\001\030\001\
\031\001\032\001\033\001\255\255\035\001\036\001\037\001\029\001\
\030\001\031\001\032\001\033\001\255\255\035\001\036\001\037\001\
\029\001\030\001\031\001\032\001\033\001\003\001\004\001\005\001\
\006\001"

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
  EOL\000\
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
# 316 "parser.ml"
               : Functions.mainTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.statement) in
    Obj.repr(
# 64 "parser.mly"
                                 ( SingleStatement _1 )
# 323 "parser.ml"
               : Functions.body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Functions.statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.body) in
    Obj.repr(
# 65 "parser.mly"
                                 ( MultiStatement (_1, _2) )
# 331 "parser.ml"
               : Functions.body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.forDo) in
    Obj.repr(
# 69 "parser.mly"
                               ( ForStatement _1 )
# 338 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.ifElse) in
    Obj.repr(
# 70 "parser.mly"
                                    ( IfStatement _1 )
# 345 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Functions.action) in
    Obj.repr(
# 71 "parser.mly"
                        ( ActionStatement _1 )
# 352 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 75 "parser.mly"
                                    ( ForEach (_2, _4, _6) )
# 361 "parser.ml"
               : Functions.forDo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Functions.bool_cmd) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 76 "parser.mly"
                                  ( ForBool (_2, _4) )
# 369 "parser.ml"
               : Functions.forDo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Functions.bool_cmd) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 80 "parser.mly"
                                         ( If (_2, _4) )
# 377 "parser.ml"
               : Functions.ifElse))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Functions.bool_cmd) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Functions.body) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 81 "parser.mly"
                                            ( IfElse (_2, _4, _6) )
# 386 "parser.ml"
               : Functions.ifElse))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.operation) in
    Obj.repr(
# 85 "parser.mly"
                   ( Operation _1 )
# 393 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.decAction) in
    Obj.repr(
# 86 "parser.mly"
                    ( DecAction _1 )
# 400 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.mutAction) in
    Obj.repr(
# 87 "parser.mly"
                    ( MutAction _1 )
# 407 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.print) in
    Obj.repr(
# 88 "parser.mly"
                      ( PrintAction _1 )
# 414 "parser.ml"
               : Functions.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.setAction) in
    Obj.repr(
# 92 "parser.mly"
                   ( SetAction _1 )
# 421 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 93 "parser.mly"
                    ( IntCmd _1 )
# 428 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 94 "parser.mly"
                     ( StrCmd _1 )
# 435 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 95 "parser.mly"
                  ( BoolCmd _1 )
# 442 "parser.ml"
               : Functions.operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.int_cmd) in
    Obj.repr(
# 99 "parser.mly"
                           ( _2 )
# 449 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_int) in
    Obj.repr(
# 100 "parser.mly"
                         ( IntOrVar _1 )
# 456 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 101 "parser.mly"
                             ( Plus (_1, _3) )
# 464 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 102 "parser.mly"
                              ( Minus (_1, _3) )
# 472 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 103 "parser.mly"
                               ( Times (_1, _3) )
# 480 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 104 "parser.mly"
                               ( Divide (_1, _3) )
# 488 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 105 "parser.mly"
                             ( Mod (_1, _3) )
# 496 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 109 "parser.mly"
               ( Int _1 )
# 503 "parser.ml"
               : Functions.def_int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
               ( Int_Idf _1 )
# 510 "parser.ml"
               : Functions.def_int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 114 "parser.mly"
                         ( StrOrVar _1 )
# 517 "parser.ml"
               : Functions.str_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.str_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 115 "parser.mly"
                               ( Cat (_1, _3) )
# 525 "parser.ml"
               : Functions.str_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                 ( Str _1 )
# 532 "parser.ml"
               : Functions.def_string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
               ( Str_Idf _1 )
# 539 "parser.ml"
               : Functions.def_string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.bool_cmd) in
    Obj.repr(
# 124 "parser.mly"
                            ( _2 )
# 546 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_bool) in
    Obj.repr(
# 125 "parser.mly"
                       ( BoolOrVar _1 )
# 553 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 126 "parser.mly"
                          ( Les (_1, _3) )
# 561 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 127 "parser.mly"
                             ( Grt (_1, _3) )
# 569 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 128 "parser.mly"
                             ( IntEq (_1, _3) )
# 577 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.str_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 129 "parser.mly"
                             ( StrEq (_1, _3) )
# 585 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.bool_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 130 "parser.mly"
                               ( BlEq (_1, _3) )
# 593 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.bool_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 131 "parser.mly"
                           ( And (_1, _3) )
# 601 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.bool_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 132 "parser.mly"
                          ( Or (_1, _3) )
# 609 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 136 "parser.mly"
               ( Bool _1 )
# 616 "parser.ml"
               : Functions.def_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 137 "parser.mly"
               ( Bool _1 )
# 623 "parser.ml"
               : Functions.def_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
               ( Bool_Idf _1 )
# 630 "parser.ml"
               : Functions.def_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "parser.mly"
               ( Set _1 )
# 637 "parser.ml"
               : Functions.setAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 143 "parser.mly"
                             ( SetAdd (_1, _3) )
# 645 "parser.ml"
               : Functions.setAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 144 "parser.mly"
                              ( SetRem (_1, _3) )
# 653 "parser.ml"
               : Functions.setAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 148 "parser.mly"
                ( _1 )
# 660 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "parser.mly"
               ( _1 )
# 667 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
                  ( LVarDec _2 )
# 674 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 154 "parser.mly"
                  ( IVarDec (_2, IntOrVar (Int 0)) )
# 681 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 155 "parser.mly"
                            ( IVarDec (_2, _4) )
# 689 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                   ( SVarDec (_2, StrOrVar (Str "")) )
# 696 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 157 "parser.mly"
                            ( SVarDec (_2, _4) )
# 704 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
                  ( BVarDec (_2, BoolOrVar (Bool false)) )
# 711 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 159 "parser.mly"
                             ( BVarDec (_2, _4) )
# 719 "parser.ml"
               : Functions.decAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.setAction) in
    Obj.repr(
# 163 "parser.mly"
                           ( SetMut (_1, _3) )
# 727 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 164 "parser.mly"
                        ( IntMut (_1, _3) )
# 735 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 165 "parser.mly"
                             ( StrMut (_1, _3) )
# 743 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 166 "parser.mly"
                          ( BlMut (_1, _3) )
# 751 "parser.ml"
               : Functions.mutAction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.operation) in
    Obj.repr(
# 170 "parser.mly"
                        ( Print _2 )
# 758 "parser.ml"
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
