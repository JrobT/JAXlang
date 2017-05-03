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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

open Functions;;

# 49 "parser.ml"
let yytransl_const = [|
  267 (* START *);
  268 (* FINISH *);
  269 (* FOR *);
  270 (* IN *);
  271 (* DO *);
  272 (* STOP *);
  273 (* PLACE *);
  274 (* DELETE *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* END *);
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
  289 (* CONS *);
  290 (* LESS *);
  291 (* GREATER *);
  292 (* ISEQUAL *);
  293 (* NOT *);
  294 (* OR *);
  295 (* AND *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* STRING *);
  259 (* SVAR *);
  260 (* IVAR *);
  261 (* BVAR *);
  262 (* LVAR *);
  263 (* INPUT *);
  264 (* NUMBER *);
  265 (* TRUE *);
  266 (* FALSE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\006\000\006\000\007\000\007\000\
\007\000\007\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\008\000\008\000\009\000\009\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\013\000\
\013\000\013\000\014\000\014\000\014\000\015\000\015\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\016\000\016\000\
\016\000\016\000\018\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\001\000\001\000\002\000\007\000\005\000\
\005\000\007\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\003\000\001\000\001\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\001\000\001\000\003\000\003\000\001\000\001\000\002\000\
\002\000\004\000\002\000\004\000\002\000\004\000\003\000\003\000\
\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\060\000\025\000\029\000\000\000\000\000\
\000\000\000\000\046\000\040\000\041\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\000\000\011\000\
\000\000\027\000\000\000\020\000\000\000\032\000\015\000\000\000\
\013\000\012\000\014\000\000\000\000\000\000\000\000\000\000\000\
\026\000\042\000\000\000\000\000\000\000\030\000\000\000\047\000\
\059\000\000\000\000\000\000\000\048\000\000\000\000\000\001\000\
\003\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\031\000\028\000\000\000\000\000\
\000\000\023\000\024\000\000\000\000\000\000\000\037\000\000\000\
\000\000\044\000\045\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\009\000\000\000\000\000\
\007\000\010\000"

let yydgoto = "\002\000\
\004\000\019\000\020\000\021\000\022\000\023\000\024\000\043\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000"

let yysindex = "\013\000\
\009\255\000\000\106\255\000\000\000\000\000\000\250\254\014\255\
\025\255\059\255\000\000\000\000\000\000\132\255\142\255\001\255\
\178\255\142\255\032\255\106\255\000\000\000\000\007\255\000\000\
\244\254\000\000\222\255\000\000\229\255\000\000\000\000\030\255\
\000\000\000\000\000\000\070\255\008\255\142\255\116\255\079\255\
\000\000\000\000\244\254\222\255\020\255\000\000\082\255\000\000\
\000\000\067\255\077\255\089\255\000\000\214\255\181\255\000\000\
\000\000\000\000\070\255\070\255\008\255\008\255\008\255\008\255\
\008\255\008\255\008\255\142\255\142\255\142\255\070\255\070\255\
\065\255\008\255\206\255\229\255\000\000\116\255\106\255\106\255\
\070\255\008\255\142\255\000\000\000\000\000\000\065\255\096\255\
\096\255\000\000\000\000\206\255\206\255\206\255\000\000\229\255\
\053\255\000\000\000\000\232\255\109\255\115\255\172\255\065\255\
\206\255\229\255\106\255\000\000\106\255\000\000\124\255\137\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\197\255\
\061\255\131\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\218\255\000\000\000\000\000\000\000\000\
\128\255\000\000\129\255\000\000\136\255\000\000\000\000\138\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\141\255\151\255\153\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\161\255\000\000\164\255\007\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\152\255\016\255\
\135\255\000\000\000\000\160\255\177\255\185\255\000\000\003\255\
\076\255\000\000\000\000\000\000\000\000\000\000\000\000\170\255\
\176\255\182\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\237\255\000\000\000\000\000\000\000\000\141\000\253\255\
\067\000\001\000\000\000\011\000\000\000\164\000\128\000\000\000\
\000\000\000\000"

let yytablesize = 268
let yytable = "\025\000\
\057\000\005\000\006\000\046\000\041\000\042\000\048\000\011\000\
\005\000\012\000\013\000\041\000\025\000\001\000\044\000\044\000\
\025\000\039\000\054\000\003\000\059\000\036\000\039\000\060\000\
\045\000\047\000\018\000\039\000\055\000\039\000\021\000\058\000\
\073\000\074\000\079\000\021\000\030\000\075\000\044\000\030\000\
\021\000\037\000\021\000\056\000\021\000\021\000\071\000\072\000\
\076\000\021\000\021\000\021\000\038\000\021\000\021\000\068\000\
\087\000\069\000\070\000\102\000\103\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\044\000\044\000\044\000\006\000\
\046\000\030\000\100\000\025\000\025\000\104\000\095\000\096\000\
\097\000\030\000\105\000\044\000\030\000\042\000\039\000\111\000\
\068\000\112\000\038\000\070\000\078\000\106\000\081\000\038\000\
\042\000\059\000\042\000\042\000\038\000\080\000\038\000\025\000\
\082\000\025\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\038\000\012\000\013\000\083\000\068\000\014\000\069\000\
\070\000\048\000\011\000\107\000\015\000\086\000\063\000\064\000\
\016\000\017\000\108\000\018\000\005\000\006\000\040\000\041\000\
\042\000\098\000\099\000\113\000\012\000\013\000\005\000\006\000\
\046\000\041\000\042\000\047\000\047\000\022\000\012\000\013\000\
\017\000\016\000\022\000\047\000\049\000\018\000\114\000\022\000\
\018\000\022\000\043\000\022\000\022\000\051\000\036\000\018\000\
\022\000\022\000\022\000\036\000\022\000\022\000\033\000\049\000\
\036\000\053\000\036\000\033\000\050\000\051\000\052\000\053\000\
\033\000\057\000\033\000\036\000\056\000\036\000\036\000\034\000\
\109\000\110\000\052\000\033\000\034\000\033\000\033\000\035\000\
\050\000\034\000\077\000\034\000\035\000\101\000\054\000\085\000\
\000\000\035\000\000\000\035\000\034\000\000\000\034\000\034\000\
\068\000\000\000\069\000\070\000\035\000\026\000\035\000\035\000\
\000\000\026\000\026\000\026\000\026\000\002\000\026\000\026\000\
\026\000\002\000\061\000\062\000\063\000\064\000\002\000\002\000\
\084\000\000\000\061\000\062\000\063\000\064\000\000\000\065\000\
\066\000\067\000\061\000\062\000\063\000\064\000\000\000\065\000\
\066\000\067\000\084\000\000\000\061\000\062\000\063\000\064\000\
\068\000\000\000\069\000\070\000"

let yycheck = "\003\000\
\020\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\001\001\009\001\010\001\004\001\016\000\001\000\014\000\015\000\
\020\000\015\001\018\000\011\001\033\001\028\001\020\001\036\001\
\014\000\015\000\026\001\025\001\018\000\027\001\015\001\025\001\
\036\000\026\001\015\001\020\001\033\001\037\000\038\000\036\001\
\025\001\028\001\027\001\012\001\029\001\030\001\017\001\018\001\
\038\000\034\001\035\001\036\001\028\001\038\001\039\001\036\001\
\060\000\038\001\039\001\079\000\080\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\002\001\
\003\001\025\001\074\000\079\000\080\000\081\000\068\000\069\000\
\070\000\033\001\082\000\083\000\036\001\025\001\028\001\107\000\
\036\001\109\000\015\001\039\001\014\001\083\000\028\001\020\001\
\036\001\033\001\038\001\039\001\025\001\020\001\027\001\107\000\
\028\001\109\000\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\038\001\009\001\010\001\028\001\036\001\013\001\038\001\
\039\001\006\001\007\001\015\001\019\001\059\000\031\001\032\001\
\023\001\024\001\016\001\026\001\001\001\002\001\003\001\004\001\
\005\001\071\000\072\000\016\001\009\001\010\001\001\001\002\001\
\003\001\004\001\005\001\017\001\018\001\015\001\009\001\010\001\
\025\001\025\001\020\001\025\001\016\000\026\001\022\001\025\001\
\025\001\027\001\025\001\029\001\030\001\025\001\015\001\026\001\
\034\001\035\001\036\001\020\001\038\001\039\001\015\001\025\001\
\025\001\025\001\027\001\020\001\003\001\004\001\005\001\006\001\
\025\001\025\001\027\001\036\001\025\001\038\001\039\001\015\001\
\021\001\022\001\025\001\036\001\020\001\038\001\039\001\015\001\
\025\001\025\001\039\000\027\001\020\001\078\000\025\001\027\001\
\255\255\025\001\255\255\027\001\036\001\255\255\038\001\039\001\
\036\001\255\255\038\001\039\001\036\001\025\001\038\001\039\001\
\255\255\029\001\030\001\031\001\032\001\012\001\034\001\035\001\
\036\001\016\001\029\001\030\001\031\001\032\001\021\001\022\001\
\027\001\255\255\029\001\030\001\031\001\032\001\255\255\034\001\
\035\001\036\001\029\001\030\001\031\001\032\001\255\255\034\001\
\035\001\036\001\027\001\255\255\029\001\030\001\031\001\032\001\
\036\001\255\255\038\001\039\001"

let yynames_const = "\
  START\000\
  FINISH\000\
  FOR\000\
  IN\000\
  DO\000\
  STOP\000\
  PLACE\000\
  DELETE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  END\000\
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
  NUMBER\000\
  TRUE\000\
  FALSE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 58 "parser.mly"
                            ( Body _2 )
# 301 "parser.ml"
               : Functions.main))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.statement) in
    Obj.repr(
# 62 "parser.mly"
                                 ( SingleStatement _1 )
# 308 "parser.ml"
               : Functions.body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Functions.statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.body) in
    Obj.repr(
# 63 "parser.mly"
                                 ( Multiline (_1, _2) )
# 316 "parser.ml"
               : Functions.body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.forAll) in
    Obj.repr(
# 67 "parser.mly"
                               ( ForStatement _1 )
# 323 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.ifElse) in
    Obj.repr(
# 68 "parser.mly"
                                    ( IfStatement _1 )
# 330 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Functions.doSomething) in
    Obj.repr(
# 69 "parser.mly"
                            ( CmdStatement _1 )
# 337 "parser.ml"
               : Functions.statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 73 "parser.mly"
                                 ( ForEach (_2, _4, _6) )
# 346 "parser.ml"
               : Functions.forAll))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Functions.bool_cmd) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 74 "parser.mly"
                                  ( ForBool (_2, _4) )
# 354 "parser.ml"
               : Functions.forAll))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Functions.bool_cmd) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 78 "parser.mly"
                                         ( If (_2, _4) )
# 362 "parser.ml"
               : Functions.ifElse))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Functions.bool_cmd) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Functions.body) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Functions.body) in
    Obj.repr(
# 79 "parser.mly"
                                            ( IfElse (_2, _4, _6) )
# 371 "parser.ml"
               : Functions.ifElse))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.expr) in
    Obj.repr(
# 83 "parser.mly"
                   ( Expr _1 )
# 378 "parser.ml"
               : Functions.doSomething))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.dec_cmd) in
    Obj.repr(
# 84 "parser.mly"
                 ( DecCmd _1 )
# 385 "parser.ml"
               : Functions.doSomething))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.mut_cmd) in
    Obj.repr(
# 85 "parser.mly"
                 ( MutCmd _1 )
# 392 "parser.ml"
               : Functions.doSomething))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.print) in
    Obj.repr(
# 86 "parser.mly"
                      ( PrintCmd _1 )
# 399 "parser.ml"
               : Functions.doSomething))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.set_cmd) in
    Obj.repr(
# 90 "parser.mly"
                  ( SetCmd _1 )
# 406 "parser.ml"
               : Functions.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 91 "parser.mly"
                    ( IntCmd _1 )
# 413 "parser.ml"
               : Functions.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 92 "parser.mly"
                     ( StrCmd _1 )
# 420 "parser.ml"
               : Functions.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 93 "parser.mly"
                  ( BoolCmd _1 )
# 427 "parser.ml"
               : Functions.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.int_cmd) in
    Obj.repr(
# 97 "parser.mly"
                           ( _2 )
# 434 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_int) in
    Obj.repr(
# 98 "parser.mly"
                       ( IntOrVar _1 )
# 441 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 99 "parser.mly"
                             ( Plus (_1, _3) )
# 449 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 100 "parser.mly"
                              ( Minus (_1, _3) )
# 457 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 101 "parser.mly"
                                ( Times (_1, _3) )
# 465 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 102 "parser.mly"
                                ( Divide (_1, _3) )
# 473 "parser.ml"
               : Functions.int_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 106 "parser.mly"
               ( Int _1 )
# 480 "parser.ml"
               : Functions.def_int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
               ( Int_Idf _1 )
# 487 "parser.ml"
               : Functions.def_int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 111 "parser.mly"
                         ( StrOrVar _1 )
# 494 "parser.ml"
               : Functions.str_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.str_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 112 "parser.mly"
                                   ( Cat (_1, _3) )
# 502 "parser.ml"
               : Functions.str_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                 ( Str _1 )
# 509 "parser.ml"
               : Functions.def_string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
               ( Str_Idf _1 )
# 516 "parser.ml"
               : Functions.def_string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Functions.bool_cmd) in
    Obj.repr(
# 121 "parser.mly"
                                ( _2 )
# 523 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_bool) in
    Obj.repr(
# 122 "parser.mly"
                         ( BoolOrVar _1 )
# 530 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 123 "parser.mly"
                              ( Less (_1, _3) )
# 538 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 124 "parser.mly"
                             ( Greater (_1, _3) )
# 546 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.int_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 125 "parser.mly"
                             ( IntEql (_1, _3) )
# 554 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.str_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 126 "parser.mly"
                                 ( StrEql (_1, _3) )
# 562 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.bool_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 127 "parser.mly"
                               ( BlEql (_1, _3) )
# 570 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.bool_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 128 "parser.mly"
                               ( And (_1, _3) )
# 578 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Functions.bool_cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 129 "parser.mly"
                              ( Or (_1, _3) )
# 586 "parser.ml"
               : Functions.bool_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 133 "parser.mly"
               ( Bool _1 )
# 593 "parser.ml"
               : Functions.def_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 134 "parser.mly"
               ( Bool _1 )
# 600 "parser.ml"
               : Functions.def_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
               ( Bool_Idf _1 )
# 607 "parser.ml"
               : Functions.def_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "parser.mly"
                   ( Set _1 )
# 614 "parser.ml"
               : Functions.set_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 140 "parser.mly"
                             ( SetPlace (_1, _3) )
# 622 "parser.ml"
               : Functions.set_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.def_string) in
    Obj.repr(
# 141 "parser.mly"
                              ( SetDel (_1, _3) )
# 630 "parser.ml"
               : Functions.set_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                ( _1 )
# 637 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 146 "parser.mly"
               ( _1 )
# 644 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 150 "parser.mly"
                  ( LVarDec _2 )
# 651 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 151 "parser.mly"
                  ( IVarDec (_2, IntOrVar (Int 0)) )
# 658 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 152 "parser.mly"
                                ( IVarDec (_2, _4) )
# 666 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
                   ( SVarDec (_2, StrOrVar (Str "")) )
# 673 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 154 "parser.mly"
                                ( SVarDec (_2, _4) )
# 681 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
                  ( BVarDec (_2, BoolOrVar (Bool false)) )
# 688 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 156 "parser.mly"
                                 ( BVarDec (_2, _4) )
# 696 "parser.ml"
               : Functions.dec_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.set_cmd) in
    Obj.repr(
# 160 "parser.mly"
                           ( SetMut (_1, _3) )
# 704 "parser.ml"
               : Functions.mut_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.int_cmd) in
    Obj.repr(
# 161 "parser.mly"
                            ( IntMut (_1, _3) )
# 712 "parser.ml"
               : Functions.mut_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.str_cmd) in
    Obj.repr(
# 162 "parser.mly"
                             ( StrMut (_1, _3) )
# 720 "parser.ml"
               : Functions.mut_cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Functions.bool_cmd) in
    Obj.repr(
# 163 "parser.mly"
                              ( BlMut (_1, _3) )
# 728 "parser.ml"
               : Functions.mut_cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Functions.expr) in
    Obj.repr(
# 167 "parser.mly"
                        ( Print _2 )
# 735 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Functions.main)
