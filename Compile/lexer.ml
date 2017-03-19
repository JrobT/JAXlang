# 3 "lexer.mll"
 

open Parser         (* parser.mli *)
exception EOF       (* End_of_file exception *)


# 9 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\212\255\077\000\160\000\235\000\054\001\002\000\000\000\
    \003\000\003\000\008\000\226\255\012\000\002\000\232\255\011\000\
    \235\255\236\255\073\001\001\000\004\000\242\255\243\255\245\255\
    \246\255\247\255\248\255\249\255\081\001\005\000\002\000\018\000\
    \254\255\147\001\255\255\070\001\004\000\252\255\001\000\009\000\
    \020\000\251\255\023\000\002\000\250\255\023\000\005\000\238\255\
    \227\255\237\255\233\255\006\000\022\000\039\000\239\255\031\000\
    \230\255\234\255\231\255\068\000\054\000\069\000\229\255\228\255\
    \063\000\225\255\213\255\061\000\061\000\224\255\002\000\095\000\
    \091\000\086\000\223\255\104\000\103\000\214\255\091\000\222\255\
    \089\000\098\000\106\000\103\000\099\000\221\255\105\000\110\000\
    \106\000\108\000\001\000\152\000\141\000\136\000\144\000\139\000\
    \215\255\230\001\049\002\124\002\199\002\018\003\093\003";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\015\000\014\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\011\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\035\000\036\000\037\000\038\000\038\000\038\000";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\035\000\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \255\255\000\000\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\032\000\032\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \032\000\014\000\033\000\005\000\002\000\021\000\019\000\050\000\
    \027\000\026\000\030\000\024\000\035\000\023\000\037\000\022\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\011\000\025\000\017\000\020\000\016\000\003\000\
    \004\000\049\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
    \091\000\078\000\039\000\075\000\010\000\012\000\028\000\081\000\
    \058\000\013\000\047\000\055\000\009\000\064\000\045\000\070\000\
    \008\000\080\000\067\000\029\000\018\000\071\000\007\000\065\000\
    \060\000\038\000\059\000\040\000\015\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\057\000\
    \041\000\048\000\046\000\053\000\054\000\056\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \063\000\061\000\062\000\066\000\068\000\069\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\101\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \072\000\073\000\074\000\076\000\077\000\079\000\086\000\082\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\083\000\084\000\085\000\087\000\088\000\089\000\
    \090\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\092\000\093\000\094\000\095\000\096\000\
    \001\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \036\000\000\000\000\000\000\000\000\000\000\000\000\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\000\000\000\000\000\000\000\000\000\000\000\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\051\000\043\000\033\000\000\000\034\000\000\000\000\000\
    \000\000\000\000\044\000\052\000\000\000\000\000\000\000\000\000\
    \042\000\000\000\255\255\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\000\000\000\000\000\000\000\000\000\000\255\255\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \102\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
    \000\000\000\000\000\000\000\000\030\000\000\000\036\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \090\000\007\000\038\000\070\000\000\000\000\000\000\000\006\000\
    \013\000\000\000\046\000\051\000\000\000\010\000\043\000\008\000\
    \000\000\006\000\009\000\000\000\000\000\008\000\000\000\010\000\
    \012\000\029\000\012\000\039\000\000\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\015\000\
    \040\000\042\000\045\000\052\000\053\000\055\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \059\000\060\000\061\000\064\000\067\000\068\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \071\000\072\000\073\000\075\000\076\000\078\000\080\000\081\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\082\000\083\000\084\000\086\000\087\000\088\000\
    \089\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\091\000\092\000\093\000\094\000\095\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\018\000\028\000\033\000\255\255\033\000\255\255\255\255\
    \255\255\255\255\028\000\018\000\255\255\255\255\255\255\255\255\
    \028\000\255\255\035\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\255\255\255\255\255\255\255\255\255\255\035\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec lexer_main lexbuf =
    __ocaml_lex_lexer_main_rec lexbuf 0
and __ocaml_lex_lexer_main_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 17 "lexer.mll"
                         str_id
# 357 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 17 "lexer.mll"
                                                ( STRING(List.nth (Str.split_delim (Str.regexp "\"") str_id) 1) )
# 361 "lexer.ml"

  | 1 ->
# 18 "lexer.mll"
                                                ( lexer_main lexbuf )
# 366 "lexer.ml"

  | 2 ->
let
# 11 "lexer.mll"
                           lxm
# 372 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 19 "lexer.mll"
                                                ( INT (int_of_string lxm) )
# 376 "lexer.ml"

  | 3 ->
# 20 "lexer.mll"
                                                ( lexer_main lexbuf )
# 381 "lexer.ml"

  | 4 ->
# 21 "lexer.mll"
                                                ( START )
# 386 "lexer.ml"

  | 5 ->
# 22 "lexer.mll"
                                                ( FINISH )
# 391 "lexer.ml"

  | 6 ->
# 23 "lexer.mll"
                                                ( LPAREN )
# 396 "lexer.ml"

  | 7 ->
# 24 "lexer.mll"
                                                ( RPAREN )
# 401 "lexer.ml"

  | 8 ->
# 25 "lexer.mll"
                                                ( EOL )
# 406 "lexer.ml"

  | 9 ->
# 26 "lexer.mll"
                                                ( PLUS )
# 411 "lexer.ml"

  | 10 ->
# 27 "lexer.mll"
                                                ( MINUS )
# 416 "lexer.ml"

  | 11 ->
# 28 "lexer.mll"
                                                ( TIMES )
# 421 "lexer.ml"

  | 12 ->
# 29 "lexer.mll"
                                                ( DIVIDE )
# 426 "lexer.ml"

  | 13 ->
# 30 "lexer.mll"
                                                ( MOD )
# 431 "lexer.ml"

  | 14 ->
# 31 "lexer.mll"
                                                ( EQUAL )
# 436 "lexer.ml"

  | 15 ->
# 32 "lexer.mll"
                                                ( CONS )
# 441 "lexer.ml"

  | 16 ->
# 33 "lexer.mll"
                                                ( TRUE true )
# 446 "lexer.ml"

  | 17 ->
# 34 "lexer.mll"
                                                ( FALSE false )
# 451 "lexer.ml"

  | 18 ->
# 35 "lexer.mll"
                                                ( ISEQUAL )
# 456 "lexer.ml"

  | 19 ->
# 36 "lexer.mll"
                                                ( LESS )
# 461 "lexer.ml"

  | 20 ->
# 37 "lexer.mll"
                                                ( GREATER )
# 466 "lexer.ml"

  | 21 ->
# 38 "lexer.mll"
                                                ( OR )
# 471 "lexer.ml"

  | 22 ->
# 39 "lexer.mll"
                                                ( AND )
# 476 "lexer.ml"

  | 23 ->
# 40 "lexer.mll"
                                                ( NOT )
# 481 "lexer.ml"

  | 24 ->
# 41 "lexer.mll"
                                                ( IF )
# 486 "lexer.ml"

  | 25 ->
# 42 "lexer.mll"
                                                ( THEN )
# 491 "lexer.ml"

  | 26 ->
# 43 "lexer.mll"
                                                ( ELSE )
# 496 "lexer.ml"

  | 27 ->
# 44 "lexer.mll"
                                                ( END )
# 501 "lexer.ml"

  | 28 ->
# 45 "lexer.mll"
                                                ( FOR )
# 506 "lexer.ml"

  | 29 ->
# 46 "lexer.mll"
                                                ( COLON )
# 511 "lexer.ml"

  | 30 ->
# 47 "lexer.mll"
                                                ( DO )
# 516 "lexer.ml"

  | 31 ->
# 48 "lexer.mll"
                                                ( STOP )
# 521 "lexer.ml"

  | 32 ->
# 49 "lexer.mll"
                                                ( PRINT )
# 526 "lexer.ml"

  | 33 ->
# 50 "lexer.mll"
                                                ( VAR )
# 531 "lexer.ml"

  | 34 ->
# 51 "lexer.mll"
                                                ( STRING "")
# 536 "lexer.ml"

  | 35 ->
let
# 52 "lexer.mll"
                             int_var_id
# 542 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 52 "lexer.mll"
                                                ( IVAR( int_var_id ) )
# 546 "lexer.ml"

  | 36 ->
let
# 53 "lexer.mll"
                             str_var_id
# 552 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 53 "lexer.mll"
                                                ( SVAR( str_var_id ) )
# 556 "lexer.ml"

  | 37 ->
let
# 54 "lexer.mll"
                             bl_var_id
# 562 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 54 "lexer.mll"
                                                ( BVAR( bl_var_id ) )
# 566 "lexer.ml"

  | 38 ->
let
# 55 "lexer.mll"
                              set
# 572 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 55 "lexer.mll"
                                                ( LVAR set )
# 576 "lexer.ml"

  | 39 ->
let
# 56 "lexer.mll"
                         input
# 582 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 56 "lexer.mll"
                                                ( INPUT input  )
# 586 "lexer.ml"

  | 40 ->
let
# 57 "lexer.mll"
                         output_stuff
# 592 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 13) in
# 57 "lexer.mll"
                                                ( COUNT output_stuff )
# 596 "lexer.ml"

  | 41 ->
# 58 "lexer.mll"
                                                ( PLACE )
# 601 "lexer.ml"

  | 42 ->
# 59 "lexer.mll"
                                                ( DELETE )
# 606 "lexer.ml"

  | 43 ->
# 60 "lexer.mll"
                                                ( raise EOF )
# 611 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_lexer_main_rec lexbuf __ocaml_lex_state

;;

