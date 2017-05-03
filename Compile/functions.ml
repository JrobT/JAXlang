
type def_string = 
| Str of string
| Str_Idf of string
;;

type str_cmd =
| StrOrVar of def_string
| Cat of str_cmd * def_string
;;

type def_int = 
| Int of int
| Int_Idf of string
;;

type int_cmd =
| IntOrVar of def_int
| Plus of int_cmd * int_cmd
| Minus of int_cmd * int_cmd
| Times of int_cmd * int_cmd
| Divide of int_cmd * int_cmd
;;

type def_bool = 
| Bool of bool
| Bool_Idf of string
;;

type bool_cmd =
| BoolOrVar of def_bool
| Less of int_cmd * int_cmd
| Greater of int_cmd * int_cmd
| StrEql of str_cmd * str_cmd
| IntEql of int_cmd * int_cmd
| BlEql of bool_cmd * bool_cmd
| Or of bool_cmd * bool_cmd
| And of bool_cmd * bool_cmd
;;

type set_cmd =
| Set of string
| SetPlace of string * def_string
| SetDel of string * def_string
;;

type mut_cmd =
| SetMut of string * set_cmd
| StrMut of string * str_cmd
| IntMut of string * int_cmd
| BlMut of string * bool_cmd
;;

type dec_cmd =
| LVarDec of string
| IVarDec of string * int_cmd
| SVarDec of string * str_cmd
| BVarDec of string * bool_cmd
;;

type expr =
| SetCmd of set_cmd
| StrCmd of str_cmd
| IntCmd of int_cmd
| BoolCmd of bool_cmd
;;

type print =
| Print of expr
;;

type doSomething = 
| Expr of expr
| DecCmd of dec_cmd
| MutCmd of mut_cmd
| PrintCmd of print
;;

type body =
| SingleStatement of statement
| Multiline of statement * body
and 
statement = 
| CmdStatement of doSomething
| IfStatement of ifElse
| ForStatement of forAll
and
ifElse =
| If of bool_cmd * body
| IfElse of bool_cmd * body * body
and 
forAll =
| ForBool of bool_cmd * body
| ForEach of string * string * body
;;

type main = 
| Body of body
;;
