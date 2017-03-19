
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
| Mod of int_cmd * int_cmd
;;

type def_bool = 
| Bool of bool
| Bool_Idf of string
;;

type bool_cmd =
| BoolOrVar of def_bool
| Les of int_cmd * int_cmd
| Grt of int_cmd * int_cmd
| LesEq of int_cmd * int_cmd
| GrtEq of int_cmd * int_cmd
| IntEq of int_cmd * int_cmd
| StrEq of str_cmd * str_cmd
| BlEq of bool_cmd * bool_cmd
| Or of bool_cmd * bool_cmd
| And of bool_cmd * bool_cmd
;;

type setAction =
| Set of string
| SetAdd of string * def_string
| SetRem of string * def_string
;;

type decAction =
| LVarDec of string
| IVarDec of string * int_cmd
| SVarDec of string * str_cmd
| BVarDec of string * bool_cmd
;;

type operation =
| SetAction of setAction
| IntCmd of int_cmd
| StrCmd of str_cmd
| BoolCmd of bool_cmd
;;

type print =
| Print of operation
;;

type mutAction =
| SetMut of string * setAction
| IntMut of string * int_cmd
| StrMut of string * str_cmd
| BlMut of string * bool_cmd
;;

type action = 
| Operation of operation
| DecAction of decAction
| MutAction of mutAction
| PrintAction of print
;;

type body =
| SingleStatement of statement
| MultiStatement of statement * body
and 
statement = 
| IfStatement of ifElse
| ForStatement of forDo
| ActionStatement of action
and 
ifElse =
| If of bool_cmd * body
| IfElse of bool_cmd * body * body
and 
forDo =
| ForBool of bool_cmd * body
| ForEach of string * string * body
;;

type mainTree = 
| Body of body
;;
