type parseMyStr = 
| Str of string
| StrName of string
;;

type strAction =
| StrWithName of parseMyStr
| Cons of strAction * parseMyStr
;;

type setStuff =
| Set of string
| SetPlace of string * parseMyStr
| SetDelete of string * parseMyStr
;;

type parseMyInt = 
| Int of int
| IntName of string
;;

type intAction =
| IntWithName of parseMyInt
| Plus of intAction * intAction
| Minus of intAction * intAction
| Times of intAction * intAction
| Divide of intAction * intAction
| Mod of intAction * intAction
| Uminus of intAction
;;

type parseMyBool = 
| Bool of bool
| BoolName of string
;;

type boolAction =
| BoolWithName of parseMyBool
| Less of intAction * intAction
| Greater of intAction * intAction
| IntEql of intAction * intAction
| StrEql of parseMyStr * parseMyStr
| BlEql of boolAction * boolAction
| Or of boolAction * boolAction
| And of boolAction * boolAction
| Not of boolAction
;;

type decline =
| LVarDec of string
| IVarDec of string * intAction
| SVarDec of string * parseMyStr
| BVarDec of string * boolAction
;;

type operation =
| Setline of setStuff
| Intline of intAction
| Strline of parseMyStr
| Boolline of boolAction
;;

type print =
| Print of operation
;;

type mutline =
| SetMut of string * setStuff
| StrMut of string * parseMyStr
| IntMut of string * intAction
| BlMut of string * boolAction
;;

type line =
| Operation of operation
| Decline of decline
| Mutline of mutline
| Printline of print
;;

type body =
| SingleStatement of statement
| Multiline of statement * body
and 
statement = 
| IfStatement of ifElse
| ForStatement of forDo
| LineStatement of line
and 
ifElse =
| If of boolAction * body
| IfElse of boolAction * body * body
and
forDo =
| ForEach of string * string * body
| TrueOrFalse of boolAction * body
;;

type functions_main = 
| Body of body
;;
