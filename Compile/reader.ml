
open List;;
open Lexer;;
open Functions;;

let inputNumber = ref 0;;

let car x = match x with 
  | (h, t) -> h;;
let split list n =
  let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  
                       in aux n [] list;;
let makeOutSet o =
  let trun = (car (split o !inputNumber)) in
  let rec makeOutSetAux o = match o with 
  | [] -> ""
  | [x] -> x
  | head::body -> head^", "^(makeOutSetAux body)
                  in "{"^(makeOutSetAux trun)^"}"
;;

let rec emptyStringSymbols = function
  | [] -> []
  | (h::t) when h = "" -> ":"::(emptyStringSymbols t) 
  | (h::t) ->  h::(emptyStringSymbols t)
;;

module VariableBinding = Map.Make(String);;
let bindMyString = ref VariableBinding.empty;;
let bindMyInt = ref VariableBinding.empty;;
let bindMyBool = ref VariableBinding.empty;;
let bindMySet = ref VariableBinding.empty;;

let strings_in_input input = 
  let remove_stuff = Str.global_replace (Str.regexp "['{'|'}'|' ']") "" in
    Str.split_delim (Str.regexp ",") (remove_stuff input);;
let uniq lst = (* found something on hashtable ocaml, what do you think? *)
  let unique = Hashtbl.create (List.length lst) in
  List.iter (fun i -> Hashtbl.replace unique i ()) lst;
  Hashtbl.fold (fun i () j -> i :: j) unique []
;;
let sort l =
  List.sort compare (uniq l)
;;
let uniq_strings input =
  sort (strings_in_input input)
;;

(* made them methods, cleaner than how it was before *)
let lookupSet name =
  VariableBinding.find name !bindMySet
;;
let lookupStrVar e = match e with
  | Str s -> s
  | Str_Idf sv ->
        VariableBinding.find sv !bindMyString
;;
let lookupIntVar e = match e with
  | Int i -> i 
  | Int_Idf iv ->
        VariableBinding.find iv !bindMyInt
;;
let lookupBlVar e = match e with
  | Bool b -> b
  | Bool_Idf bv ->
        VariableBinding.find bv !bindMyBool
;;

let dealWithInput input_line stream_number= 
  bindMySet := VariableBinding.add ("$in"^(string_of_int !stream_number)) (emptyStringSymbols (uniq_strings input_line)) !bindMySet
;;
let dealWithSets e  = match e with
  | Set s -> lookupSet s
  | SetPlace (name, sv) -> 
          (let string_val = lookupStrVar sv in
                try
                    let set = lookupSet name in string_val :: set;
                with Not_found -> failwith (name))
  | SetDel (name, sv) -> 
          (let string_val = lookupStrVar sv in
            let set = lookupSet name in
                List.filter (fun x -> 
                    if (compare string_val x)==0 then false else true) set)
;;

let setVariable name = 
  try
    try
        VariableBinding.find name !bindMyInt;
        None
    with Not_found -> try 
        VariableBinding.find name !bindMyString; 
        None
    with Not_found -> 
        VariableBinding.find name !bindMyBool;
        None        
  with Not_found ->  failwith (name)
;;

let rec str_cmd e = match e with
  | StrOrVar sv -> lookupStrVar sv
  | Cat (s1, s2) -> (str_cmd s1) ^ (lookupStrVar s2)
;;
let rec mathsStuff e = match e with
  | IntOrVar iv -> lookupIntVar iv
  | Plus (v1, v2) -> (mathsStuff v1) + (mathsStuff v2)
  | Minus (v1, v2) -> (mathsStuff v1) - (mathsStuff v2)
  | Times (v1, v2) -> (mathsStuff v1) * (mathsStuff v2)
  | Divide (v1, v2) -> (mathsStuff v1) / (mathsStuff v2)
;;
let rec booleanFun e = match e with
  | BoolOrVar bv -> lookupBlVar bv
  | Less (ia1, ia2) -> (mathsStuff ia1) < (mathsStuff ia2)
  | Greater (ia1, ia2) -> (mathsStuff ia1) > (mathsStuff ia2)
  | IntEql (ia1, ia2) -> (mathsStuff ia1) == (mathsStuff ia2)
  | StrEql (sa1, sa2) -> let value = String.compare (str_cmd sa1) (str_cmd sa2) in value == 0
  | BlEql (ba1, ba2) -> (booleanFun ba1) == (booleanFun ba2)
  | Or (ba1, ba2) -> (booleanFun ba1) || (booleanFun ba2)
  | And (ba1, ba2) -> (booleanFun ba1) && (booleanFun ba2)
;;
let rec declaration e = match e with
  | LVarDec s -> bindMySet := VariableBinding.add s [] !bindMySet
  | IVarDec (s, ia) -> bindMyInt := VariableBinding.add s (mathsStuff ia) !bindMyInt
  | SVarDec (s, sa) -> bindMyString := VariableBinding.add s (str_cmd sa) !bindMyString
  | BVarDec (s, ba) -> bindMyBool := VariableBinding.add s (booleanFun ba) !bindMyBool
;;

let rec printOut e = match e with
    | Print (StrCmd s) -> print_newline (print_string (str_cmd s))
    | Print (SetCmd s) ->
        let set = (dealWithSets s) in print_newline (print_string (makeOutSet (emptyStringSymbols (sort set))))
    | Print (IntCmd i) -> print_newline (print_int (mathsStuff i))
    | Print (BoolCmd b) -> 
        (let result = (booleanFun b) in
            if (result == true) then print_newline (print_string "true") 
            else print_newline (print_string "false"))
;;

let runExpr e = match e with
  | SetCmd sa -> dealWithSets sa; ()
  | IntCmd ia -> mathsStuff ia; ()
  | StrCmd sa -> str_cmd sa; ()
  | BoolCmd ba -> booleanFun ba; ()
;;

let mutation e = match e with
  | SetMut (name, sa) -> 
          (let new_set = dealWithSets sa in 
            bindMySet := VariableBinding.add name new_set !bindMySet)
  | IntMut (name, ia) -> 
          (let new_int = mathsStuff ia in
            bindMyInt := VariableBinding.add name new_int !bindMyInt)
  | StrMut (name, sa) -> 
          (let new_str = str_cmd sa in
            bindMyString := VariableBinding.add name new_str !bindMyString)
  | BlMut (blName, ba) -> 
          (let new_bl = booleanFun ba in
            bindMyBool := VariableBinding.add blName new_bl !bindMyBool)              
;;

let doSomethingCool e = match e with
  | Expr op -> runExpr op
  | DecCmd s -> declaration s
  | MutCmd s -> mutation s
  | PrintCmd s -> printOut s
;;

let rec dealWithProgram e = match e with
  | Body code -> dealWithCode code
and dealWithCode e = match e with
  | SingleStatement s -> preparedStatements s
  | Multiline (s, b) -> preparedStatements s; dealWithCode b
and forLoop e = match e with
    | ForBool (bl, bod) -> while (booleanFun bl) do (dealWithCode bod) done
    | ForEach (v, setName, bod) -> try
      (let set = lookupSet setName in
        let count = List.length set in
            declaration (SVarDec (v, StrOrVar (Str "")));
            for i = 0 to (count - 1) do 
                mutation (StrMut (v, StrOrVar (Str (List.nth set i))));
                dealWithCode bod;
            done;
        bindMyString := VariableBinding.remove v !bindMyString)
        with Not_found -> failwith (setName)
and preparedStatements e = match e with
    | CmdStatement s -> doSomethingCool s
    | IfStatement s -> dealWithIf s
    | ForStatement s -> forLoop s
  and dealWithIf e = match e with
    | If (b, bod) -> if (booleanFun b) then (dealWithCode bod)
    | IfElse (b, bod1, bod2) -> if (booleanFun b) then (dealWithCode bod1) else (dealWithCode bod2)
;;

let getInput = 
  try
    let info = ref 0 in
      while true do
        let line = input_line stdin in
          if (Str.string_match (Str.regexp "^[0-9]+$") line 0) then 
            (inputNumber := (int_of_string line); (* Alex: still not working *)
             bindMyInt := VariableBinding.add "_output_count" !inputNumber !bindMyInt)
          else( 
            dealWithInput line info;
            info := !info + 1
          )
      done;
      None
  with
      End_of_file -> None
;;
let run = getInput;
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = (Parser.main Lexer.lexer_main lexbuf) in
      dealWithProgram result
  with Lexer.EOF ->
    exit 0
