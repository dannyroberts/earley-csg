open Cfg.Tree
type var = string
type exp = Unit | Bool of bool | Int of int | Char of char | Str of string | Var of var | Not of exp |
	Minus of exp | Sum of exp * exp | Prod of exp * exp | Equals of exp * exp | Less of exp * exp |
	GetChar of exp * exp | Len of exp | Atoi of exp | Fail
	

exception Hell;;

let string_of_exp e =
	let rec string_of_exp' e bind = 
		(*bind: minus-> 0, prod->1, sum->2, equals->3*)
		let paren cond str = if cond then "(" ^ str ^ ")" else str in
		match e with
		  Bool b -> string_of_bool b
		| Int n -> string_of_int n
		| Char c -> "\'" ^ String.escaped (String.make 1 c) ^ "\'"
		| Str str -> "\"" ^ String.escaped str ^ "\""
		| Var x -> x
		| Minus e -> "-" ^ string_of_exp' e 0
		| Not e -> "!" ^ string_of_exp' e 0
		| Sum (e1,e2) -> paren (bind < 2)
			(string_of_exp' e1 2 ^ "+" ^ string_of_exp' e2 2)
		| Prod(e1,e2) -> paren (bind < 1)
			(string_of_exp' e1 1 ^ "*" ^ string_of_exp' e2 1)
		| Equals(e1,e2) -> paren (bind < 3)
			(string_of_exp' e1 3 ^ "=" ^ string_of_exp' e2 3)
		| Less(e1,e2) -> paren (bind <3)
			(string_of_exp' e1 3 ^ "<" ^ string_of_exp' e2 3)
		| GetChar(stre,inte) ->
			string_of_exp' stre 0 ^ "[" ^ string_of_exp' inte 10 ^ "]"
		| Len(stre) -> "len(" ^ string_of_exp' stre 10 ^ ")"
		| Atoi e -> "int(" ^ string_of_exp' e 10 ^ ")"
		| Unit -> "()"
	in
	string_of_exp' e 10

let rec find x (cxt:(var*exp) list) : exp =
	match cxt with
	  [] -> raise Hell
	| (y,e)::cxt -> if x=y then e else (find x cxt)

let rec val_of_exp exp (cxt:(var*exp) list) : exp =
	try
		match exp with
		  Var x -> find x cxt
		| Sum (e1,e2) ->
			let Int n1 = val_of_exp e1 cxt in
			let Int n2 = val_of_exp e2 cxt in
			Int (n1+n2)
		| Prod (e1,e2) ->
			let Int n1 = val_of_exp e1 cxt in
			let Int n2 = val_of_exp e2 cxt in
			Int (n1*n2)
		| Minus e ->
			let Int n = val_of_exp e cxt in
			Int (-n)
		| GetChar(e1,e2) ->
			let Str str = val_of_exp e1 cxt in
			let Int n = val_of_exp e2 cxt in
			Char (str.[n])
		| Len e ->
			let Str str = val_of_exp e cxt in
			Int (String.length str)
		| Atoi e ->
			let Str str = val_of_exp e cxt in
			Int (int_of_string str)
		| Equals(e1,e2) ->
			Bool ((val_of_exp e1 cxt) = (val_of_exp e2 cxt))
		| Less(e1,e2) ->
			let Int n1 = val_of_exp e1 cxt in
			let Int n2 = val_of_exp e2 cxt in
			Bool (n1 < n2)
		| Not e ->
			let Bool b = val_of_exp e cxt in
			Bool (not b)
		| _ -> exp
	with _ -> Fail


let exp_transducer =
	let ic = open_in_bin "exp.trans" in
	(Marshal.from_channel ic:Cfg.transducer);;



let rec sum = function
  e::[] -> e
| e::es -> Sum(e, sum es)
| _ -> raise Hell

let rec prod = function
  e::[] -> e
| e::es -> Prod(e, prod es)
| _ -> raise Hell

let char_of_tree = function
  Tree("EC", [TChar c]) -> c
| Tree("EC", [_; TChar c]) -> c
| _ -> raise Hell

let rec exp_of_tree et =
	match et with
	  Tree("INT", tl) -> Int (int_of_string (string_of_tchar_list tl))
	| Tree("CHAR", [t]) -> Char (char_of_tree t)
	| Tree("STR", tl) -> Str (String.concat "" (List.map (fun x -> String.make 1 (char_of_tree x)) tl))
	| Tree("UNIT", []) -> Unit
	| Tree("VAR", tl) -> let x = string_of_tchar_list tl in
		(match x with
		  "true" -> Bool true
		| "false" -> Bool false
		| _ -> Var x)
	| Tree("MINUS", [t]) -> Minus (exp_of_tree t)
	| Tree("SUM", tl) -> sum (List.map exp_of_tree tl)
	| Tree("PROD", tl) -> prod (List.map exp_of_tree tl)
	| Tree("EQUALS", [t1;t2]) -> Equals(exp_of_tree t1, exp_of_tree t2)
	| Tree("NEQ", [t1;t2]) -> Not(Equals(exp_of_tree t1, exp_of_tree t2))
	| Tree("LESS", [t1;t2]) -> Less(exp_of_tree t1, exp_of_tree t2)
	| Tree("LEQ", [t1;t2]) -> Not(Less(exp_of_tree t2, exp_of_tree t1))
	| Tree("GREATER", [t1;t2]) -> Less(exp_of_tree t2, exp_of_tree t1)
	| Tree("GEQ", [t1;t2]) -> Not(Less(exp_of_tree t1, exp_of_tree t2))
	| Tree("GETCHAR", [t1; t2]) -> GetChar (exp_of_tree t1, exp_of_tree t2)
	| Tree("LEN", [t]) -> Len (exp_of_tree t)
	| Tree("ATOI", [t]) -> Atoi (exp_of_tree t)
	| _ -> raise Hell

let exp_of_string str =
	let tree = Cfg.parse_tree str exp_transducer in
	print_endline (string_of_tree tree);
	match tree with
	  Tree("TOP", [et]) -> exp_of_tree et
	| _ -> raise Hell
;;

try if (Sys.argv.(0)="exp") then
	let rec read ()=
		try let str = read_line() in str ^ "\n" ^ read() with End_of_file -> "" in
	let str = read() in
	let exp = (exp_of_string str) in
	print_endline (string_of_exp exp);
	print_endline (string_of_exp (val_of_exp exp ["x", Int 3]))
else ()
with _ -> ()