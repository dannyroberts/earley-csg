open Cfg.Tree

exception Hell

let csb_transducer =
	let ic = open_in_bin "csb.trans" in
	(Marshal.from_channel ic:Cfg.transducer);;

type nont = string
type var = string
type char_range = ((char * char) list)
type exp = Exp.exp
	
type rhs = Eps | Char of char | CharClass of char_range | Nont of nont * exp
	| Cat of rhs * rhs | Alt of rhs * rhs | Star of rhs
	| Capture of rhs * var | Assert of exp | Set of var * exp | Show of var
	
type rule = Rule of bool * nont * rhs
type csb = rule list

let alpha = ['A','Z';'a','z']
let alphanum = ['A','Z';'a','z';'0','9']
let num = ['0','9']
let space = ['\n','\n';'\t','\t';' ',' ']
let any = [Char.chr 0, Char.chr 255]

let rec inrange (range:char_range) (c:char) =
	match range with
	  [] -> false
	| (a,z)::range' -> if a <= c && c <= z then true else (inrange range' c)

let rec string_of_char_range range =
	match range with
	  p when p = alphanum -> "\\w"
	| p when p = alpha -> "\\a"
	| p when p = num -> "\\d"
	| p when p = space -> "\\s"
	| p when p = any -> "."
	| _ -> "{" ^ (
		String.concat ";" (
			List.map (function (a,z) -> (String.make 1 a) ^ "-" ^ (String.make 1 z)) range
		)
	) ^ "}"


let string_of_rhs (re:rhs) = 
	let rec string_of_rhs' (re:rhs) (bind:int) = 
		(* bind: Star->1,Cat->2,Alt->3,Capture/Set->4*)
		match re with
		  Eps -> "()"
		| Char c ->
			begin
				match c with
				  _ when inrange alphanum c -> String.make 1 c
				| '\n' -> "\\n"
				| '\t' -> "\\t"
				| _ -> "\\" ^ String.make 1 c
			end
		| CharClass p -> (string_of_char_range p)
		| Nont (nt,e) -> "{" ^ nt ^ (
				if e=Exp.Unit then "" else "(" ^ (Exp.string_of_exp e) ^ ")"
			) ^ "}"
		| Alt (re1,re2) -> let p1,p2 = if bind < 3 then "(",")" else "","" in
			p1 ^ (string_of_rhs' re1 3) ^ "|" ^ (string_of_rhs' re2 3) ^ p2
		| Cat (re1,re2) -> let p1,p2 = if bind < 2 then "(",")" else "","" in 
			p1 ^ (string_of_rhs' re1 2) ^ (string_of_rhs' re2 2) ^ p2
		| Star re -> string_of_rhs' re 1 ^ "*"
		| Capture (re,x) -> let p1,p2 = if bind < 4 then "(",")" else "","" in
			p1 ^ (string_of_rhs' re 4) ^ "@" ^ x ^ p2
		| Set (x,e) -> let p1,p2 = if bind < 4 then "(",")" else "","" in
			p1 ^ x ^ "=" ^ Exp.string_of_exp e ^ p2
		| Assert e -> "[" ^ Exp.string_of_exp e ^ "]"
		| Show var -> let p1,p2 = if bind < 4 then "(",")" else "","" in p1 ^ "$" ^ var ^ p2
		(*| Plus re -> "(" ^ string_of_rhs re ^ ")+"
		| Opt re -> "(" ^ string_of_rhs re ^ ")?"*)
	in
	string_of_rhs' re 10

let string_of_rule = function Rule (show,nt,re) ->
	(if show then "" else ".") ^ nt ^ " ::= " ^ (string_of_rhs re)

let string_of_csb (csb:csb) = 
	(String.concat "\n" (List.map string_of_rule csb))

let rec rhs_of_tree tree =
	match tree with Tree(head,trees) -> (
	match (head,trees) with
	  "CAPTURE", [rhs_t; var_t] -> Capture(rhs_of_tree rhs_t, string_of_tchar_tree "VAR" var_t)
	| "SET", [var_t; exp_t] -> Set(string_of_tchar_tree "VAR" var_t, Exp.exp_of_tree exp_t)
	| "ASSERT", [exp_t] -> Assert(Exp.exp_of_tree exp_t)
	| "EPS", [] -> Eps
	| "NONT", [nont_t; exp_t] -> Nont(string_of_tchar_tree "NT" nont_t, Exp.exp_of_tree exp_t)
	| "NONT", [nont_t] -> Nont(string_of_tchar_tree "NT" nont_t, Exp.Unit)
	| "SAVE", [rhs_t] -> Cat(Capture(rhs_of_tree rhs_t, "$"), Show ("$"))
	| "SHOW", [var_t] -> Show(string_of_tchar_tree "VAR" var_t)
	| "UNY", [rhs_t; TChar c] -> let rhs = rhs_of_tree rhs_t in begin
		match c with
		  '*' -> Star(rhs)
		| '+' -> Cat(rhs, Star(rhs))
		| '?' -> Alt(rhs, Eps)
		| _ -> raise Hell
	end
	| "CAT", rhs_t::rhs_trees ->
		List.fold_left (fun x y -> Cat(x, rhs_of_tree y)) (rhs_of_tree rhs_t) rhs_trees
	| "ALT", rhs_t::rhs_trees ->
		List.fold_left (fun x y -> Alt(x, rhs_of_tree y)) (rhs_of_tree rhs_t) rhs_trees
	| "C", [TChar c] -> if c='.' then CharClass(any) else Char (c)
	| "C", [_; TChar c] -> 
		(match c with
		  's' -> CharClass (space)
		| 'w' -> CharClass (alphanum)
		| 'a' -> CharClass (alpha)
		| 'd' -> CharClass (num)
		| _ -> Char (c))
	| "SPACE", _ -> Star(CharClass space)
	| _ -> raise Hell
	) | _ -> raise Hell
	
let rule_of_tree tree =
	match tree with
	  Tree("RULE", [nt_tree; rhs_tree]) ->
		Rule(true,  string_of_tchar_tree "NT" nt_tree, rhs_of_tree rhs_tree)
	| Tree("RULE", [TChar '.'; nt_tree; rhs_tree]) ->
		Rule(false, string_of_tchar_tree "NT" nt_tree, rhs_of_tree rhs_tree)
	| _ -> raise Hell
	
let csb_of_string str =
	let tree = Cfg.parse_tree str csb_transducer in
	(*print_endline (string_of_tree tree);*)
	match tree with
	  Tree("CSB", rule_trees) -> List.map rule_of_tree rule_trees
	| _ -> raise Hell




type branch = Trans of node ref * rhs | Call of node ref * exp | Return of nont * bool |
	Push of node ref | Pop of node ref * var
	
and node = {
	mutable char_out: branch list;
	mutable nont_out: branch list;
	mutable return: branch list;
	mutable null_out: branch list;
	mutable num: int
}
and parse = PInit | PTransEps of item | PShow of item * exp |
	PCall | PReturn of item * item * nont * exp * bool
and item = {start: int; state: node ref; context: (var * exp) list; capture: int list; parse: parse}

let rec find context x =
	match context with
	  [] -> Exp.Unit
	| (x', e)::cxt when x=x' -> e
	| _::cxt -> find cxt x


let add_branch n b =
	match b with
	  Trans (nr, re) -> (
		match re with
		  Eps | Assert _ | Set _ | Show _ -> n.null_out <- b::n.null_out
		| Char _ -> n.char_out <- b::n.char_out
		| CharClass _ -> n.char_out <- b::n.char_out
		| Nont _ -> n.nont_out <- b::n.nont_out
		| _ -> raise (Invalid_argument "branch re must be atomic")
	  )
	| Call _ | Push _ | Pop _ -> n.null_out <- b::n.null_out
	| Return _ -> n.return <- b::n.return


let new_node bs =
	let n = {null_out=[]; char_out=[]; nont_out=[]; return=[]; num=0} in
	List.iter (add_branch n) bs;
	n

let branches n = n.null_out @ n.char_out @ n.nont_out @ n.return
let string_of_branch = function
  Trans (nr, rhs) -> "--" ^ string_of_rhs rhs ^ "--> " ^ string_of_int !nr.num
| Call (nr,e) -> "-call-" ^
	(if e=Exp.Unit then "" else "(" ^ Exp.string_of_exp e ^ ")")
	^ "-> " ^ string_of_int !nr.num
| Return (nt,_) -> "|-----> " ^ nt
| Push nr -> "-push-> " ^ string_of_int !nr.num
| Pop (nr,y) -> "-pop-" ^ y ^ "->" ^ string_of_int !nr.num

let string_of_node (n:node) =
	string_of_int n.num ^ " " ^ (
		String.concat
			("\n" ^ (string_of_int n.num) ^ " ")
			(List.map string_of_branch (branches n))
	) ^ "\n"

let string_of_item item =
	(string_of_int item.start) ^ " ~~~~> (" ^ (string_of_int !(item.state).num) ^ ") " ^
	"[" ^ (String.concat "; " (List.map (function (var,e) -> var ^ "," ^ Exp.string_of_exp e) item.context)) ^ "] " ^
	"[" ^ (String.concat "; " (List.map string_of_int item.capture)) ^ "]"
let string_of_earley_set items = String.concat "\n" (List.map string_of_item items)

type fragment = (node ref * node ref)
type transducer = {nodes: node ref list; s: nont}

let string_of_transducer (transducer:transducer) = 
	(String.concat "\n" (List.map (fun x -> string_of_node !x) transducer.nodes))

let rec transducer_of_csb (csb:csb) : transducer =
	let cs_tbl = Hashtbl.create 1 in (*callee-show table*)
	List.iter (
		function Rule (show,nt,_) ->
			if Hashtbl.mem cs_tbl nt then raise (Invalid_argument nt)
			else Hashtbl.add cs_tbl nt (ref (new_node []), show)
	) csb;
	let callee_of_nont nt =
		try let x,_ = Hashtbl.find cs_tbl nt in x
		with Not_found -> raise (Invalid_argument nt)
	in
	let callee_show_of_nont nt =
		try Hashtbl.find cs_tbl nt
		with Not_found -> raise (Invalid_argument nt) in
	let rec frag_of_rhs rhs : fragment =
		let f = ref (new_node []) in
		match rhs with
		  Eps -> (f,f)
		| Char _ | CharClass _ | Assert _ | Set _ | Show _ ->
			(ref (new_node [Trans (f, rhs)]), f)
		| Nont (nt,e) -> (ref (new_node [Trans (f,rhs); Call (callee_of_nont nt, e)]), f)
		| Cat (rhs1, rhs2) ->
			let [(s1,f1); (s2,f2)] = List.map frag_of_rhs [rhs1; rhs2] in
			f1 := !s2;
			(s1,f2)
		| Alt (rhs1, rhs2) ->
			let [(s1,f1); (s2,f2)] = List.map frag_of_rhs [rhs1; rhs2] in
			!f1.null_out <- [Trans (f,Eps)];
			!f2.null_out <- [Trans (f,Eps)];
			(ref (new_node [Trans (s1,Eps); Trans (s2,Eps)]), f)
		| Star rhs' ->
			let (s',f') = frag_of_rhs rhs' in
			!f'.null_out <- [Trans (s', Eps)];
			add_branch !s' (Trans(f,Eps));
			(s',f)
		| Capture (rhs', var) -> 
			let (s', f') = frag_of_rhs rhs' in
			!f'.null_out <- [Pop (f,var)];
			(ref (new_node [Push s']), f)
	in
	let getNodeRefs (n:node) =
		let i = ref 1 in
		let rec getNodeRefs' (n:node) (rlist:node ref list) =
			let rec fromBS (bs:branch list) (rlist:node ref list) =
				match bs with
				  [] -> rlist
				| Trans (nr, _)::bs' | Call (nr,_)::bs' | Push nr::bs' | Pop (nr,_)::bs' ->
					let rlist' = fromBS bs' rlist in
					if !nr.num = 0 then getNodeRefs' !nr rlist'
					else rlist'
				| Return _::bs' -> fromBS bs' rlist
			in
			n.num <- !i;
			i := !i+1;
			fromBS (branches n) (ref n::rlist)
		in
		List.rev (getNodeRefs' n [])
	in
	match csb with
	  [] -> raise (Invalid_argument "csb must have at least one rule")
	| Rule(show,start,_)::_ -> List.iter (
		function Rule(_,nt,rhs) ->
			let (s,f) = frag_of_rhs rhs in
			let x,show = callee_show_of_nont nt in
			add_branch !f (Return (nt,show));
			x := !s
		) csb;
		{nodes=getNodeRefs !(callee_of_nont start); s=start}




let earley_parse str trans =
	let earley_sets = Array.make (String.length str +1) [] in
	let visited = Hashtbl.create 1 in
	let unvisited item_x j = not (Hashtbl.mem visited (item_x.start, !(item_x.state).num, item_x.context, item_x.capture, j)) in
	let rec populate j = 
		let rec add_item item_x j =
			if not (unvisited item_x j) then raise (Invalid_argument "already visited") else
			Hashtbl.add visited (item_x.start, !(item_x.state).num, item_x.context, item_x.capture, j) ();
			earley_sets.(j) <- item_x::earley_sets.(j);
			(*print_endline ((string_of_item item_x) ^ "\t" ^ (string_of_int j) ^ ": " ^ (String.sub str 0 j));*)
			if complete item_x then (); ()
		and reach (item:item) =
			let rec reach_bs (bs:branch list) = 
				match bs with
				  [] -> false
				| Trans(x, Show var)::bs' ->
					let item_x = {item with state=x; parse=PShow(item, Exp.val_of_exp (Exp.Var var) item.context)} in
					let change = reach_bs bs' in
					if unvisited item_x j then (add_item item_x j; true) else change
				| Trans(x, Eps)::bs' ->
					let item_x = {item with state=x; parse=PTransEps(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j then (add_item item_x j; true) else change
				| Trans(x, Set(var,e))::bs' ->
					let v = Exp.val_of_exp e item.context in
					let cxt = (var, v)::item.context in
					let item_x = {item with state=x; context=cxt; parse=PTransEps(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j && v != Exp.Fail then (add_item item_x j; true) else change
				| Trans(x, Assert e)::bs' ->
					let b =
						match Exp.val_of_exp e item.context with
						  Exp.Bool b -> b
						| _ -> false
					in
					let item_x = {item with state=x; parse=PTransEps(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j && b then (add_item item_x j; true) else change
				| Call (x,e)::bs' ->
					let v = Exp.val_of_exp e item.context in
					let item_x = {item with start=j; state=x; context=[("arg",v)]; capture=[]; parse=PCall} in
					let change = reach_bs bs' in
					if unvisited item_x j && v != Exp.Fail then (
						add_item item_x j;
						true
					) else change
				| Push x:: bs' ->
					let item_x = {item with state=x; capture=j::item.capture; parse=PTransEps(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j then (
						add_item item_x j;
						true
					) else change
				| Pop (x,var)::bs' ->
					let i::cap = item.capture in
					let cxt = (var, Exp.Str (String.sub str i (j-i)))::item.context in
					let item_x = {item with state=x; capture=cap; context=cxt; parse=PTransEps(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j then (
						add_item item_x j;
						true
					) else change
				| _ -> raise (Invalid_argument "reach requires a call or a null transition")
			in
			reach_bs !(item.state).null_out
		and complete (item:item) = 
			let v = find item.context "arg" in
			let rec complete_bs (bs:branch list) = 
				match bs with
				  [] -> false
				| Return (nt,show)::bs' ->
					List.fold_left (function change -> function item_i ->
						List.fold_left  (function change -> function Trans(x, Nont (nt',e')) ->
							let v' = Exp.val_of_exp e' item_i.context in
							if nt = nt' && v = v' then (
								let item_x = {item_i with state=x; parse=PReturn(item_i,item,nt,v,show)} in
								if unvisited item_x j then (
									add_item item_x j;
									true
								) else false
							) else false
							| _ -> raise (Invalid_argument "complete")
						) change !(item_i.state).nont_out
					) false earley_sets.(item.start)
				| _ -> raise (Invalid_argument "complete")
			in
			let change1 = complete_bs !(item.state).return in
			let change2 = reach item in
			change1 || change2
		in
		let scan (item:item) = (* seed set j using items from set j-1 *)
			let rec scan_bs (bs:branch list) =
				let doit x=
					let	item_x = {item with
						state=x;
						parse=PTransEps item
					} in
					if unvisited item_x j then (
						add_item item_x j;
					);
				in
				match bs with
				  [] -> ()
				| Trans(x,Char (c))::bs' when c=str.[j-1] -> doit x; scan_bs bs'
				| Trans(x,CharClass (p))::bs' when (inrange p str.[j-1]) -> doit x; scan_bs bs'
				| _::bs' -> scan_bs bs'
			in
			scan_bs !(item.state).char_out
		in
		if j <= String.length str then (
			if j > 0 then List.iter scan earley_sets.(j-1);
			List.iter (function item -> while complete item do () done) earley_sets.(j);
			populate (j+1)
		)

	in
	match trans.nodes with first::_ ->
		earley_sets.(0) <- [{start=0; state=first; context=[]; capture=[]; parse=PInit}];
		populate 0;
		earley_sets
	| _ -> raise (Invalid_argument "empty trans")


module Tree =
  struct
	type tree = Tree of nont * exp * tree list | Leaf of exp
	let string_of_leaf_list tchars=
		String.concat "" (
			List.map (
				function
				  (Leaf e) -> Exp.string_of_exp e
				| _ -> raise (Invalid_argument "not a Leaf")
			) tchars
		)
	
	let string_of_tree tree = 
		let tab = "   " in
		let rec string_of_tree' dstr tree=
			match tree with
			  Tree(nt, e, trees) -> "{" ^ nt ^ " " ^ Exp.string_of_exp e ^ "}" ^ "\n"
				^ tab ^ dstr ^ (String.concat ("\n"
				^ tab ^ dstr) (List.map (string_of_tree' (tab ^ dstr)) trees))
			| Leaf e -> Exp.string_of_exp e
		in
		string_of_tree' "" tree

  end

open Tree

let parse_tree str trans =
	let final_set = (earley_parse str trans).(String.length str) in
	let rec find_root set =
		match set with
		  [] -> raise (Invalid_argument ("parse_tree: cannot parse string: " ^ str))
		| item::set' ->
			if item.start = 0 &&
				((List.mem (Return (trans.s, true )) !(item.state).return) ||
				( List.mem (Return (trans.s, false)) !(item.state).return))
			then
				item
			else
				find_root set'
	in
	let rec build_tree item_j =
		match item_j.parse with
		  PReturn (item_i', item_j', nt, v, true) -> 
				(Tree (nt, v, List.rev (build_tree item_j')))::(build_tree item_i')
		| PReturn (item_i', item_j', _, _, false) ->
				(build_tree item_j') @ (build_tree item_i')
		| PTransEps item_j' -> build_tree item_j'
		| PShow (item_j', v) -> (Leaf v)::(build_tree item_j')
		| PCall -> []
		| PInit -> []
	in
	let root = find_root final_set in
	Tree(trans.s, Exp.Unit, List.rev (build_tree root))


;;


if Sys.argv.(0) = "csb" then
	try
		let n = int_of_string Sys.argv.(1) in
		(*let csb_str n =
			let rec csb_str' n =
				match n with
				  0 -> ""
				| _ -> "a?" ^ (csb_str' (n-1)) ^ "a"
			in
			"A = " ^ csb_str' n ^ ";"
		in
		let rec str n =
			String.make n 'a'
		in
		let csb = csb_of_string (csb_str n) in*)
		let rec str n =
			match n with
			  _ when n <=1 -> "a a"
			| _ -> str (n-1) ^ String.make (n-1) 'a' ^ " a"
		in
		let csb = csb_of_string ("double = .*.*.*;") in
		let td = transducer_of_csb csb in
		let code = str n ^ "\n" in
		let tree = parse_tree code td in
		if (string_of_tree tree) = "" then () else ()
	with _ ->
		let read_file file =
			let rec read_ic ic =
				try let str = input_line ic in str ^ "\n" ^ read_ic ic with End_of_file -> ""
			in
			read_ic (open_in_bin file)
		in
		let rec read ()=
			try let str = read_line() in str ^ "\n" ^ read() with End_of_file -> ""
		in
		let str = read_file "csb_example.txt" in
		let csb = csb_of_string str in
		let td = transducer_of_csb csb in
		print_endline (string_of_csb csb);
		print_endline (string_of_transducer td);
		let code = read () in
		(*for i = 0 to String.length code do
			print_endline (string_of_int i);
			print_endline (string_of_earley_set (earley_parse code td).(i));
		done;*)
		let tree = parse_tree code td in
		print_endline (string_of_tree tree)
else ()

