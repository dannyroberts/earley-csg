type nont = string
type char_range = ((char * char) list)
type rexp = Eps | Char of char * bool | CharClass of char_range * bool | Nont of nont
	| Cat of rexp * rexp | Alt of rexp * rexp | Star of rexp
type rule = Rule of bool * nont * rexp
type cfg = rule list

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

let string_of_rexp (re:rexp) = 
	let rec string_of_rexp' (re:rexp) (bind:int) = 
		(* bind: Star->1,Cat->2,Alt->3 *)
		match re with
		  Eps -> "()"
		| Char (c,show) -> let p1,p2 = if show then "(",")" else "","" in
			p1 ^ (if inrange alphanum c then "" else "\\") ^ String.make 1 c ^ p2
		| CharClass (p,show) -> let p1,p2 =
			if show then "(",")" else "","" in p1 ^ (string_of_char_range p) ^ p2
		| Nont nt -> "{" ^ nt ^ "}"
		| Alt (re1,re2) -> let p1,p2 = if bind < 3 then "[","]" else "","" in
			p1 ^ (string_of_rexp' re1 3) ^ "|" ^ (string_of_rexp' re2 3) ^ p2
		| Cat (re1,re2) -> let p1,p2 = if bind < 2 then "[","]" else "","" in 
			p1 ^ (string_of_rexp' re1 2) ^ (string_of_rexp' re2 2) ^ p2
		| Star re -> string_of_rexp' re 1 ^ "*"
		(*| Plus re -> "(" ^ string_of_rexp re ^ ")+"
		| Opt re -> "(" ^ string_of_rexp re ^ ")?"*)
	in
	string_of_rexp' re 4


let string_of_rule = function Rule (show,nt,re) ->
	(if show then "" else ".") ^ nt ^ " ::= " ^ (string_of_rexp re)

let string_of_cfg (cfg:cfg) = 
	(String.concat "\n" (List.map string_of_rule cfg))


type branch = Trans of node ref * rexp | Call of node ref | Return of nont * bool
and node = {
	mutable eps_out: branch list;
	mutable char_out: branch list;
	mutable nont_out: branch list;
	mutable call_out: branch list;
	mutable return: branch list;
	mutable num: int
}
and parse = PInit | PTransEps of item | PTrans of item * char
	| PCall of item | PReturn of item * item * string * bool
and item = {start: int; state: node ref; parse: parse}

let add_branch n b =
	match b with
	  Trans (nr, re) -> (
		match re with
		  Eps -> n.eps_out <- b::n.eps_out
		| Char (c,_) -> n.char_out <- b::n.char_out
		| CharClass (p,_) -> n.char_out <- b::n.char_out
		| Nont nt -> n.nont_out <- b::n.nont_out
		| _ -> raise (Invalid_argument "branch re must be atomic")
	  )
	| Call nr -> n.call_out <- b::n.call_out
	| Return (nt,_) -> n.return <- b::n.return


let new_node bs =
	let n = {eps_out=[]; char_out=[]; nont_out=[]; call_out=[]; return=[]; num=0} in
	List.iter (add_branch n) bs;
	n

let branches n = n.eps_out @ n.char_out @ n.nont_out @ n.call_out @ n.return
let string_of_branch = function
  Trans (nr, re) -> (match re with
	  Char (c,_) -> "--" ^ string_of_rexp re ^ "---> " ^ string_of_int (!nr.num)
	| CharClass (p,_) -> "-" ^ string_of_rexp re ^ "-> " ^ string_of_int (!nr.num)
	| Eps -> "======> " ^ string_of_int (!nr.num)
	| Nont nt -> "-{" ^ nt ^ "}--> " ^ string_of_int (!nr.num)
	| _ -> raise (Invalid_argument "branch re must be atomic")
  )
| Call nr -> "-call-> " ^ string_of_int (!nr.num)
| Return (nt,true) -> "|-----> " ^ nt
| Return (nt,false)-> "|=====> " ^ nt



let string_of_node (n:node) =
	string_of_int n.num ^ " " ^ (
		String.concat
			("\n" ^ (string_of_int n.num) ^ " ")
			(List.map string_of_branch (branches n))
	) ^ "\n"



let string_of_item item = (string_of_int item.start) ^ " ~~~~> (" ^ (string_of_int !(item.state).num) ^ ")"
let string_of_earley_set items = String.concat "\n" (List.map string_of_item items)

type fragment = (node ref * node ref)
type transducer = {nodes: node ref list; s: nont}

let rec new_transducer (cfg:cfg) : transducer =
	let s_nt = Hashtbl.create 1 in
	List.iter (
		function Rule (show,nt,_) ->
			if Hashtbl.mem s_nt nt
			then raise (Invalid_argument "cfg with reduntant rules")
			else Hashtbl.add s_nt nt (ref (new_node []), show)
		) cfg;
	let find_start nt = try (let x,_ = Hashtbl.find s_nt nt in x) with Not_found -> raise (Invalid_argument nt) in
	let find_start_show nt = try (Hashtbl.find s_nt nt) with Not_found -> raise (Invalid_argument nt) in
	let rec makeFrag (re:rexp): fragment =
		let f = ref (new_node []) in
		match re with
	  	  Eps -> (f,f)
		| Char _ | CharClass _ -> (ref (new_node [Trans (f, re)]),f)
		(*| CharClass p -> (ref (new_node [Trans (f, CharClass p)]),f)*)
		| Nont nt ->
			(ref (new_node [Trans (f, Nont nt); Call (find_start nt)]),f)
		| Cat (re1, re2) -> 
			let (s1,f1) = makeFrag re1 in
			let	(s2,f2) = makeFrag re2 in
			f1 := !s2;
			(s1,f2)
		| Alt (re1,re2) ->
			let (s1,f1) = makeFrag re1 in
			let (s2,f2) = makeFrag re2 in
			!f1.eps_out <- [Trans (f,Eps)];
			!f2.eps_out <- [Trans (f,Eps)];
			(ref (new_node [Trans(s1,Eps);Trans(s2,Eps)]),f)
		| Star re ->
			let (s',f') = makeFrag re in
			!f'.eps_out <- [Trans (s',Eps)];
			!s'.eps_out <- Trans(f,Eps)::!s'.eps_out;
			(s',f)
	in
	let getNodeRefs (n:node) =
		let i = ref 1 in
		let rec getNodeRefs' (n:node) (rlist:node ref list) =
			let rec fromBS (bs:branch list) (rlist:node ref list) =
				let doit (nr:node ref) (bs':branch list) = 
					let rlist' = fromBS bs' rlist in
					if !nr.num = 0 then getNodeRefs' !nr rlist'
					else rlist'
				in
				match bs with
				  [] -> rlist
				| Trans (nr, _)::bs' -> doit nr bs'
				| Call nr::bs' -> doit nr bs'
				| Return _::bs' -> fromBS bs' rlist
			in
			n.num <- !i;
			i := !i+1;
			fromBS (branches n) (ref n::rlist)
		in
		List.rev (getNodeRefs' n [])
	in
	match cfg with
	  [] -> raise (Invalid_argument "cfg must have at least one rule")
	| Rule(show,start,_)::_ -> List.iter (
		function Rule(_,nt,re) ->
			let (s,f) = makeFrag re in
			let x,show = find_start_show nt in
			(*!f.bs <- (Return nt)::!f.bs;*)
			add_branch !f (Return (nt,show));
			x := !s
		) cfg;
		{nodes=getNodeRefs !(find_start start); s=start}


let string_of_transducer (transducer:transducer) = 
	(String.concat "\n" (List.map (fun x -> string_of_node !x) transducer.nodes))


let earley_parse str trans =
	let earley_sets = Array.make (String.length str +1) [] in
	let visited = Hashtbl.create 1 in
	let unvisited item_x j = not (Hashtbl.mem visited (item_x.start, j, !(item_x.state).num)) in
	let rec populate j = 
		let rec add_item item_x j =
			if not (unvisited item_x j) then raise (Invalid_argument "already visited") else
			Hashtbl.add visited (item_x.start, j, !(item_x.state).num) ();
			earley_sets.(j) <- item_x::earley_sets.(j);
			(*print_endline ((string_of_item item_x) ^ "\t" ^ (String.sub str 0 j));*)
			if complete item_x then (); ()
		and reach (item:item) =
			let rec reach_bs (bs:branch list) = 
				match bs with
				  [] -> false
				| Trans(x, Eps)::bs' ->
					let item_x = {start=item.start; state=x; parse=PTransEps(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j then (
						add_item item_x j;
						true
					) else change
				| Call x::bs' ->
					let item_x = {start=j; state=x; parse=PCall(item)} in
					let change = reach_bs bs' in
					if unvisited item_x j then (
						add_item item_x j;
						true
					) else
						change
				| _ -> raise (Invalid_argument "reach requires a call or a null transition")
			in
			let change1 = reach_bs !(item.state).eps_out in
			let change2 = reach_bs !(item.state).call_out in
			change1 || change2
		and complete (item:item) = 
			let rec complete_bs (bs:branch list) = 
				match bs with
				  [] -> false
				| Return (nt,show)::bs' ->
					List.fold_left (function change -> function item_i ->
						List.fold_left  (function change -> function Trans(x, Nont nt') ->
							if nt = nt' then (
								let item_x = {start=item_i.start; state=x; parse=PReturn(item_i,item,nt,show)} in
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
				let doit x show=
					let	item_x = {
						start=item.start;
						state=x;
						parse=if show then PTrans(item,str.[j-1]) else PTransEps item
					} in
					if unvisited item_x j then (
						add_item item_x j;
					);
				in
				match bs with
				  [] -> ()
				| Trans(x,Char (c,show))::bs' when c=str.[j-1] -> doit x show; scan_bs bs'
				| Trans(x,CharClass (p,show))::bs' when (inrange p str.[j-1]) -> doit x show; scan_bs bs'
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
		earley_sets.(0) <- [{start=0; state=first; parse=PInit}];
		populate 0;
		earley_sets
	| _ -> raise (Invalid_argument "empty trans")


module Tree =
  struct
	type tree = Tree of nont * tree list | TChar of char
	let string_of_tchar_list tchars=
		String.concat "" (
			List.map (
				function
				  (TChar c) -> String.make 1 c
				| _ -> raise (Invalid_argument "not a TChar")
			) tchars
		)

	let string_of_tchar_tree head tree =
		match tree with
		  Tree(head',tchars) when head=head'-> string_of_tchar_list tchars
		| _ -> raise (Invalid_argument "string_of_tchar_tree")
		
	
	let string_of_tree tree = 
		let tab = "   " in
		let rec string_of_tree' dstr tree=
			match tree with
			  Tree(nt, trees) -> "{" ^ nt ^ "}" ^ "\n"
				^ tab ^ dstr ^ (String.concat ("\n"
				^ tab ^ dstr) (List.map (string_of_tree' (tab ^ dstr)) trees))
			| TChar c -> String.make 1 c
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
		  PReturn (item_i', item_j', nt, true) -> 
				(Tree (nt, List.rev (build_tree item_j')))::(build_tree item_i')
		| PReturn (item_i', item_j', nt, false) ->
				(build_tree item_j') @ (build_tree item_i')
		| PTransEps item_j' -> build_tree item_j'
		| PTrans (item_j', c) -> (TChar c)::(build_tree item_j')
		| PCall (item_j') -> []
		| PInit -> []
	in
	let root = find_root final_set in
	Tree(trans.s, List.rev (build_tree root))


let space_re = Star(CharClass (space,false))
let alpha_re = CharClass (alpha,true)
let alphanum_re = CharClass (alphanum,true)
let any_re = CharClass (any,true)
let charf c = Char (c,false)
let chart c = Char (c,true)
let rulet nt re = Rule (true,  nt, re)
let rulef nt re = Rule (false, nt, re)

let rec alt = function
  re::[] -> re
| re::res -> Alt(re, alt res)
| _ -> raise (Invalid_argument "alt")
let rec cat = function
  re::[] -> re
| re::res -> Cat(re, cat res)
| _ -> raise (Invalid_argument "cat")

let cfg_transducer =
	try let ic = open_in_bin "cfg.trans" in
	(Marshal.from_channel ic:transducer) with _ -> {nodes=[];s=""}
	
let nont_of_tree = function Tree("NT", tchars) ->
		string_of_tchar_list tchars
	| _ -> raise (Invalid_argument "nont_of_tree")	

let rec	rexp_of_tree show tree =
		match tree with
		  Tree("EPS", []) -> Eps
		| Tree("C", [TChar c]) -> if c='.' then CharClass (any,show) else Char (c,show)
		| Tree("C", [_; TChar c]) -> 
			(match c with
			  's' -> CharClass (space,show)
			| 'w' -> CharClass (alphanum,show)
			| 'a' -> CharClass (alpha,show)
			| 'd' -> CharClass (num,show)
			| _ -> Char (c,show))
		| Tree("SPACE", _) -> space_re
		| Tree("CAT", re_trees) -> cat (List.map (rexp_of_tree show) re_trees)
		| Tree("ALT", re_trees) -> alt (List.map (rexp_of_tree show) re_trees)
		| Tree("NONT", [nt_tree]) -> Nont (nont_of_tree nt_tree)
		| Tree("UNY", [re_tree; TChar '*']) -> Star (rexp_of_tree show re_tree)
		| Tree("UNY", [re_tree; TChar '+']) -> let re = rexp_of_tree show re_tree in Cat(re, Star(re))
		| Tree("UNY", [re_tree; TChar '?']) -> let re = rexp_of_tree show re_tree in Alt(re, Eps)
		| Tree("SAVE", [re_tree]) -> rexp_of_tree true re_tree
		| _ -> raise (Invalid_argument "rexp_of_tree")

let cfg_of_string str = 
	match parse_tree str cfg_transducer with
	  Tree("CFG",trees) ->
		(*print_endline (string_of_tree (Tree("CFG",trees)));*)
		let rec cfg_of_tree_list rule_trees =
			match rule_trees with
			  [] -> []
			| Tree("RULE", [nt_tree; Tree("R", [re_tree])])::rule_trees' ->
				Rule(true, nont_of_tree nt_tree, rexp_of_tree false re_tree)::(cfg_of_tree_list rule_trees')
			| Tree("RULE", [TChar '.'; nt_tree; Tree("R", [re_tree])])::rule_trees' ->
				Rule(false, nont_of_tree nt_tree, rexp_of_tree false re_tree)::(cfg_of_tree_list rule_trees')
			| _ -> raise (Invalid_argument "cfg_of_tree_list")
		in
		cfg_of_tree_list trees
	| _ -> raise (Invalid_argument "cfg_of_string")