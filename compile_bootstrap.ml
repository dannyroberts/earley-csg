open Cfg

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

let cfg = [
	rulet "CFG" (cat[space_re; Star(Nont "RULE")]);
	rulet "RULE" (cat [alt [chart '.'; Eps]; Nont "NT"; space_re; charf '='; Nont "R"; charf ';'; space_re]);
	rulet "R" (alt [Nont "R1"; Nont "R2"; Nont "R3"]);
	rulef "R1" (alt [Nont "ATOM"; Nont "UNY"; Nont "CAT"; Nont "ALT"]);
	rulef "R2" (alt [Nont "ATOM"; Nont "UNY"; Nont "CAT"]);
	rulef "R3" (alt [Nont "ATOM"; Nont "UNY"]);
	rulef "ATOM" (alt [Nont "EPS"; Nont "C"; Nont "SPACE"; Nont "PAD"; Nont "NONT"; Nont "PAREN"; Nont "SAVE"]);
	rulet "EPS" (cat [charf '('; charf ')']);
	rulet "NONT" (cat [charf '{'; Nont "NT"; charf '}']);
	rulef "PAREN" (cat [charf '('; Nont "R1"; charf ')']);
	rulet "SAVE" ( cat [charf '$'; charf '('; Nont "R1"; charf ')']);
	rulet "UNY" (cat [Nont "R3"; alt [chart '*'; chart '?'; chart '+']]);
	rulet "CAT" (cat [Nont "R3"; Nont "R2"; Star(Nont "R2")]);
	rulet "ALT" (cat [Nont "R2"; charf '|'; Nont "R2"; Star(cat [charf '|'; Nont "R2"])]);
	rulet "C" (alt [alphanum_re; chart '.'; Cat(chart '\\', any_re)]);
	rulet "SPACE" (charf '_');
	rulef "PAD" (cat [CharClass (space,false); Star(CharClass (space,false))]);
	rulet "NT" (cat [alpha_re; Star(alt[alphanum_re; chart '_'])]);
]
let cfg_transducer = new_transducer cfg
let oc = open_out_bin "cfg.trans";;
Marshal.to_channel oc cfg_transducer []