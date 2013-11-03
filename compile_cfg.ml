open Cfg

let rec readfile ic =
try let str = input_line ic in str ^ "\n" ^ (readfile ic) with End_of_file -> "";;

let compile_exp_spec () = 
	let exp_transducer =
		let exp_str =
			readfile (open_in Sys.argv.(1))
		in
		let exp_cfg =
			cfg_of_string exp_str
		in
		new_transducer exp_cfg
	in
	let oc =
		open_out_bin Sys.argv.(2)
	in
	Marshal.to_channel oc exp_transducer [];;

compile_exp_spec ();;