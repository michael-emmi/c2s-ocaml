(** A few functions to make parsing slightly simpler. *)

open Prelude
open Printf

exception Parse_error of int * int * int * int * string

let parse_string psr lxr str =
	try
		let pgm = psr lxr (Lexing.from_string str) in
		pgm
	with Parse_error (sl,sc,el,ec,_) -> begin
		let start_pos = (String.nth_index str (sl-1) '\n') + 1
		and end_pos = String.nth_index str el '\n' in
		let s = String.sub str start_pos end_pos in
		eprintf "Parse error between %n::%n and %n::%n in %s\n"
			sl sc el ec s;
    eprintf "%s^%s^\n" (String.make (max 0 (sc-1)) ' ') (String.make (max 0 (ec-sc-1)) '-');
		failwith "parse error"
	end
	
let parse_file psr lxr src_file =
	let src = open_in src_file in
	let lexbuf = Lexing.from_channel src in
	lexbuf.Lexing.lex_curr_p <- {
		lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = src_file
	};

	try
		let pgm = psr lxr lexbuf in
		close_in src;
		pgm
	with
	| Parse_error (sl,sc,el,ec,_) -> begin
		  close_in src;
		  let src = open_in src_file in
		  let s = ref "" in
		  let ln = ref 1 in

		  while !ln < sl do
			  ignore (input_line src);
			  incr ln
		  done;

		  while !ln <= el do
			  let line = input_line src in
			  s := !s ^ "\n" ^ line;
			  incr ln
		  done;
		  close_in src;

		  eprintf "Parse error between %n::%n and %n::%n in%s\n"
			  sl sc el ec !s;
      eprintf "%s^%s^\n" (String.make (max 0 (sc-1)) ' ') (String.make (max 0 (ec-sc-1)) '-');
		  failwith "parse error"
	  end
