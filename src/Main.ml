(** The top-level entry point. *)

open Prelude
open Printf

module PP = PrettyPrinting
open PP

type ast = 
	| CP of CpAst.Program.t
	| BP of BpAst.Program.t
	| BPL of BplAst.Program.t
	| PN of PnAst.Program.t

let print_to_file f d =
	match f with
	| "-" -> output_string stdout << render <| d;
	| _ -> begin
		  let oc = open_out f in
		  output_string oc << render <| d;
		  close_out oc
	  end
	
let static_options = [
	"back-end" ;
	"boogie-version" ;
	"only-boolean" ;
	"optimize-phases" ;
	"asserts-throw-exn" ;
	"ensure-well-formed" ;

	"reach-slic-error" ;
	"one-label-per-stmt" ;
	"one-target-per-goto" ;
	"dont-ignore-returns" ;
	"no-dead-stmts" ;
	"no-constrain-clauses"
]
	
let flags_file_skip_line_re = Str.regexp "^\\(#.*\\|\\)$"
	
let read_flags_file f =
	if not (Sys.file_exists f) then
		failwith (sprintf "Cannot find file `%s'." f);
	let lines = ref [] in
	let chan = open_in f in
	try while true; do
		let line = input_line chan in
		if not (Str.string_match flags_file_skip_line_re line 0) then
			lines := line :: !lines
		done; []
	with End_of_file ->
		close_in chan;
	  	List.rev !lines
	
let filename_re = Str.regexp "^[^-].*\\.\\(cp\\|bp\\|bpl\\|spec\\)$"
let argument_re = Str.regexp "^-\\(.*\\)$"
let flagsfile_re = Str.regexp "^flags \\(.*\\)$"
	
let parse_options =
	List.flatten
	<< List.map (fun opt -> 
		if Str.string_match flagsfile_re opt 0 then
			read_flags_file (Str.matched_group 1 opt)
		else [ opt ]
		)
	<< List.rev 
	<< List.fold_left (fun opts arg -> 		
		if Str.string_match argument_re arg 0 then
			(Str.matched_group 1 arg) :: opts			
		else begin match opts with
		| [] -> failwith <| sprintf "Unexpected argument parameter `%s'." arg
		| op::opts -> (op ^ " " ^ arg) :: opts
		end )
		[]

	<< List.tl
	<< List.tl
	<< Array.to_list
	<| Sys.argv

	
let parse_program src = 
	
	if Filename.check_suffix src ".cp" then begin
		
		let p = ParsingUtils.parse_file CpParser.program_top CpLexer.token src in
		
		if Option.is_none (CpAst.Program.find_proc p "main") then
			failwith "Missing procedure `main'.";
			
		CP (p)		
	end

	else if Filename.check_suffix src ".bp" then
		BP (ParsingUtils.parse_file BpParser.program_top BpLexer.token src)

	else if Filename.check_suffix src ".bpl" then
		BPL (ParsingUtils.parse_file BplParser.program_top BplLexer.token src)
		
	else if Filename.check_suffix src ".spec" then
		PN (ParsingUtils.parse_file PnParser.program_top PnLexer.token src)

	else failwith (sprintf "I don't know how to handle file `%s'." src)

let print_to_file_re = Str.regexp "^print \\(.*\\)$"
let ptcb_re = Str.regexp "post-to-call-bfs \\([0-9]+\\) \\([0-9]+\\) \\(true\\|false\\)"
let ptcbb_re = Str.regexp "post-to-call-bounded-bag \\([0-9]+\\)"
let ptcbf_re = Str.regexp "post-to-call-bounded-fifo \\([0-9]+\\)"
let lr_re = Str.regexp "lal-reps \\([0-9]+\\)"

let pntobpl_re = Str.regexp "pn-to-bpl \\([0-9]+\\)"

		  
let _ =
	(* Options.read_options ();

	begin match Options.get_string "back-end" with
	| "boom" -> begin
		  Options.set_string "target-language" "bp";
		  Options.set_bool "reach-slic-error" true;
		  Options.set_bool "one-label-per-stmt" true;
		  Options.set_bool "one-target-per-goto" true;
		  Options.set_bool "no-dead-stmts" true;
		  Options.set_bool "no-constrain-clauses" true;
		  Options.set_bool "no-ignore-returns" true
	  end
	| "moped" -> begin
		  Options.set_string "target-language" "bp";
		  Options.set_bool "reach-slic-error" true;
		  Options.set_bool "one-label-per-stmt" true;
		  Options.set_bool "one-target-per-goto" true;
		  Options.set_bool "no-dead-stmts" true;
		  Options.set_bool "no-constrain-clauses" true
	  end
	| "boogie" -> begin
		  Options.set_string "target-language" "bpl"
	  end
	| _ -> ()
	end;

	if Options.get_string "target-language" = "bp" then
		Options.set_bool "only-boolean" true;

	let src = Options.get_string "src_file" in
	 *)
	
	if not (Str.string_match filename_re (Sys.argv.(1)) 0) then
		failwith "Must specify a source file.";
		
	let src = Sys.argv.(1)
	and stages = parse_options
	in
	
	List.fold_left 
		(fun pgm stage -> match pgm, stage with
			
			(* Language translations *)
			| CP p, "cp-to-bpl" -> BPL (CpToBpl.program p)
			| CP p, "cp-to-bp" -> BP (CpToBp.program p)

			| PN p, "pn-to-bpl" -> BPL (PnToBpl.program None p)
			| PN p, s when Str.string_match pntobpl_re s 0 ->
				BPL (PnToBpl.program (Some (int_of_string <| Str.matched_group 1 s)) p)

			(* Back-end necessitites *)
			| BP p, "prepare-for-back-end" -> 
				BP (BpUtils.prepare_for_back_end p)
			| BPL p, "prepare-for-back-end" -> 
				BPL (BplUtils.prepare_for_back_end p)
				
			
			| BPL p, "esc-async" -> BPL (BplEscAsync.async_to_seq p)	
			| BPL p, "post-to-call-dfs" -> BPL (BplAsyncToSeq.post_to_call_dfs p)
			
			(* Printing *)	
			| CP p, "print" -> print_to_file "-" (CpAst.Program.print p); CP p
			| CP p, s when Str.string_match print_to_file_re s 0 -> 
				print_to_file (Str.matched_group 1 s) (CpAst.Program.print p);
				CP p
				
			| BP p, "print" -> print_to_file "-" (BpAst.Program.print p); BP p
			| BP p, s when Str.string_match print_to_file_re s 0 -> 
				print_to_file (Str.matched_group 1 s) (BpAst.Program.print p);
				BP p
				
			| BPL p, "print" -> print_to_file "-" (BplAst.Program.print p); BPL p
			| BPL p, s when Str.string_match print_to_file_re s 0 -> 
				print_to_file (Str.matched_group 1 s) (BplAst.Program.print p);
				BPL p
				
			| PN p, "print" -> print_to_file "-" (PnAst.Program.print p); PN p
			| PN p, s when Str.string_match print_to_file_re s 0 -> 
				print_to_file (Str.matched_group 1 s) (PnAst.Program.print p);
				PN p
				
			| CP p, "order-decls" -> CP (CpUtils.Program.order_declarations p)
			
			(* Concurrent translations *)
			| CP p, s when Str.string_match ptcbb_re s 0 ->
				let k = int_of_string (Str.matched_group 1 s)
				in CP (CpAsyncToSeq.post_to_call_bounded_bag k p)
				
			| CP p, "post-to-call-dfs" -> CP (CpAsyncToSeq.post_to_call_dfs p)

			| CP p, s when Str.string_match ptcb_re s 0 ->
				let k = int_of_string (Str.matched_group 1 s)
				and d = int_of_string (Str.matched_group 2 s)
				and r = bool_of_string (Str.matched_group 3 s)
				in CP (CpAsyncToSeq.post_to_call_bfs k d r p)				
		
			| CP p, s when Str.string_match ptcbf_re s 0 ->
				let k = int_of_string (Str.matched_group 1 s)
				in CP (CpAsyncToSeq.post_to_call_bounded_fifo k p)
				
			| CP p, "yield-to-skip" ->
				CP (CpEliminateYield.ignore_yield p)
				
			| CP p, s when Str.string_match lr_re s 0 ->
				let k = int_of_string (Str.matched_group 1 s)
				in CP (CpEliminateYield.lal_reps_with_maps k p)
				
			| CP p, "multi-to-single" ->
				CP (CpMultiToSingle.many_to_one p)
				
			| CP p, "asserts-to-err-flag" ->
				CP (CpAsserts.assert_to_err_flag false p)
				
			| CP p, "ensure-well-formed" ->
				CP (CpUtils.Program.ensure_well_formed p)
				
			| _, "help" ->
				printf ( 
					"ToDo: better help interface\n" ^^
					"  Here are some options for now:\n" ^^
					"  -print\n" ^^
					"  -pn-to-bpl <k:int>\n" ^^
					"  -post-to-call-dfs\n" ^^
					"  -post-to-call-bfs <k:int>\n" ^^
					"  ... plus many more undocumented! ;-)\n" );
				pgm
				
			| _ -> 
				Printf.eprintf 
					"Translation `%s' undefined for %s programs.\n"
					stage
					( match pgm with 
						| CP _ -> "Concurrent" 
						| BP _ -> "Boolean"
						| BPL _ -> "Boogie" 
						| PN _ -> "Petri net");
				pgm
		 )
		(parse_program src)
		stages