(** The top-level entry point. *)

open Prelude
open Printf

module PP = PrettyPrinting
open PP

let print_to_file f d =
	match f with
	| "-" -> output_string stdout << render <| d;
	| _ -> begin
		  let oc = open_out f in
		  output_string oc << render <| d;
		  close_out oc
	  end
		  
let _ =
	Options.read_options ();

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

	if Filename.check_suffix src ".bp" then
		print_to_file "-"
		<< BpAst.Program.print
		<< BpUtils.prepare_for_back_end
		<| ParsingUtils.parse_file BpParser.program_top BpLexer.token src

	else if Filename.check_suffix src ".bpl" then
		print_to_file "-"
		<< BplAst.Program.print
		<< BplUtils.prepare_for_back_end
		<| ParsingUtils.parse_file BplParser.program_top BplLexer.token src

	else if Filename.check_suffix src ".cp" then
		let p = ParsingUtils.parse_file
			CpParser.program_top CpLexer.token src in

		if Option.is_none (CpAst.Program.find_proc p "main") then
			failwith "Missing procedure `main'.";
		
		print_to_file "-"

		<< ( if Options.get_string "target-language" = "bp" then
				 BpAst.Program.print
				 << BpUtils.prepare_for_back_end
				 << CpToBp.program

			 else if Options.get_string "target-language" = "bpl" then
				 BplAst.Program.print
				 << BplUtils.prepare_for_back_end
				 << CpToBpl.program

			 else CpAst.Program.print )
			
		<< CpUtils.Program.order_declarations

		<< ( match Options.get_string "task-scheduler" with
			 | "bounded-bag" ->
					let k = Options.get_int "fifo-size" in
					CpAsyncToSeq.post_to_call_bounded_bag k
			 | "dfs" -> CpAsyncToSeq.post_to_call_dfs
			 | "bfs" ->
				let k = Options.get_int "phase-bound" in
 				let d = Options.get_int "delay-bound" in
				let p = Options.get_bool "fewer-phases" in
				   CpAsyncToSeq.post_to_call_bfs k d p
			 | "bounded-fifo" -> 
				   let k = Options.get_int "fifo-size" in
				   CpAsyncToSeq.post_to_call_bounded_fifo k
			 | _ -> id )

		<< ( match Options.get_int "interleaving-bound",
				 Options.get_bool "only-boolean"
			 with
(* 			 | 0, _ -> CpEliminateYield.ignore_yield *)
(* 			 | k, true -> CpEliminateYield.lal_reps_bool k *)
			 | 0, _ -> CpEliminateYield.ignore_yield
			 | k, true -> failwith "fix me"
			 | k, false -> CpEliminateYield.lal_reps_with_maps k )

		<< ( if Options.get_bool "multi-to-single"
			 then CpMultiToSingle.many_to_one
			 else id )
			
		<< CpAsserts.assert_to_err_flag 
			(Options.get_bool "asserts-throw-exn")

		<< ( if Options.get_bool "ensure-well-formed" 
			 then CpUtils.Program.ensure_well_formed 	
			 else id )
			
		<| p

	else Printf.printf
		( "I don't know what to do with file `%s'.\n"
		  ^^ "Give me an XXX.bp or an XXX.cp file.\n" )
		src
