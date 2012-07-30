(** The top-level entry point. *)

open Prelude
open Printf

module PP = PrettyPrinting
open PP

module Op = Options

type ast = 
	| CP of CpAst.Program.t
	| BP of BpAst.Program.t
	| BPL of BplAst.Program.t
	| PN of PnAst.Program.t
	| CFG of Cfg.Grammar.t

let print_to_file f d =
	match f with
	| "-" -> output_string stdout << render <| d;
	| _ -> begin
		  let oc = open_out f in
		  output_string oc << render <| d;
		  close_out oc
	  end	
	
let parse_program src = 
	
	if Filename.check_suffix src ".cp" then begin
		let p = ParsingUtils.parse_file CpParser.program_top CpLexer.token src in		
		if Option.is_none (CpAst.Program.find_proc p "main") then
			failwith "Missing procedure `main'.";			
		CP p		
	end

	else if Filename.check_suffix src ".bp" then
		BP (ParsingUtils.parse_file BpParser.program_top BpLexer.token src)

	else if Filename.check_suffix src ".bpl" then
		BPL (ParsingUtils.parse_file BplParser.program_top BplLexer.token src)
		
	else if Filename.check_suffix src ".spec" then
		PN (ParsingUtils.parse_file PnParser.program_top PnLexer.token src)
		
	else if Filename.check_suffix src ".cfg" then
		CFG (ParsingUtils.parse_file CfgParser.grammar_top CfgLexer.token src)

	else failwith (sprintf "I don't know how to handle file `%s'." src)

let _ =
	
	let src, stages = Op.parse_cmdline () in
	
	List.fold_left 
		(fun pgm stage -> match pgm, stage with
			
			(* Language translations *)
			| CP p, ("cp-to-bpl",_) -> BPL (CpToBpl.program p)
			| CP p, ("cp-to-bp",_) -> BP (CpToBp.program p)

			| PN p, ("pn-to-bpl",[Op.Int k]) -> BPL (PnToBpl.program k p)
			
			| CFG p, ("cfg-to-presburger",_) -> BPL (Parikh.image_of_cfg p)
			| BPL p, ("violin-instrument", [Op.Int k]) -> BPL (BplViolin.instrument k p)

			(* Back-end necessitites *)
			| BP p, ("prepare-for-back-end",_) -> BP (BpUtils.prepare_for_back_end p)
			| BPL p, ("prepare-for-back-end",_) -> BPL (BplUtils.prepare_for_back_end p)
			
			| BPL p, ("esc-async",_) -> BPL (BplEscAsync.async_to_seq p)	
			| BPL p, ("delay-bounding",[Op.Int rounds ; Op.Int delays]) -> 
				BPL (BplAsyncToSeq.delay_bounding rounds delays p)
      | BPL p, ("phase-bounding",[Op.Int phases ; Op.Int delays]) -> 
        BPL (BplFifoSeq.phase_bounding phases delays p)
			
			(* Printing *)	
			| CP p, ("print",[Op.File f]) -> print_to_file f (CpAst.Program.print p); CP p
			| BP p, ("print",[Op.File f]) -> print_to_file f (BpAst.Program.print p); BP p
			| BPL p, ("print",[Op.File f]) -> print_to_file f (BplAst.Program.print p); BPL p
			| PN p, ("print",[Op.File f]) -> print_to_file f (PnAst.Program.print p); PN p
			| CFG p, ("print",[Op.File f]) -> print_to_file f (Cfg.Grammar.print p); CFG p
				
			| CP p, ("order-decls",_) -> CP (CpUtils.Program.order_declarations p)
			
			(* Concurrent translations *)
			| CP p, ("post-to-call-bounded-bag", [Op.Int k]) ->
				CP (CpAsyncToSeq.post_to_call_bounded_bag k p)
				
			| CP p, ("post-to-call-dfs",_) -> 
				CP (CpAsyncToSeq.post_to_call_dfs p)

			| CP p, ("post-to-call-bfs", [Op.Int k; Op.Int d; Op.Bool r]) ->
				CP (CpAsyncToSeq.post_to_call_bfs k d r p)	
				
			| CP p, ("post-to-call-bounded-fifo", [Op.Int k]) ->
				CP (CpAsyncToSeq.post_to_call_bounded_fifo k p)
				
			| CP p, ("yield-to-skip",_) ->
				CP (CpEliminateYield.ignore_yield p)
				
			| CP p, ("lal-reps", [Op.Int k]) ->
				CP (CpEliminateYield.lal_reps_with_maps k p)
				
			| CP p, ("multi-to-single",_) ->
				CP (CpMultiToSingle.many_to_one p)
				
			| CP p, ("asserts-to-err-flag",_) ->
				CP (CpAsserts.assert_to_err_flag false p)
				
			| CP p, ("ensure-well-formed",_) ->
				CP (CpUtils.Program.ensure_well_formed p)
				
			| _ -> 
				Printf.eprintf 
					"Warning: Flag `%s' undefined for %s programs.\n"
					(fst stage)
					( match pgm with 
						| CP _ -> "Concurrent" 
						| BP _ -> "Boolean"
						| BPL _ -> "Boogie" 
						| PN _ -> "Petri net"
						| CFG _ -> "Context-free grammar");
				pgm
		 )
		(parse_program src)
		stages