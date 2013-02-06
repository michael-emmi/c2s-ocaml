(** The top-level entry point. *)

open Prelude
open Printf

module Op = Options

type ast = 
	| BP of BpAst.Program.t
	| BPL of BplAst.Program.t
	| PN of PnAst.Program.t
	| CFG of Cfg.Grammar.t

let print_to_file f d =
	match f with
	| "-" -> output_string stdout << PrettyPrinting.render <| d;
	| _ -> begin
		  let oc = open_out f in
		  output_string oc << PrettyPrinting.render <| d;
		  close_out oc
	  end	
	
let parse_program src = 
  if Filename.check_suffix src ".bp" then
		BP (ParsingUtils.parse_file BpParser.program_top BpLexer.token src)

	else if Filename.check_suffix src ".bpl" then
		BPL ( BplUtils.Extensions.Program.post_parsing
          << ParsingUtils.parse_file BplParser.program_top BplLexer.token 
          <| src )
		
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
			| PN p, ("pn-to-bpl",[Op.Int k]) -> BPL (PnToBpl.program k p)
			
			| CFG p, ("cfg-to-presburger",_) -> BPL (Parikh.image_of_cfg p)
			| BPL p, ("violin-instrument", [Op.Int k]) -> BPL (BplViolin.instrument k p)

			(* Back-end necessitites *)
			| BP p, ("prepare-for-back-end",_) -> BP (BpUtils.prepare_for_back_end p)
			| BPL p, ("prepare-for-back-end",_) -> 
        BPL (BplUtils.Extensions.Program.pre_boogie p)
			
      | BPL p, ("seq-framework",_) -> BPL (BplSeqFramework.seq_framework p)
			| BPL p, ("esc-async",_) -> BPL (BplEscAsync.async_to_seq p)	
			| BPL p, ("delay-bounding",[Op.Int rounds ; Op.Int delays]) -> 
				BPL (BplAsyncToSeq.delay_bounding rounds delays p)
      | BPL p, ("phase-bounding",[Op.Int phases ; Op.Int delays]) -> 
        BPL (BplFifoSeq.phase_bounding phases delays p)
      | BPL p, ("multi-to-single",_) -> BPL (BplMultiToSingle.multi_to_single p)
			
			(* Printing *)	
			| BP p, ("print",[Op.File f]) -> print_to_file f (BpAst.Program.print p); BP p
			| BPL p, ("print",[Op.File f]) -> print_to_file f (BplAst.Program.print p); BPL p
			| PN p, ("print",[Op.File f]) -> print_to_file f (PnAst.Program.print p); PN p
			| CFG p, ("print",[Op.File f]) -> print_to_file f (Cfg.Grammar.print p); CFG p
				
			| _ -> 
				Printf.eprintf 
					"Warning: Flag `%s' undefined for %s programs.\n"
					(fst stage)
					( match pgm with 
						| BP _ -> "Boolean"
						| BPL _ -> "Boogie" 
						| PN _ -> "Petri net"
						| CFG _ -> "Context-free grammar");
				pgm
		 )
		(parse_program src)
		stages