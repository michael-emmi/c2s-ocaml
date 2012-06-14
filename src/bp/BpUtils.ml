(** Utilities for Booelan programs. *)

open Prelude
open Printf
open BpAst

module E = Expression
module S = Statement

(** Transform [assert e] into [if (!e) then SLIC_ERROR: skip; else skip; fi]. *)
let assert_to_slic_error =
	function
	| (ls, S.Assert e) ->
		  [ ls, S.Ite ( Option.map (fun e -> E.Not e) e,
					  [ ["SLIC_ERROR"], S.Skip ],
					  [ [], S.Skip ] ) ]
	| s -> [s]

(** Transform [L1: L2: .. Lk: stmt] into [L1: skip; L2: skip; ..; Lk: stmt]. *)
let one_label_per_stmt =
	function
	| ([],s) -> [([],s)]
	| (ls,s) ->
		  List.map (fun l -> ([l],S.Skip)) (List.init ls)
		  @ [([List.last ls],s)]

(** Transform [goto L1, L2, .., Lk] into [if * then goto L1; else if * ..]. *)
let rec one_target_per_goto s = match s with
	| (ls,S.Goto (l::ids)) when List.length ids > 0 ->
		  [(ls,S.Ite (
				None,
				[([],S.Goto [l])],
				one_target_per_goto ([],S.Goto ids) ))]
	| _ -> [s]

(** Transform [dead i1,..,in] into [skip]. *)
let no_dead_stmts =
	function
	| (ls,S.Dead _) -> [(ls,S.Skip)]
	| s -> [s]

(** Transform [n]-ary return parameter [call p(e1,..,ek)] into [call x1,..,xn
	:= p(e1,..,ek)]. *)
let no_ignore_returns prg =
	function
	| (ls,S.Call (p,es,xs)) ->
		  let _, rs = Program.type_of_proc prg p in
		  [ ls, S.Call (p,es,
						xs @ List.make (const <| "__junk")
							(rs - List.length xs)) ]
	| s -> [s]

(** Transform [x1,..,xn := e1,..,en constrain e] into [x1,..,xn := e1,..,en;
	assume e]. *)
let no_constrain_clauses =
	function
	| (ls,S.Assign (ids,es,Some e)) ->
		  [ (ls,S.Assign (ids,es,None)) ; ([],S.Assume e) ]
	| s -> [s]

(** Apply whatever transformations necessary for the back-end in use. *) 
let prepare_for_back_end p =
	id
	<< ( if Options.global_bool "reach-slic-error"
		 then Program.map_stmts assert_to_slic_error
		 else id )
	<< ( if Options.global_bool "one-label-per-stmt"
		 then Program.map_stmts one_label_per_stmt
		 else id )
	<< ( if Options.global_bool "one-target-per-goto"
		 then Program.map_stmts one_target_per_goto
		 else id )
	<< ( if Options.global_bool "no-ignore-returns"
		 then (fun p ->
				   Program.map_stmts (no_ignore_returns p)
				   <| Program.add_decls ["__junk"] p)
		 else id )
	<< ( if Options.global_bool "no-dead-stmts"
		 then Program.map_stmts no_dead_stmts
		 else id )
	<< ( if Options.global_bool "no-constrain-clauses"
		 then Program.map_stmts no_constrain_clauses
		 else id )
	<| p

