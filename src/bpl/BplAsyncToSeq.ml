(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils
open Operators

module Tr = BplTranslateUtils

let stage_id = "A2S"

(** An encoding of the depth-first task-scheduling order, a restriction of 
 * the unordered (i.e., "bag") semantics.*)
let post_to_call_dfs p =

	let guess = fun x -> sprintf "%s__%s__guess" x stage_id
	and saved = fun x-> sprintf "%s__%s__saved" x stage_id
	and next = fun x-> sprintf "%s__%s___next" x stage_id
	and next_init = fun x-> sprintf "%s__%s__next_init" x stage_id
	in

	let gs_decls = 
		List.filter ((=) "var" << D.kind) (Program.decls p) in

	let extra_global_decls =
		List.map (D.rename next) gs_decls
		@ List.map (D.to_const << D.rename next_init) gs_decls

	and extra_proc_decls =
		List.map (D.rename saved) gs_decls
		@ List.map (D.rename guess) gs_decls
	in

	let gs = List.map D.name gs_decls in
	let saved_gs = List.map saved gs
	and next_gs = List.map next gs
	and init_next_gs = List.map next_init gs
	and guess_gs = List.map guess gs
	in

	let init_predicate = 
		E.conj << List.map (uncurry ($=$))
		<| List.combine init_next_gs next_gs

	and validity_predicate = 
		E.conj << List.map (uncurry ($=$))
		<| List.combine init_next_gs gs
	in

	let translate_post s =
		match s with
		
		| ls, S.Call (ax,n,ps,[]) when A.has "async" ax ->

			Ls.add_labels ls [
			  saved_gs $:=$ gs;
			  gs $:=$ next_gs;
			  guess_gs $:=?$ ();
			  next_gs $:=$ guess_gs;

			  Ls.stmt <| S.Call (A.strip "async" ax, n, ps, []);

			  Ls.stmt <| S.Assume
					  ( E.conj (List.map (fun g -> g $=$ guess g) gs) ) ;

			  gs $:=$ saved_gs;
			  (* Ls.stmt <| S.Dead (saved_gs @ guess_gs); *)
		  ]	
			
	    | _, S.Call (ax,_,_,_) when A.has "async" ax ->
			failwith "Found async call with assignments."
			
		| _ -> [s]
		
	and last_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> E.ident (next x)
		| E.Sel (E.Id x,es) when List.mem x gs -> E.Sel (E.ident (next x), es)
		| _ -> e
	
	in

	(if List.length gs = 0 then
		Program.translate
			~per_stmt_map: 
				(fun _ -> function
				   | ls, S.Call (ax,n,ps,[]) when A.has "async" ax ->
					 	(ls, S.Call (A.strip "async" ax,n,ps,[]))::[]
				   | _, S.Call (ax,_,_,_) when A.has "async" ax ->
						failwith "Found async call with assignments."
				   | s -> s :: [])
		<< id
	else
		Program.translate
			~new_global_decls: extra_global_decls
			~per_stmt_map: (const translate_post)
			~proc_body_prefix:
				(fun (n,_) -> 
					if n = Tr.init_proc then
						[ Ls.assume init_predicate ]
					else if n = Tr.validate_proc then
						[ Ls.assume validity_predicate ]
					else [] )
			~new_local_decls:
				( fun (_,p) -> 
					if Ls.contains_rec 
						(function (_, S.Call (ax,_,_,_)) 
						          when A.has "async" ax -> true 
						 | _ -> false) 
						(Procedure.stmts p)
					then extra_proc_decls
					else [] )
			~per_expr_map: 
				(fun p -> 
					if p = Tr.check_proc then last_gval gs
					else id)
		<< id 
	)
	<| p
	
