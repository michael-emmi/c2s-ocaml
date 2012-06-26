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
let post_to_call_dfs pgm =

	let guess = fun x -> sprintf "%s.%s.guess" x stage_id
	and save = fun x-> sprintf "%s.%s.save" x stage_id
	and next = fun x-> sprintf "%s.%s.next" x stage_id
	and init = fun x-> sprintf "%s.%s.0" x stage_id
	and err_flag = sprintf "%s.err" stage_id
	and main_proc = sprintf "Main.%s" stage_id
	in

	let gs_decls = 
		List.filter ((=) "var" << D.kind) (Program.decls pgm) in
	
	let next_decls = List.map (D.rename next) gs_decls
	and init_decls = List.map (D.to_const << D.rename init) gs_decls
	and guess_decls = List.map (D.rename guess) gs_decls
	and save_decls = List.map (D.rename save) gs_decls
	in

	let gs = List.map D.name gs_decls in
	let save_gs = List.map save gs
	and next_gs = List.map next gs
	and init_gs = List.map init gs
	and guess_gs = List.map guess gs
	in

	let init_predicate = 
		E.conj << List.map (uncurry ($=$))
		<| List.combine gs init_gs
		
	and init_assigns =
		next_gs $:=$ guess_gs

	and validity_predicate = 
		E.conj << List.map (uncurry ($=$))
		<| List.combine gs guess_gs
	in
	

	let new_decls = 
		
		Declaration.parse <| sprintf
			"var %s: bool;
			 procedure %s () 
			 {
			    %s := false;
				assume %s;
				%s 
				call Main();
				assume %s;
				assert !%s;
				return;
			 }"
			err_flag
			main_proc
			err_flag
			(E.to_string init_predicate)
			(Ls.to_string init_assigns)
			(E.to_string validity_predicate)
			err_flag
			
	in
	
	
	let is_async_call = function
	| (_, S.Call (ax,_,_,_)) when A.has "async" ax -> true
	| _ -> false
	in
	

	let translate_post s =
		match s with
		
		| ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
			if rs != [] then
				warn <| 
				sprintf "Found async call (to procedure `%s') with assignments." n;
			
			Ls.add_labels ls [
			  save_gs $:=$ gs;
			  gs $:=$ next_gs;
			  guess_gs $:=?$ ();
			  next_gs $:=$ guess_gs;

			  Ls.stmt <| S.Call (A.strip "async" ax, n, ps, rs);

			  Ls.stmt <| S.Assume
					  ( E.conj (List.map (fun g -> g $=$ guess g) gs) ) ;

			  gs $:=$ save_gs;
			  (* Ls.stmt <| S.Dead (saved_gs @ guess_gs); *)
		  ]	
			
			
		| _ -> [s]
		
		
	and translate_assert s =
		match s with
		| ls, S.Assert e -> 
			Ls.add_labels ls [ 
				[E.ident err_flag] 
				|:=| [ E.ident err_flag ||| !| e] ]
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
				   | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
						if rs != [] then
							warn <| 
							sprintf "Found async call (to procedure `%s') with assignments." n;
					 	(ls, S.Call (A.strip "async" ax,n,ps,rs))::[]
				   | s -> s :: [])
		<< id
	else
		Program.translate
			~new_global_decls: ( init_decls @ next_decls @ new_decls )
			~per_stmt_map: (const (translate_post <=< translate_assert))
								
			~new_local_decls: (fun (n,p) -> 
				if n = main_proc then guess_decls
				else match Program.find_proc pgm n with
				| Some p when Ls.contains_rec is_async_call (Procedure.stmts p) ->
					save_decls @ guess_decls
				| _ -> [] )
					
			~per_expr_map: 
				(fun p -> 
					if p = Tr.check_proc then last_gval gs
					else id)
			~replace_global_decls: 
				( function D.Proc (ax,n,p) -> 
					[ D.Proc (A.add (A.num "inline" 1) ax, n, p) ]
				  | d -> d :: [] )
		<< id 
	)
	<| pgm
	
