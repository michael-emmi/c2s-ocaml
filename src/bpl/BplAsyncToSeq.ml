(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils
open Operators

module Tr = BplSeqFramework

let stage_id = "A2S"

(** An encoding of the depth-first task-scheduling order, a restriction of 
 * the unordered (i.e., "bag") semantics.*)
let delay_bounding rounds delays pgm =
	
	let main_proc = "Main" in
	
	let gs = List.map D.name 
		<< List.filter ((=) "var" << D.kind) 
		<| Program.decls pgm in
	
	let vectorize_var_decl = function
	| D.Var (ax,n,t,i) when List.mem n gs -> 
		D.Var (ax,n,T.Map ([],[T.Int],t),i)
	| d -> d
	
	and vectorize_expr = function
	| E.Id x when List.mem x gs -> E.sel (E.ident x) [E.ident "k"]
	| e -> e
	in
	
	let delay_label_idx = ref 0 in

	let guess = fun x -> sprintf "%s.%s.guess" x stage_id
	and save = fun x-> sprintf "%s.%s.save" x stage_id
	and next = fun x-> sprintf "%s.%s.next" x stage_id
	and init = fun x-> sprintf "%s.%s.0" x stage_id
	and err_flag = sprintf "%s.err" stage_id
	and rounds_const = sprintf "%s.ROUNDS" stage_id
	and delays_const = sprintf "%s.DELAYBOUND" stage_id
	and delays_var = sprintf "%s.delays" stage_id
	and round_idx = sprintf "k"
	and init_round_idx = sprintf "k.0"
	and top_proc = sprintf "Main.%s" stage_id
	and delay_label _ = 
		incr delay_label_idx;
		sprintf "%s.DELAY.%n" stage_id (!delay_label_idx)
	in

	let gs_decls = 
		List.map vectorize_var_decl
		<< List.filter ((=) "var" << D.kind) 
		<| Program.decls pgm in
	
	let next_decls = List.map (D.rename next) gs_decls
	and init_decls = List.map (D.to_const << D.rename init) gs_decls
	and guess_decls = List.map (D.rename guess) gs_decls
	and save_decls = List.map (D.rename save) gs_decls
	in

	let save_gs = List.map save gs
	and next_gs = List.map next gs
	and init_gs = List.map init gs
	and guess_gs = List.map guess gs
	in
	
	(* let global_access e gs =  *)
		
	
	let new_decls = [
		D.Const ([], false, rounds_const, T.Int, ()) ;
		D.Axiom ([], E.ident rounds_const |=| E.num rounds) ;
		D.Const ([], false, delays_const, T.Int, ()) ;
		D.Axiom ([], E.ident delays_const |=| E.num delays) ;
		D.Var ([], delays_var, T.Int, None) ;
		D.Var ([], err_flag, T.Bool, None) ;
		D.Proc ([A.unit "entrypoint"], top_proc, (
			[],[],[],[],
			guess_decls,
			(
				(E.ident delays_var |:=| E.num 0)
				@ (E.ident err_flag |:=| E.bool false)
				@ [ Ls.assume (gs $==$ init_gs) ]
				@ (next_gs $::=$ guess_gs)
				@ [ Ls.call main_proc [E.num 0] [];
					Ls.assume (gs $==$ guess_gs) ]
					
				@ ( List.map (fun i -> 
						Ls.assume
						<< E.conj
						<< List.map (fun g -> 
							E.sel (E.ident <| next g) [E.num (i-1)] 
							|=| E.sel (E.ident <| init g) [E.num (i)] ) 
						<| gs )
				  	<| List.range 1 (rounds-1) )
					
				@ [ Ls.assert_ (!| (E.ident err_flag)) ;
					Ls.return ]
			) 
			))
		]
			
	in
	
	
	let is_async_call = function
	| (_, S.Call (ax,_,_,_)) when A.has "async" ax -> true
	| _ -> false
	in
	
	let translate_yield s =
		if Ls.is_yield s then [
			( if Ls.is_short_yield s then Ls.ifstar else Ls.whilestar ) (
				Ls.add_labels [delay_label ()] [
					Ls.assume (E.ident round_idx |<| E.ident rounds_const) ;
					Ls.assume (E.ident delays_var |<| E.ident delays_const) ;
					Ls.incr (E.ident round_idx) ; 
				    Ls.incr (E.ident delays_var) ]
			)
		] else [s]

	and translate_call s =
		match s with
		| ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s]
		| ls, S.Call (ax,n,ps,rs) ->
			(* ToDo: return assignments for round_idx *)
			[ ls, S.Call (ax,n,ps@[E.ident round_idx],rs) ]
		| _ -> [s]

	and translate_post s =
		match s with
		| ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s]
		| ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
			if rs <> [] then
				warn <| 
				sprintf "Found async call (to procedure `%s') with assignments." n;
			
			(* let xs = List.filter (fun x -> global_access x gs) ps in *)
			
			Ls.add_labels ls (
				(* ToDo: evaluate arguments so globals can be saved. *)
			  (save_gs $::=$ gs)
			  @ (gs $::=$ next_gs)
			  @ [Ls.havoc guess_gs]
			  @ (next_gs $::=$ guess_gs)
		      @ List.map Ls.stmt [ 
				S.Call (A.strip "async" ax, n, ps, rs) ;
			  	S.Assume ([],E.conj (List.map (fun g -> g $=$ guess g) gs)) ]
			  @ (gs $::=$ save_gs) 
			)			
			
		| _ -> [s]
		
		
	and translate_assert s =
		match s with
		| ls, S.Assert (_,e) -> 
			Ls.add_labels ls 
			<< List.singleton
			<< Ls.ifthen (E.ident round_idx |<| E.ident rounds_const)
			<| (E.ident err_flag |:=| ( E.ident err_flag ||| !| e ))
		| _ -> [s]
	
	in
	
	
	(if List.length gs = 0 then
		Program.translate
			~per_stmt_map: 
				(fun _ -> function
				   | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
						if rs <> [] then
							warn <| 
							sprintf "Found async call (to procedure `%s') with assignments." n;
					 	(ls, S.Call (A.strip "async" ax,n,ps,rs))::[]
				   | s -> s :: [])
		<< id
	else
		id
		
		<< Program.translate
			~new_global_decls: ( init_decls @ next_decls @ new_decls )
			~per_stmt_map: (const (translate_post <=< translate_assert))
			
			~new_proc_params: 
				(fun (ax,n,_) ->
					if A.has "leavealone" ax then []
					else [init_round_idx, T.Int] )
								
			~new_local_decls: 
				(fun (ax,n,_) -> 
					if A.has "leavealone" ax then []
					else match Program.find_proc pgm n with
					| Some p when Ls.contains_rec is_async_call (Procedure.stmts p) ->
						save_decls @ guess_decls
					| _ -> [] )
					
			~replace_global_decls: 
				( function D.Proc (ax,n,p) as d -> 
					if A.has "leavealone" ax then [d]
					else [ D.Proc (A.add (A.num "inline" 1) ax, n, p) ]
				  | d -> d :: [] )

		<< Program.translate
			~replace_global_decls: (fun d -> vectorize_var_decl d :: [])
			~new_local_decls: 
				(fun (ax,_,p) ->  
					if A.has "leavealone" ax then []
					else [D.Var ([], round_idx, T.Int, None)])
			~proc_body_prefix: 
				(fun (ax,_,_) ->
					if A.has "leavealone" ax then []
					else (round_idx $:=$ init_round_idx ))
			~per_stmt_map: (const (translate_call <=< translate_yield))
			~per_expr_map: (fun n -> if n == top_proc then id else vectorize_expr)			
		
	)
	<| pgm
	
