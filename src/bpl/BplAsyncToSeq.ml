(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

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
    D.const rounds_const T.Int ;
    D.axiom (E.ident rounds_const |=| E.num rounds) ;
    D.const delays_const T.Int ;
    D.axiom (E.ident delays_const |=| E.num delays) ;
    D.var delays_var T.Int ;
    D.var err_flag T.Bool ;    
    D.proc
      ~attrs:[A.unit "entrypoint"]
      top_proc
      ~decls: guess_decls
      ~body: (
				(E.ident delays_var |:=| E.num 0)
				@ (E.ident err_flag |:=| E.bool false)
				@ [ Ls.assume (gs $==$ init_gs) ]
				@ (next_gs $::=$ guess_gs)
				@ [ Ls.call main_proc ~params:[E.num 0];
					Ls.assume (gs $==$ guess_gs) ]
					
				@ ( List.map (fun i -> 
						Ls.assume
						<< E.conj
						<< List.map (fun g -> 
							E.sel (E.ident <| next g) [E.num (i-1)] 
							|=| E.sel (E.ident <| init g) [E.num (i)] ) 
						<| gs )
				  	<| List.range 1 (rounds-1) )
					
				@ [ Ls.assume (E.ident err_flag) ;
					Ls.return () ]
			) 
  ] in
	
	let translate_yield s =
		if Ls.is_yield s then
      let ss = [
				Ls.assume ~labels:[delay_label ()] (E.ident round_idx |<| E.ident rounds_const) ;
				Ls.assume (E.ident delays_var |<| E.ident delays_const) ;
				Ls.incr (E.ident round_idx) ; 
				Ls.incr (E.ident delays_var) 
      ]
			in [ if Ls.is_short_yield s then Ls.ifthenelse ss else Ls.whiledo ss ]
		else [s]

	and translate_call s =
		match s with
		| ls, S.Call (ax,n,ps,rs) ->
			(* ToDo: return assignments for round_idx *)
			[ ls, S.Call (ax,n,ps@[E.ident round_idx],rs) ]
		| _ -> [s]

	and translate_post s =
		match s with
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
			<< List.unit
			<< Ls.ifthenelse ~expr:(E.ident round_idx |<| E.ident rounds_const)
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
    
    << Program.add_inline_attribute
      ~ignore_attrs: ["leavealone"; "entrypoint"]
      
    (* Sequentalization step: translate asynchronous calls to 
       synchronous calls. 
       ToDo: separate the Seq-framework part. *)
		<< Program.translate
      ~ignore_attrs: ["leavealone"]
			~new_global_decls: ( init_decls @ next_decls @ new_decls )		
			~new_proc_params: (const [init_round_idx, T.Int])								
			~per_stmt_map: (const (translate_post <=< translate_assert))	
      
    (* Add extra declarations for procedures which make async calls. *)
    << Program.translate
			~new_local_decls: 
        ( function
          | _, _, p when Ls.contains_rec Ls.is_async (Procedure.stmts p) ->
            save_decls @ guess_decls
          | _ -> [] )

    (* Yield elimination / vectorization step. *)
		<< Program.translate
      ~ignore_attrs: ["leavealone"]
			~replace_global_decls: (fun d -> vectorize_var_decl d :: [])
			~new_local_decls: (const [D.var round_idx T.Int])
			~proc_body_prefix: (const (round_idx $:=$ init_round_idx))
			~per_stmt_map: (const (translate_call <=< translate_yield))
			~per_expr_map: (const vectorize_expr)

	)
	<| pgm
	
