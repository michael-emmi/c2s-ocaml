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
	
	let gs = List.map D.name 
    << List.filter (not << A.has "leavealone" << D.attrs)
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
  
  let add_debug_info = true in
  
	let guess = fun x -> sprintf "%s.%s.guess" x stage_id
	and save = fun x-> sprintf "%s.%s.save" x stage_id
	and next = fun x-> sprintf "%s.%s.next" x stage_id
	and init = fun x-> sprintf "%s.%s.0" x stage_id
	and rounds_const = sprintf "%s.ROUNDS" stage_id
	and delays_const = sprintf "%s.DELAYBOUND" stage_id
	and delays_var = sprintf "%s.delays" stage_id
	and round_idx = sprintf "k"
	and init_round_idx = sprintf "k.0"
	and delay_label _ = 
		incr delay_label_idx;
		sprintf "%s.DELAY.%n" stage_id (!delay_label_idx)
	in

	let gs_decls = 
		List.map vectorize_var_decl
    << List.filter ((=) "var" << D.kind &&&& (not << D.has "leavealone"))
		<| Program.decls pgm in
	
	let next_decls = List.map (D.rename next) gs_decls
	and init_decls = List.map (D.to_const << D.rename init) gs_decls
	and guess_decls = List.map (D.rename guess) gs_decls
	and save_decls = List.map (D.rename save) gs_decls
	in

	let save_gs = List.map save gs
	and next_gs = List.map next gs
	and guess_gs = List.map guess gs
	in
	
	(* let global_access e gs =  *)
		
    
  let init_predicate_stmts = 
    (E.ident delays_var |:=| E.num 0)
    @ ( if add_debug_info then [ 
          Ls.call "boogie_si_record_int"
            ~attrs:[A.unit "leavealone"]
            ~params:[E.ident rounds_const] ;
          Ls.call "boogie_si_record_int"
            ~attrs:[A.unit "leavealone"]
            ~params:[E.ident delays_const]
        ] else [] )    
    @ List.map (fun g -> Ls.assume (g $=$ init g)) gs
    @ ( next_gs $::=$ guess_gs )
  in
  
  let validity_predicate_stmts = 
    List.map (fun g -> Ls.assume (g $=$ guess g)) gs
    @ ( List.flatten
        << List.map 
          (fun i -> 
            List.map 
              (fun g -> 
                Ls.assume (
                  E.sel (E.ident <| next g) [E.num (i-1)] 
                  |=| E.sel (E.ident <| init g) [E.num (i)] ))
                gs ) 
        <| List.range 1 (rounds-1) )
  in

    
  let predicates s =
    if Ls.has_attr "initial" s then s :: init_predicate_stmts
    else if Ls.has_attr "validity" s then s :: validity_predicate_stmts
    else s :: []	
	in
    
	
	let new_decls = [
    D.const rounds_const T.Int ;
    D.axiom (E.ident rounds_const |=| E.num rounds) ;
    D.const delays_const T.Int ;
    D.axiom (E.ident delays_const |=| E.num delays) ;
    D.var delays_var T.Int ;
  ] in
	
	let translate_yield s =
		if Ls.is_yield s then
      let ss = [
				Ls.assume ~labels:[delay_label ()] 
          (E.ident round_idx |<| (E.ident rounds_const |-| E.num 1)) ;
				Ls.assume (E.ident delays_var |<| E.ident delays_const) ;
				Ls.incr (E.ident round_idx) ; 
				Ls.incr (E.ident delays_var) 
      ]
			in [ if Ls.is_short_yield s then Ls.ifthenelse ss else Ls.whiledo ss ]
		else [s]

	and translate_call s =
		match s with
		| ls, S.Call (ax,n,ps,rs) when n = "Main" ->
      [ ls, S.Call (ax,n,ps@[E.num 0],rs) ]

		| ls, S.Call (ax,n,ps,rs) ->      
			(* ToDo: return assignments for round_idx *)
			[ ls, S.Call (ax,n,ps@[E.ident round_idx],rs) ]
      
		| _ -> [s]

	and translate_post s =
		match s with
		| ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
			if rs <> [] then
				warn "Found async call (to procedure `%s') with assignments." n;
			
			(* let xs = List.filter (fun x -> global_access x gs) ps in *)
			
			Ls.add_labels ls (
				(* ToDo: evaluate arguments so globals can be saved. *)
			  (save_gs $::=$ gs)
			  @ (gs $::=$ next_gs)
			  @ [Ls.havoc guess_gs]
			  @ (next_gs $::=$ guess_gs)
	      @ [ 
          Ls.call ~attrs:(A.strip "async" ax) n ~params:ps ~returns:rs ;
          Ls.assume (E.conj (List.map (fun g -> g $=$ guess g) gs))
        ]
			  @ (gs $::=$ save_gs) 
			)			
			
		| _ -> [s]
  in
		
	(if List.length gs = 0 then
		Program.translate
			~per_stmt_map: 
				(fun _ -> function
				   | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
						if rs <> [] then
							warn "Found async call (to procedure `%s') with assignments." n;
					 	(ls, S.Call (A.strip "async" ax,n,ps,rs))::[]
				   | s -> s :: [])
		<< id
	else
		id
    
    (* << Program.add_inline_attribute *)
      (* ~ignore_attrs: ["leavealone"; "entrypoint"] *)
      
    
    << Program.translate
      ~new_local_decls:
        ( function 
          | ax, _, _ when A.has "entrypoint" ax -> guess_decls 
          | _ -> [] )
      ~per_stmt_map: 
        ( function
          | ax, _, _ when A.has "entrypoint" ax ->
             predicates <=< translate_call
           | _ -> List.unit )
      
    (* Sequentalization step: translate asynchronous calls to 
       synchronous calls. 
       ToDo: separate the Seq-framework part. *)
		<< Program.translate
      ~ignore_attrs: ["leavealone"]
			~new_global_decls: ( init_decls @ next_decls @ new_decls )		
			~new_proc_params: (const [init_round_idx, T.Int])								
			~per_stmt_map: (const translate_post)	
      
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
			~proc_body_prefix: 
        (const 
            ( (round_idx $:=$ init_round_idx)
            @ ( if add_debug_info
                then [ Ls.call "boogie_si_record_int" 
                  ~attrs:[A.unit "leavealone"]
                  ~params:[E.ident init_round_idx] ]
                else [] )))
			~per_stmt_map: (const (translate_call <=< translate_yield))
			~per_expr_map: (const vectorize_expr)
      
    << Program.translate

	)
	<| pgm
	
