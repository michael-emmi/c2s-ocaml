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

let is_main (_,n,_) = n = "Main"

let loattr = "leavealone"
let nyattr = "noyields"

(** An encoding of the depth-first task-scheduling order, a restriction of 
 * the unordered (i.e., "bag") semantics.*)
let delay_bounding rounds delays pgm =
	
	let gs = List.map D.name 
    << List.filter (not << A.has "leavealone" << D.attrs)
		<< List.filter ((=) D.V << D.kind) 
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
  and jump_var = sprintf "%s.jump" stage_id
	and round_idx = sprintf "k"
  and ignore_round_idx = sprintf "k.ignore"
	and init_round_idx = sprintf "k.0"
	and delay_label _ = 
		incr delay_label_idx;
		sprintf "%s.DELAY.%n" stage_id (!delay_label_idx)
	in

	let gs_decls = 
		List.map vectorize_var_decl
    << List.filter ((=) D.V << D.kind &&&& (not << D.has loattr))
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

  let ignore_procs = 
    List.map D.name
    << List.filter (fun p -> not (Program.is_defined pgm (D.name p)) || D.has loattr p || D.has nyattr p)
    <| Program.procs pgm
  in

  
  let is_boogie_ident s = Str.string_match (Str.regexp "boogie_si_record_.*") s 0 in
    
  let init_predicate_stmts = 
    (E.ident delays_var |:=| E.num 0)
    @ ( if add_debug_info then [ 
          Ls.call "boogie_si_record_int"
            ~attrs:[A.unit loattr]
            ~params:[E.ident rounds_const] ;
          Ls.call "boogie_si_record_int"
            ~attrs:[A.unit loattr]
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
                
        (* Note: technically this should go from 1 to (rounds-1).
           However, Boogie for some strange reason will not take our
           generated program when there these constraints are completely
           absent -- i.e. when rounds < 2. *)
        <| List.range 1 (rounds) )
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
      let jump_fixed e i = 
        Ls.ifthenelse [
          Ls.assume ~labels:[delay_label ()] (Option.reduce id (E.bool true) e);
  				Ls.assume (E.ident round_idx |+| E.num i |<| E.ident rounds_const);
  				Ls.assume (E.ident delays_var |+| E.num i |<=| E.ident delays_const);
  				Ls.incr (E.ident round_idx) i;
  				Ls.incr (E.ident delays_var) i;
        ]

      and jump_range e i k = 
        Ls.ifthenelse [
          Ls.assume ~labels:[delay_label ()]
            (Option.reduce id (E.bool true) e);

          Ls.havoc [jump_var];
          Ls.assume (E.ident jump_var |>=| E.num (Option.reduce id 1 i));
          Ls.assume (Option.reduce (fun k -> E.ident jump_var |<=| E.num k) 
              (E.bool true) k);

          Ls.assume (E.ident round_idx |+| E.ident jump_var |<| E.ident rounds_const);
          Ls.assume (E.ident delays_var |+| E.ident jump_var |<=| E.ident delays_const);

          Ls.assign [Lv.from_expr <| E.ident round_idx] [E.ident round_idx |+| E.ident jump_var];
          Ls.assign [Lv.from_expr <| E.ident delays_var] [E.ident delays_var |+| E.ident jump_var];
        ]
      in
      match A.get "yield" (Ls.attrs s) with
      | [Left (E.Lit (Literal.Num i))] -> 
        [ jump_fixed None i ]

      | [Left (E.Lit (Literal.Num i)); Left (E.Lit (Literal.Num k))] -> 
        [ jump_range None (Some i) (Some k) ]

      | [Left e] ->
        [ jump_range (Some e) None None ]
          
      | [Left e; Left (E.Lit (Literal.Num i))] -> 
        [ jump_fixed (Some e) i ]

      | [Left e; Left (E.Lit (Literal.Num i)); Left (E.Lit (Literal.Num k))] ->
        [ jump_range (Some e) (Some i) (Some k) ]
        
      | _ -> [ jump_range None None None ]
          
    else [s]

 (*  THE OLD VERSION
     if Ls.is_yield s then
       let ss = [
         Ls.assume ~labels:[delay_label ()] 
           (E.ident round_idx |<| (E.ident rounds_const |-| E.num 1)) ;
         Ls.assume (E.ident delays_var |<| E.ident delays_const) ;
         Ls.incr (E.ident round_idx) ; 
         Ls.incr (E.ident delays_var) 
       ]
       in [ if Ls.is_short_yield s then Ls.ifthenelse ss else Ls.whiledo ss ]
     else [s] *)

	and translate_call s =
		match s with
		| ls, S.Call (ax,n,ps,rs) when n = "Main" ->
      [ ls, S.Call (ax,n,ps@[E.num 0],rs) ]

		| ls, S.Call (ax,n,ps,rs) when not (A.has "async" ax) ->      
			[ ls, S.Call (ax, n, 
        ps@[E.ident round_idx],
        if A.has nyattr ax then rs else rs@[round_idx]
      )]

		| _ -> [s]
    
	and translate_post s =
		match s with
		| ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
			if rs <> [] then
				warn "Found async call (to procedure `%s') with assignments." n;
			
			Ls.add_labels ls (
        
        (* Map global variables in argument expressions 
        * to their "saved" values *)
        let ps = List.map (E.map (fun e -> 
          match e with
          | E.Id x when List.mem x gs -> E.Id (save x)
          | _ -> e
        )) ps in
        
			  (save_gs $::=$ gs)
			  @ (gs $::=$ next_gs)
			  @ [Ls.havoc guess_gs]
			  @ (next_gs $::=$ guess_gs)
	      @ [ 
          Ls.call ~attrs:(A.strip "async" ax) n 
            ~params:(ps@[E.ident round_idx]) 
            ~returns:(rs@[ignore_round_idx]) ;
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
    
    (* In the case we have introduced vectorized expressions in axioms,
     * we must quantify the vector variable. 
     * NOTE: This should never actually be the case, since we should not be
     * allowed to mention global variables inside of axioms. *)
     (* << List.map (function
       | D.Axiom (ax,e) 
         when E.contains (function E.Id x when x = round_idx -> true | _ -> false) e ->
           D.Axiom (ax, E.forall [round_idx, T.Int] e)
       | d -> d
       ) *)
    
    (* << Program.add_inline_attribute *)
      (* ~ignore_attrs: ["leavealone"; "entrypoint"] *)
      
    (* In case we have introduced vectorized expressions in requires clauses,
     * we must make them refer to the initial round-index variable, rather than
     * the returned round-index variable. *)
    << List.map (function 
      | D.Proc (ax,n,(tx,ps,rs,sx,bd)) when not (A.has nyattr ax) -> 
        let sx' = List.map (function 
          | Sp.Requires (fr,ax,e) -> 
            Sp.Requires (fr, ax, begin
              E.map (function 
                | E.Id x when x = round_idx -> E.Id (init_round_idx)
                | e -> e ) e
            end)
          | s -> s ) sx in
        D.Proc (ax, n, (tx, ps, rs, sx', bd))
      | d -> d )
      
    (* For procedures without bodies, add an ensures clause which says the
     * value of the round-index variable is invariant. *)
    << List.map (function
      | D.Proc (ax,n,(tx,ps,rs,sx,None)) when List.mem (round_idx,T.Int) rs ->
        D.Proc (ax,n,(tx,ps,rs,sx@[Sp.ensures (round_idx $=$ init_round_idx)],None))
      | d -> d )
    
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
      ~ignore_attrs: [loattr]
			~new_global_decls: ( init_decls @ next_decls @ new_decls )		
			~new_proc_params: 
        (fun (ax,n,_) -> if A.has nyattr ax then [round_idx, T.Int] else [init_round_idx, T.Int])
			~per_stmt_map: (const translate_post)	
      
    (* Add extra declarations for procedures which make async calls. *)
    << Program.translate
			~new_local_decls: 
        ( fun (_,_,p) ->
            ( if Ls.contains_rec Ls.is_async (Procedure.stmts p) 
              then save_decls @ guess_decls
              else [] )
            
        )

    (* Yield elimination / vectorization step. *)
		<< Program.translate
      ~ignore_attrs: [loattr]
			~replace_global_decls: (fun d -> vectorize_var_decl d :: [])
      ~new_global_decls: ( [D.var ignore_round_idx T.Int] )
      ~new_local_decls: 
        (fun (ax,n,_) -> if n = "Main" then [D.var round_idx T.Int] else [])
			~new_proc_rets: 
        (fun (ax,n,_) -> if n = "Main" or A.has nyattr ax then [] else [round_idx, T.Int])
			~proc_body_prefix: 
        (fun (ax,_,_) -> if A.has nyattr ax then [] else 
          begin
            ( (round_idx $:=$ init_round_idx)
            @ ( if add_debug_info
                then [ Ls.call "boogie_si_record_int" 
                  ~attrs:[A.unit loattr]
                  ~params:[E.ident init_round_idx] ]
                else [] )) 
          end )
			~per_stmt_map: (const (translate_call <=< translate_yield))
			~per_expr_map: (const vectorize_expr)

    << Program.translate
      ~new_local_decls: 
        (fun (ax,n,p) -> 
          if Ls.contains_rec Ls.is_yield (Procedure.stmts p)
          then [D.var jump_var T.Int]
          else [] )
      
    (* Prevent bodiless procedures from possibly yielding. *)
    << Program.translate
      ~replace_global_decls:
        ( fun d -> match d with
          | D.Proc (ax,n,p) when is_boogie_ident n ->
            [ D.Proc (A.add (A.unit loattr) ax,n,p) ]
          | D.Proc (ax,n,p) when List.mem n ignore_procs ->
            [ D.Proc (A.add (A.unit nyattr) ax,n,p) ]
          | _ -> [d]
        )        
      ~per_stmt_map:
        ( fun _ s -> match s with
          | ls, S.Call (ax,n,ps,rs) when is_boogie_ident n ->
            [ ls, S.Call (A.add (A.unit loattr) ax,n,ps,rs)]
          | ls, S.Call (ax,n,ps,rs) when List.mem n ignore_procs ->
            [ ls, S.Call (A.add (A.unit nyattr) ax,n,ps,rs)]
          | _ -> [s]
        )
	)
	<| pgm
	
