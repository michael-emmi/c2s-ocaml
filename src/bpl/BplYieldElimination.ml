open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

let delay_bounding rounds delays pgm =
  
  let add_debug_info = true in
  
  let print_val var =
    if add_debug_info then 
      Ls.call "boogie_si_record_int" ~attrs:[A.unit M.leavealone] ~params:[E.ident var]
    else Ls.skip ()
  in

  let is_boogie_ident s = Str.string_match (Str.regexp "boogie_si_record_.*") s 0 in

  let ignore_procs = 
    List.map D.name
    << Program.filter (fun p ->
      not (Program.is_defined (D.name p) pgm) 
      || D.has M.leavealone p 
      || D.has M.noyields p)
    <| Program.procs pgm
  in
  
	let delay_label_idx = ref 0 in
  
	let rounds_const = sprintf "#ROUNDS" 
	and delays_const = sprintf "#DELAYBOUND" 
	and delays_var = sprintf "#d"
  and jump_var = sprintf "#jump"
	and round_idx = sprintf "#k"
  and ignore_round_idx = sprintf "#k.ignore"
	and init_round_idx = sprintf "#k.0"
	and delay_label _ = 
		incr delay_label_idx;
		sprintf "~yield.%n" (!delay_label_idx)
	in

	let gs_decls = 
    List.filter ((=) D.V << D.kind &&&& (not << D.has M.leavealone))
		<| Program.decls pgm in
  let gs = List.map D.name gs_decls in

  let init = fun x-> sprintf "%s.0" x in      	

	let vectorize_var_decl = function
	| D.Var (ax,n,t,i) when List.mem n gs -> 
		D.Var (ax,n,T.Map ([],[T.Int],t),i)
	| d -> d
	
	and vectorize_expr = function
	| E.Id x when List.mem x gs -> E.sel (E.ident x) [E.ident round_idx]
	| e -> e
  
  and jump_fixed e i = 
    Ls.ifthenelse [
      Ls.assume ~labels:[delay_label ()] (Option.reduce id (E.bool true) e);
			Ls.assume (E.ident round_idx |+| E.num i |<| E.ident rounds_const);
			Ls.assume (E.ident delays_var |+| E.num i |<=| E.ident delays_const);
			Ls.incr (E.ident round_idx) i;
			Ls.incr (E.ident delays_var) i;
      print_val round_idx ]

  and jump_range e i k = 
    Ls.ifthenelse [
      Ls.assume ~labels:[delay_label ()] (Option.reduce id (E.bool true) e);
      Ls.havoc [jump_var];
      Ls.assume (E.ident jump_var |>=| E.num (Option.reduce id 1 i));
      Ls.assume (Option.reduce (fun k -> E.ident jump_var |<=| E.num k) 
          (E.bool true) k);
      Ls.assume (E.ident round_idx |+| E.ident jump_var |<| E.ident rounds_const);
      Ls.assume (E.ident delays_var |+| E.ident jump_var |<=| E.ident delays_const);

      Ls.assign [Lv.from_expr <| E.ident round_idx] [E.ident round_idx |+| E.ident jump_var];
      Ls.assign [Lv.from_expr <| E.ident delays_var] [E.ident delays_var |+| E.ident jump_var];
      print_val round_idx ]
  
  and begin_seq_code = 
    (E.ident delays_var |:=| E.num 0)
    :: (E.ident round_idx |:=| E.num 0)
    :: print_val (rounds_const)
    :: print_val (delays_const)
    :: (gs $::=$ List.map init gs)
    
  and end_seq_code = 
    List.flatten << List.map 
      (fun i -> 
        List.map 
          (fun g -> 
            Ls.assume (
              E.sel (E.ident g) [E.num (i-1)] 
              |=| E.sel (E.ident <| init g) [E.num (i)] ))
            gs )
      (* Note: technically this should go from 1 to (rounds-1).
         However, Boogie for some strange reason will not take our
         generated program when there these constraints are completely
         absent -- i.e. when rounds < 2. *)
      <| List.range 1 (rounds)
  in
  
	if List.length gs = 0 then pgm
	else begin
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
      (* ~ignore_attrs: [M.leavealone; M.entrypoint] *)

    << List.map (D.strip M.noyields)
      
    (* In case we have introduced vectorized expressions in requires clauses,
     * we must make them refer to the initial round-index variable, rather than
     * the returned round-index variable. *)
    << List.map (function 
      | D.Proc (ax,n,(tx,ps,rs,sx,bd)) when not (A.has M.noyields ax) -> 
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
    
    (* Yield elimination / vectorization step. *)

		<< Program.translate

      ~ignore_attrs: [M.leavealone]

      ~prepend_global_decls: (List.map (D.add (A.unit M.leavealone)) (
        D.const rounds_const T.Int
        :: D.axiom (E.ident rounds_const |=| E.num rounds)
        :: D.const delays_const T.Int
        :: D.axiom (E.ident delays_const |=| E.num delays)
        :: D.var delays_var T.Int
        :: D.var ignore_round_idx T.Int
        :: List.map (D.to_const << D.rename init << vectorize_var_decl) gs_decls))

			~replace_global_decls: (fun d -> vectorize_var_decl d :: [])

			~new_proc_params: 
        (fun (ax,n,_) -> 
          if A.has M.entrypoint ax then []
          else if A.has M.noyields ax then [round_idx, T.Int] 
          else [init_round_idx, T.Int])

      ~new_proc_rets:
        (fun (ax,n,_) -> 
          if A.has M.noyields ax || A.has M.entrypoint ax then [] 
          else [round_idx, T.Int])
          
      ~new_local_decls:
        (fun (ax,n,p) -> 
          (if A.has M.entrypoint ax then [ D.var round_idx T.Int ] else [])
          @ ( if Procedure.exists_expr ((=) (E.ident jump_var)) p
              then [D.var jump_var T.Int] 
              else [] ))

			~proc_body_prefix: 
        (fun (ax,_,_) -> 
          if A.has M.noyields ax || A.has M.entrypoint ax then [] 
          else begin
            (round_idx $:=$ init_round_idx)
            :: print_val init_round_idx
            :: []
          end)
          
      ~per_stmt_map: (const <| function
        | s when Ls.has_attr M.begin_seq s -> begin_seq_code @ [s]    
        | s when Ls.has_attr M.end_seq s -> s :: end_seq_code
        | s when Ls.is_yield s -> begin

          (* Conditionless yield *)
          match A.get "yield" (Ls.attrs s) with
          | Left (E.Lit (Literal.Num i)) :: [] -> jump_fixed None i :: []
          | Left (E.Lit (Literal.Num i)) :: Left (E.Lit (Literal.Num k)) :: [] -> 
            jump_range None (Some i) (Some k) :: []

          (* Conditional yield *)
          | Left e :: [] -> jump_range (Some e) None None :: []          
          | Left e :: Left (E.Lit (Literal.Num i)) :: [] -> jump_fixed (Some e) i :: []
          | Left e :: Left (E.Lit (Literal.Num i)) :: Left (E.Lit (Literal.Num k)) :: [] ->
            jump_range (Some e) (Some i) (Some k) :: []
        
          | _ -> jump_range None None None :: []
        end

    		| ls, S.Call (ax,n,ps,rs) when not (A.has M.leavealone ax) -> begin

          (* Pass the round index through calls. *)
    			(ls, S.Call (ax, n, ps@[E.ident round_idx],
            if A.has M.noyields ax then rs 
            else if A.has M.async ax then rs@[ignore_round_idx]
            else rs@[round_idx])) :: []
        end
      
    		| s -> s :: []
      
      )
			~per_expr_map: (const vectorize_expr)
      
    (* Prevent bodiless procedures from possibly yielding. *)
    << Program.translate
      ~replace_global_decls:
        ( fun d -> match d with
          | D.Proc (ax,n,p) when is_boogie_ident n ->
            [ D.Proc (A.add (A.unit M.leavealone) ax,n,p) ]
          | D.Proc (ax,n,p) when List.mem n ignore_procs ->
            [ D.Proc (A.add (A.unit M.noyields) ax,n,p) ]
          | _ -> [d]
        )        
      ~per_stmt_map:
        ( fun _ s -> match s with
          | ls, S.Call (ax,n,ps,rs) when is_boogie_ident n ->
            [ ls, S.Call (A.add (A.unit M.leavealone) ax,n,ps,rs)]
          | ls, S.Call (ax,n,ps,rs) when List.mem n ignore_procs ->
            [ ls, S.Call (A.add (A.unit M.noyields) ax,n,ps,rs)]
          | _ -> [s]
        )
    <| pgm
  end
	
