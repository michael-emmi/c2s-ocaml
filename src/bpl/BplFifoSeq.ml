(** TACAS '12 FiFo sequentialization for Boogie programs. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils
open Operators

module Tr = BplSeqFramework

let stage_id = "FiFoSeq"

(** An encoding of the breadth-first task-scheduling order, which preserves
 * the ordered "FiFo" task-buffer semantics, though only executes tasks up
 * a given level [k] of the task-creation tree. *)
let phase_bounding_without_delays k p =
  let phases = k+1 in

  let phase_var = sprintf "%s.phase" stage_id
  and bound = sprintf "%s.PHASES" stage_id
  and repl_var = fun x-> sprintf "%s.%s.vec" x stage_id
  and init_var = fun x-> sprintf "%s.%s.vec.0" x stage_id in

	let repl g e = E.sel (E.ident <| repl_var g) [e] in
  let init g e = E.sel (E.ident <| init_var g) [e] in

  let gvars =
    List.map D.name
    << List.filter (function 
      | D.Var (ax,_,_,_) when not (A.has "leavealone" ax) -> true
      | _ -> false)
    <| Program.decls p
  in
	
  (* let pid_vec_t t = Type.Map ([], [Tr.pid_type], Type.Int) in *)
  let rnd_vec_t t = Type.Map ([], [Type.Int], t) in
    
  let init_predicate = 
    List.map (fun g -> 
      Ls.assume (repl_var g $=$ init_var g)) 
    <| gvars
  and validity_predicate = 
    List.flatten
    << List.map (fun i -> 
        List.map Ls.assume
     		<< List.map (fun g -> repl g (E.num (i-1)) |=| init g (E.num i))
     		<| gvars )
 		<| List.range 1 (phases-1)
  in

  let post_to_call_no_globals s =
    match s with
    | ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s]
    | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
      if rs <> [] then
        warn (sprintf "Found async call (to `%s') with assignments." n);
        
      (ls, S.Call (A.strip "async" ax, n, ps, rs)) :: []
      
      (* ToDo: how to encode inter-node posting? *)
    
    | ls, s -> (ls, s)::[]
    
  and post_to_skip s =
    match s with
    | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
      (ls, S.skip) :: []
    | _ -> [s]

	and post_to_call s =
		match s with
    | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
      
      (* ToDo: inter-node posting. *)
      (* let target_pid = E.ident "TARGET_PID" in *)
      
      Ls.add_labels ls [
        Ls.ifthen (E.ident phase_var |<| E.ident bound) [
          Ls.stmt (S.Call (A.strip "async" ax, n, 
            ps @ [E.ident phase_var |+| E.num 1],
            rs
          ))
        ]
      ]
		| _ -> [s]
		
	and call s = 
		match s with
    | ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s]
    | ls, S.Call (ax,n,ps,rs) when n = Tr.main_proc_name ->
      [ ls, S.Call (ax,n,ps@[E.num 0],rs) ]
    (* | ls, S.Call (ax,n,ps,rs) when not (List.mem n Tr.aux_procs) -> *)
      (* [ ls, S.Call (ax,n,ps@[E.ident phase_var],rs)] *)
		| _ -> [s]    
    
  and predicates s =
    if Ls.is_annot [A.unit "initial"] s then [s]@init_predicate
    else if Ls.is_annot [A.unit "validity"] s then [s]@validity_predicate
    else [s]
		
		(*   and first_gval gs e =
		    match e with
		    | E.Id x when List.mem x gs -> repl x (E.num 0)
		    | _ -> e
		  
		  and last_gval gs e =
		    match e with
		    | E.Id x when List.mem x gs -> repl x (E.num <| k)
		    | _ -> e
		   *)
	in

  ( if List.length gvars = 0 then
    Program.translate
      ~per_stmt_map: (const post_to_call_no_globals)
    << id
    
  else if k = 0 then
    Program.translate 
      ~per_stmt_map: (const post_to_skip)
    << id
  else
    Program.translate
					
  		~replace_global_decls: 
  			( fun d -> match d with 
          | D.Var (_,_,_,Some _) ->
            failwith "ToDo: handle where clause in FiFoseq translation."
            
          | D.Var (ax,g,t,wh) when not (A.has "leavealone" ax) -> [
            D.Var (ax, repl_var g, rnd_vec_t t, wh) ;
            D.Const (ax, false, init_var g, rnd_vec_t t, ())
          ]
                      
  			  | _ -> [d] )
    
      ~new_global_decls: [ 
        D.const bound Type.Int ;
        D.axiom ( E.ident bound |>=| E.num 0 ) ;
        D.axiom ( E.ident bound |<| E.num phases ) ;
      ]
			
			~new_proc_params: 
				(fun (ax,n,_) ->
					if A.has "leavealone" ax then []
          else if List.mem n [] then []
					else [phase_var, T.Int] )
      
      ~new_local_decls: 
        (fun (ax,n,_) ->
          if A.has "leavealone" ax then []
          else if List.mem n [] then []
          else List.filter (function
            | D.Var (ax,_,_,_) when not (A.has "leavealone" ax) -> true
            | _ -> false)
            (Program.decls p))

			~proc_body_prefix: 
				(fun (ax,n,_) ->
					if A.has "leavealone" ax then []
          else if List.mem n [] then []
					else List.flatten 
            << List.map (fun g -> E.ident g |:=| repl g (E.ident phase_var)) 
            <| gvars
        )

  		~proc_body_suffix: 
  			(fun (ax,n,_) ->
  				if A.has "leavealone" ax then []
          else if List.mem n [] then []
  				else List.flatten 
            << List.map (fun g -> repl g (E.ident phase_var) |:=| E.ident g) 
            <| gvars
        )
          
      ~per_stmt_map: 
        (fun n -> 
          if List.mem n Tr.aux_procs then List.unit
        else predicates <=< call <=< post_to_call)
          
      ~per_expr_map: (fun n -> id)
          
		<< id )
	<| p


(** An encoding of the breadth-first task-scheduling order, which preserves
 * the ordered "FiFo" task-buffer semantics, though only executes tasks up
 * a given level [k] of the task-creation tree. *)
let phase_bounding_with_delays k delay p =
  let fewer_phases = false in
  let phases = k+1 in

  let phase_var = sprintf "__%s__phase" stage_id
  and next_phase_var = sprintf "__%s__next_phase" stage_id
  and bound = sprintf "__%s__K" stage_id
  
  and shift_var = sprintf "__%s__shift" stage_id
  and init_shift_var = sprintf "__%s__shift_init" stage_id
  and delay_var = sprintf "__%s__delay" stage_id
  and ancestor_var = sprintf "__%s__ancestor" stage_id
  and anc_stack_var = sprintf "__%s__anc_saved" stage_id
  
  and next_shift_var = sprintf "__%s__next_shift" stage_id
  and next_delay_var = sprintf "__%s__next_delay" stage_id

  and repl_var = fun x-> sprintf "%s__%s__vec" x stage_id
  and init_var = fun x-> sprintf "%s__%s__vec_init" x stage_id
  in
	
  let shift_expr phase pid = 
    E.sel (E.sel (E.ident shift_var) [phase]) [pid] in
	
  let phase_expr = 
    if delay > 0 then
      E.ident phase_var 
      |+| shift_expr (E.ident phase_var) (E.ident Tr.self_var)
    else E.ident phase_var
  in
	
  (* let repl g e = E.sel (E.ident <| repl_var g) [e]
  and init g e = E.sel (E.ident <| init_var g) [e] in *)

(*   let gvars =
    List.map D.name
    << List.filter ((=) "var" << D.kind) <| Program.decls p
  in *)
	
  let pid_vec_t t = Type.Map ([], [Tr.pid_type], Type.Int)
  and rnd_vec_t t = Type.Map ([], [Type.Int], t) in

  let new_global_decls =
    [ D.const bound Type.Int ; D.axiom ( E.ident bound |=| E.num phases ) ]

    @ ( if delay > 0 then [
        D.var shift_var (rnd_vec_t << pid_vec_t <| Type.Int);
        D.var init_shift_var (rnd_vec_t << pid_vec_t <| Type.Int);
        D.var delay_var Type.Int
      ] else [] )

    @ ( if fewer_phases then [
        D.var ancestor_var (pid_vec_t Type.Int)
      ] else [] )

    @ List.flatten (List.map (
      function D.Var (_,_,_,Some _) ->
        failwith "ToDo: handle where clause in FiFoseq translation."
      | D.Var (ax,g,t,wh) -> [
        D.Var (ax, repl_var g, rnd_vec_t t, wh) ;
        D.Const (ax, false, init_var g, rnd_vec_t t, ()) ]
      | _ -> [] ) <| Program.decls p)
  in

  (* let init_predicate = 
    ( Expression.conj
    << ( if delay > 0 then (@) [
      E.forall ["ph", T.Int; "pi", Tr.pid_type] 
        (shift_expr (E.ident "ph") (E.ident "pi") 
        |=| E.num 0) 
      ] else id ) 
    << List.make
      (fun i ->
        Expression.conj
        ( List.map 
          (fun g -> repl g (E.num i) |=| init g (E.num i))
          gvars
        @ ( if delay > 0 then [
            E.sel (E.ident shift_var) [E.num i] 
              |=| E.sel (E.ident init_shift_var) [E.num i]
          ] else [] ) ) )
    <| phases+delay )

  and validity_predicate = 
    Expression.conj
    << List.make
      (fun i ->
        Expression.conj
        ( List.map
          (fun g -> repl g (E.num i) |=| init g (E.num <| i+1))
           gvars 
        @ ( if delay > 0 then [
            E.sel (E.ident shift_var) [E.num i]
            |=| E.sel (E.ident init_shift_var) [E.num <| i+1]
          ] else [] ) ) )
    <| phases+delay-1 
  in *)

  (* let post_to_call_no_globals s =
    match s with
    | ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s]
    | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
      if rs <> [] then
        warn (sprintf "Found async call (to `%s') with assignments." n);
        
      (ls, S.Call (A.strip "async" ax, n, ps, rs)) :: []
      
      (* ToDo: how to encode inter-node posting? *)
    
    | ls, s -> (ls, s)::[] *)
		
  (* and post_to_skip s =
    match s with
    | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
      (ls, S.skip) :: []
    | _ -> [s] *)

	let post_to_call s =
		match s with
    | ls, S.Call (ax,n,ps,rs) when A.has "async" ax ->
      
      (* ToDo: inter-node posting. *)
      let target_pid = E.ident "TARGET_PID" in
      
      Ls.add_labels ls ( 
        if delay > 0 then
          Ls.ifstar [
            Ls.havoc [next_delay_var];
            Ls.havoc [next_shift_var];
            Ls.assume (
              (E.ident next_delay_var |>=| E.num 0)
              |&| (E.ident next_delay_var |>=| E.ident delay_var) 
              |&| (E.ident next_delay_var |<| E.num delay)
              |&| (E.ident next_shift_var |>=| E.num 0)
              |&| (E.ident next_shift_var 
                |>=| shift_expr (E.ident phase_var) 
                  (E.ident Tr.self_var))
              |&| (E.ident next_shift_var |<| E.num delay)
              |&| (E.ident next_shift_var |<=| E.ident next_delay_var)          
            );
          ]
          :: (delay_var $:=$ next_delay_var)
          @ ( shift_expr (E.ident phase_var) (E.ident Tr.self_var) 
            |:=| E.ident next_shift_var )
        else [] )
      @ ( E.ident next_phase_var |:=| phase_expr )
      @ ( if fewer_phases then [
          Ls.ifthen
            (E.ident next_phase_var |<=| E.sel (E.ident ancestor_var) [target_pid])
            [ Ls.incr (E.ident next_phase_var) ]
          ] else [ Ls.incr (E.ident next_phase_var) ] )

      @ [ Ls.ifthen
          (E.ident next_phase_var |<| E.num phases)
          (  ( if fewer_phases then (
              (E.ident anc_stack_var |:=| E.sel (E.ident ancestor_var) [target_pid])
              @ (E.sel (E.ident ancestor_var) [target_pid] |:=| E.ident next_phase_var)
            ) else [] )
            @ [ Ls.stmt (S.Call (A.strip "async" ax, n, ps@[E.ident next_phase_var],rs)) ]
            @ ( if fewer_phases then 
                E.sel (E.ident ancestor_var) [target_pid] |:=| E.ident anc_stack_var
              else [] ) )
        ] 
		| _ -> [s]
		
	and call s = 
		match s with
    (* | ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s]
    | ls, S.Call (ax,n,ps,rs) when n = Tr.real_main_proc ->
      [ ls, S.Call (ax,n,ps@[E.num 0],rs) ]
    | ls, S.Call (ax,n,ps,rs) when not (List.mem n Tr.aux_procs) ->
      [ ls, S.Call (ax,n,ps@[E.ident phase_var],rs)] *)
		| _ -> [s]
		
  (* and return in_proc s = 
    match s with
    | ls, S.Return e
    when not (List.mem in_proc (Tr.main_proc :: Tr.aux_procs)) ->
      Ls.add_labels ls [
        List.map (flip repl <| phase_expr) gvars |:=| List.map E.ident gvars;
        Ls.stmt (S.Return e)
      ]
    | _ -> [s]
     *)
  (* and first_gval gs e =
    match e with
    | E.Id x when List.mem x gs -> repl x (E.num 0)
    | _ -> e
  
  and last_gval gs e =
    match e with
    | E.Id x when List.mem x gs -> repl x (E.num <| k + delay)
    | _ -> e
   *)
	in

  (
  (* ( if List.length gvars = 0 then
    Program.translate
      ~per_stmt_map: (const post_to_call_no_globals)
    << id
    
  else if k = 0 then
    Program.translate 
      ~per_stmt_map: (const post_to_skip)
    << id
  else *)
    Program.translate
      ~new_global_decls: new_global_decls
      (* ~rem_global_decls: gvars *)
      (* ~add_proc_params: 
        (fun (n,_) ->
          if List.mem n (Tr.main_proc :: Tr.aux_procs) then [] 
            else [D.Const ([],phase_var,T.Int,None)]) *)
      (* ~add_local_decls: 
        (fun (n,_) ->
          if n = Tr.main_proc then []
          else if List.mem n Tr.aux_procs then []
          else begin
            (if fewer_phases 
              then [ D.Var ([], anc_stack_var, T.Int) ] 
              else [])
            @ (if delay > 0 
              then [
                D.Var ([], next_delay_var, T.Int) ;
                D.Var ([], next_shift_var, T.Int)
              ] else [])
            @ [ D.Var ([], next_phase_var, T.Int) ]
            @ (List.filter ((=) "var" << D.kind) <| Program.decls p)
          end) *)
      (* ~proc_body_prefix: 
        (fun (n,_) -> 
          if n = Tr.main_proc then []
          else if n = Tr.init_proc then
            (Ls.assume init_predicate)
            :: (if delay > 0 then [
              [E.ident delay_var] |:=| [E.num 0] ;
              [shift_expr (E.num 0) (E.ident Tr.root_pid)]
                |:=| [E.num 0]
              ] else [])
            @ ( if fewer_phases 
              then [ [E.sel (E.ident ancestor_var) [E.ident Tr.root_pid]]
                |:=| [E.num 0] ] 
              else [] )
          else if n = Tr.validate_proc then [ Ls.assume validity_predicate ]
          else if n = Tr.check_proc then []
          else [ List.map E.ident gvars |:=| 
                List.map (flip repl <| phase_expr) gvars ])  *)

      ~per_stmt_map: 
        (fun n -> 
          if List.mem n Tr.aux_procs then List.unit
          else (* return n <=< *) call <=< post_to_call)
      (* ~per_expr_map:
        (fun n -> 
          if n = Tr.init_proc then first_gval gvars
          else if n = Tr.check_proc then last_gval gvars
          else id) *)
		<< id )
	<| p

let phase_bounding phases delay p =
  ( if delay > 0 
    then phase_bounding_with_delays (phases-1) delay 
    else phase_bounding_without_delays (phases-1)) p
