(** TACAS '12 FiFo sequentialization for Boogie programs. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

let stage_id = "FiFoSeq"
let use_simple_translation = true

let assumptions pgm =
  if not (Program.exists (fun d -> D.name d = "pid" && D.kind d = D.T) pgm)
    then failwith "FiFo sequentialization expects `pid' type declaration.";

  if not (Program.forall (
    function 
    | D.Proc (ax,n,(_,ps,_,_,Some(ds,_))) 
      when not (A.has M.entrypoint ax) 
      && not (A.has M.leavealone ax)
      && not (List.mem "self" (List.map fst ps @ List.map D.name ds)) ->
        warn "Procedure %s does not have `self' variable." n;
        false
    | _ -> true) pgm)
    then failwith
      ( "FiFo sequentialization expects each non-entrypoint procedure " ^
        "defines a `self' variable." ) ;
  
  if not (Program.forall (
    function
    | D.Proc (ax,n,p) ->
      List.for_all 
        ( fun x -> match Program.find pgm x with
          | (D.Var (_,_,T.Map(_,ts,_),_))::_ when List.mem (T.t "pid") ts -> true
          | (D.Var (ax,_,_,_))::_ when A.has M.leavealone ax -> true
          | (D.Var _ )::_ ->
            warn "Procedure `%s' modifies non-pid-partitioned state `%s'." n x;
            false
          | _ -> true )
        (Procedure.mods p)
    | _ -> true ) pgm)
    then failwith
      ( "FiFo sequentialization expects procedures only modify global variables " ^
        "partitioned by process identifiers (pid)." );
        
  (* ToDo: ensure processors do not access others' partition. *)  
  
  ()

(** An encoding of the breadth-first task-scheduling order, which preserves
 * the ordered FiFo task-buffer semantics, though only executes tasks up
 * a given level k of the task-creation tree. *)
let phase_bounding_simple phase_bound delay_bound p =
  
  assumptions p;
  
  let delay_label_idx = ref 0 in
  
  let add_debug_info = true in

  let phase_var = sprintf "%s.phase" stage_id
  and phase_const = sprintf "%s.PHASE_BOUND" stage_id
  
  and delay_var = sprintf "%s.delays" stage_id
  and delay_const = sprintf "%s.DELAY_BOUND" stage_id
  and shift_var = sprintf "%s.shift" stage_id
  and init_shift_var = sprintf "%s.shift.0" stage_id
  and vphase_var = sprintf "%s.vphase" stage_id
  
  and repl_var = fun x-> sprintf "%s.%s.vec" x stage_id
  and init_var = fun x-> sprintf "%s.%s.vec.0" x stage_id in

	let repl g e = E.sel (E.ident <| repl_var g) [e] in
  let init g e = E.sel (E.ident <| init_var g) [e] in
  
  let shift_expr pid ph = E.sel (E.sel (E.ident shift_var) [pid]) [ph] in
  let init_shift_expr pid ph = E.sel (E.sel (E.ident init_shift_var) [pid]) [ph] in
    
  let gvar_decls = 
    List.filter (not << A.has M.leavealone << D.attrs) (Program.global_vars p)
  in  
  let gvars = List.map D.name gvar_decls in
	
  let rnd_vec_t t = Type.map [Type.Int] t in
  
  let proc_begin_stmts = 
    ( E.ident vphase_var 
      |:=| ( if delay_bound > 0 
             then E.ident phase_var |+| shift_expr (E.ident "self") (E.ident phase_var) 
             else E.ident phase_var ))
    @ ( if add_debug_info
        then [ Ls.call "boogie_si_record_int" 
          ~attrs:[A.unit M.leavealone]
          ~params:[E.ident vphase_var] ]
        else [] )
    @ ( List.flatten
        << List.map (fun g -> E.ident g |:=| repl g (E.ident vphase_var))
        <| gvars )
    
  and proc_end_stmts =
    List.flatten
    << List.map (fun g -> repl g (E.ident vphase_var) |:=| E.ident g)
    <| gvars
  in
    
  let init_predicate_stmts = 
    List.map (fun g -> Ls.assume (repl_var g $=$ init_var g)) gvars
    @ ( if add_debug_info then [ 
          Ls.call "boogie_si_record_int"
            ~attrs:[A.unit M.leavealone]
            ~params:[E.ident phase_const]
        ] else [] )
    
    (* Special case for delays. *)
    @ ( if delay_bound > 0 
        then begin [
          Ls.assume (shift_var $=$ init_shift_var) ;
          Ls.assume (E.forall ["p",T.t "pid"] 
            (shift_expr (E.ident "p") (E.num 0) |=| E.num 0)) 
        ] @ ( if add_debug_info then [ 
                Ls.call "boogie_si_record_int" 
                  ~attrs:[A.unit M.leavealone] 
                  ~params:[E.ident delay_const]
              ] else [] )
          @ ( E.ident delay_var |:=| E.num 0 ) 
        end
        else [] )
    
  and validity_predicate_stmts = 
    ( List.flatten
      << List.map (fun i -> 
        List.map Ls.assume
     		<< List.map (fun g -> repl g (E.num (i-1)) |=| init g (E.num i))
     		<| gvars )
      <| List.range 1 (phase_bound+delay_bound-1) )
      
    (* Special case for delays. *)
    @ ( if delay_bound > 0 
        then List.map (fun i -> 
          Ls.assume ( 
            E.forall ["p",T.t "pid"]
              ( shift_expr (E.ident "p") (E.num (i-1)) 
                |=| init_shift_expr (E.ident "p") (E.num i) ))) 
          (List.range 1 (phase_bound-1))
        else [] )
  in

  let async_to_sync_call s =
    match s with
    | ls, S.Call (ax,n,ps,rs) when A.has M.async ax ->
      if rs <> [] then warn "Found async call (to `%s') with assignments." n;
      Ls.call ~labels:ls ~attrs:ax n ~params:ps ~returns:rs :: []
      
    | ls, s -> (ls, s)::[]
    
  and post_to_skip s =
    match s with
    | ls, S.Call (ax,n,ps,rs) when A.has M.async ax ->
      Ls.skip ~labels:ls () :: []

    | _ -> [s]
    
  and calls s = 
		match s with
    | ls, S.Call (ax,n,ps,rs) when A.has M.async ax ->      

      (* Asynchronous calls. *)
      Ls.ifthenelse ~labels:ls
        ~expr:( (E.ident vphase_var |+| E.num 1) |<| E.ident phase_const)
        [ Ls.call ~attrs:(A.strip M.async ax) n 
          ~params:(ps@[E.ident vphase_var |+| E.num 1]) ]
      :: []
      
    | ls, S.Call (ax,n,ps,rs) ->
      
      (* Synchronous calls. *)
      Ls.add_labels ls proc_end_stmts
      @ [ Ls.call ~attrs:ax n ~params:(ps@[E.ident phase_var]) ~returns: rs ]
      @ proc_begin_stmts      
      
		| _ -> s :: []
    
  and delay s =
    match s with
    | ls, s when Ls.is_yield (ls,s) && delay_bound > 0 -> 
      let ss = 
        ( Ls.add_labels [sprintf "DELAY.%n" (incr delay_label_idx; !delay_label_idx)]
          proc_end_stmts )
        @ [ Ls.assume (E.ident delay_var |<| E.ident delay_const) ;
            Ls.incr (E.ident delay_var) 1;
            Ls.incr (shift_expr (E.ident "self") (E.ident phase_var)) 1 ]
        @ proc_begin_stmts in
      ( if Ls.is_short_yield (ls,s) 
        then Ls.ifthenelse ~labels:ls ss
        else Ls.whiledo ~labels:ls ss ) :: []

    | _ -> s :: []
    
  and call_main s =
    match s with
    | ls, S.Call (ax,n,ps,rs) when A.has M.formerentrypoint ax ->
      [ ls, S.Call (ax,n,ps@[E.num 0], rs) ]
    | _ -> s :: [] 
    
  and predicates s =
    if Ls.has_attr M.begin_seq s then s :: init_predicate_stmts
    else if Ls.has_attr M.end_seq s then s :: validity_predicate_stmts
    else s :: []	
	in

  ( if List.length gvars = 0 then
    
    (* When no variables, just do async calls synchronously. *)
    Program.translate
      ~per_stmt_map: (const async_to_sync_call)
    << id
    
  else if phase_bound = 0 then
    
    (* When no phases, just ignore async calls. *)
    Program.translate 
      ~per_stmt_map: (const post_to_skip)
    << id
    
  else
    
    (* Instrument the entrypoint procedure. *)
    Program.translate
      ~per_stmt_map: (const (predicates <=< call_main))
      
    (* Instrument the other procedures. *)
    << Program.translate
      ~ignore_attrs: [M.leavealone]
					
  		~replace_global_decls: 
  			( fun d -> match d with 
          | D.Var (_,_,_,Some _) ->
            failwith "ToDo: handle where clause in FiFoseq translation."
            
          | D.Var (ax,g,t,_) -> [
            D.var ~attrs:ax (repl_var g) (rnd_vec_t t) ;
            D.const ~attrs:ax (init_var g) (rnd_vec_t t)
          ]
                      
  			  | _ -> [d] )
    
      ~prepend_global_decls: (
        [ D.const phase_const Type.Int ;
          D.axiom ( E.ident phase_const |>=| E.num 0 ) ;
          D.axiom ( E.ident phase_const |<=| E.num phase_bound ) ;
        ] @ if delay_bound > 0 then [
          D.const delay_const Type.Int ;
          D.axiom ( E.ident delay_const |>=| E.num 0 ) ;
          D.axiom ( E.ident delay_const |<=| E.num delay_bound ) ;
          D.var delay_var Type.Int ;
          D.var shift_var (T.map [T.t "pid"] (T.map [T.Int] Type.Int)) ;
          D.var init_shift_var (T.map [T.t "pid"] (T.map [T.Int] Type.Int)) ;
        ] else [] )

      ~new_proc_mods: (fun (_,_,(_,_,_,_,body)) -> 
        match body with
        | Some _ -> [delay_var; shift_var; init_shift_var]
        | _ -> [])
			~new_proc_params: (const [phase_var, T.Int])
      ~new_local_decls: (const (gvar_decls @ [ D.var vphase_var Type.Int ])) 
			~proc_body_prefix: (const proc_begin_stmts)
  		~proc_before_return: (const proc_end_stmts)
      ~per_stmt_map: (const (calls <=< delay))
      ~per_expr_map: (const id)
          
		<< id )
	<| p


let phase_bounding_fewer_phases phase_bound delay_bound p = 
  failwith "ToDo: implement fewer-phases phase-bounding translation."

let phase_bounding phase_bound delay_bound p = 
  if use_simple_translation 
    then phase_bounding_simple phase_bound delay_bound p
  else phase_bounding_fewer_phases phase_bound delay_bound p
