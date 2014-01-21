(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

(* call {:async false} x := some_task() ~> call x := some_task() *)

let async_to_seq pgm =
  
  let has_rounds = Program.exists ((=) "#ROUNDS" << D.name) pgm in
  
	let guess = fun x -> sprintf "%s.guess" x
	and save = fun x -> sprintf "%s.save" x
	and next = fun x -> sprintf "%s.next" x 
  and seq_idx = sprintf "#s"
  and seq_idx_local = sprintf "#s.me"
  and is_sync = "#is_sync"
  and is_sync_init = "#is_sync.0"
  and value = "#value"
  and ready = "#ready"
  and round = "#round"
  and task_idx = "#t"
  and fresh_task_idx = "#task_idx"
  in
  
  let ready_expr t =
    if has_rounds then E.sel (E.sel (E.ident ready) [E.ident "#k"]) [t]
    else E.sel (E.ident ready) [t]
  in
	
	let gs_decls = 
    List.filter ((=) D.V << D.kind &&&& (not << D.has M.leavealone))
		<| Program.decls pgm in
	let gs = List.map D.name gs_decls in
	
	let next_decls = List.map (D.rename next) gs_decls
	and guess_decls = List.map (D.rename guess) gs_decls
	and save_decls = List.map (D.rename save) gs_decls
  in

	let next_gs = List.map next gs
	and save_gs = List.map save gs
	and guess_gs = List.map guess gs
	in
        
  let pause_segment = save_gs $::=$ gs
  and resume_segment = gs $::=$ save_gs

  and begin_segment = 
    (gs $::=$ next_gs) @ [Ls.havoc guess_gs] @ (next_gs $::=$ guess_gs)
    @ [Ls.incr (E.ident seq_idx) 1]
  and end_segment = List.map (fun g -> Ls.assume (g $=$ guess g)) gs
  in
    
  let is_async_proc p = 
    List.exists (D.has "async")
    <| Program.filter ((=) p << D.name) pgm
  in

  let begin_seq_code =
    (E.ident seq_idx |:=| E.num 0)
    :: Ls.havoc guess_gs
    :: (next_gs $::=$ guess_gs)


  and end_seq_code =
    List.map (fun g -> Ls.assume (g $=$ guess g)) gs
    @ ( gs $::=$ next_gs )
  in
		
	if List.length gs = 0 then
		Program.translate
			~per_stmt_map: 
				(fun _ -> function
				   | ls, S.Call (ax,n,ps,rs) when A.has M.async ax ->
						if rs <> [] then
							warn "Found async call (to procedure `%s') with assignments." n;
					 	(ls, S.Call (A.strip M.async ax,n,ps,rs))::[]
				   | s -> s :: [])
		pgm

	else
		Program.translate
      ~prepend_global_decls:
        ( D.var seq_idx T.Int 
          :: D.var fresh_task_idx T.Int
          :: D.var ready ((if has_rounds then T.map [T.Int] else id) << T.map [T.Int] <| T.Bool)
          :: D.var value (T.map [T.Int] T.Int)
          :: D.var round (T.map [T.Int] T.Int)
          :: next_decls )
        
			~new_proc_params: 
        (fun (ax,_,_) -> 
          if A.has M.leavealone ax then [] 
          else if A.has "async" ax then [task_idx, T.Int; is_sync_init, T.Bool; seq_idx_local, T.Int]
          else [seq_idx_local, T.Int])
          
			~new_local_decls:
        (fun (ax,_,p) ->
          if A.has M.async ax then [D.var is_sync T.Bool] @ save_decls @ guess_decls
          else if A.has M.entrypoint ax then guess_decls
          else [])
          
      ~proc_body_prefix: (function 
        | (ax,_,p) when A.has M.async ax -> 
          
          (* Pause the caller's segment if beginning an async segment. *)
          (E.ident is_sync |:=| E.ident is_sync_init)
          :: Ls.ifthenelse 
            ~expr:(E.not_ << E.ident <| is_sync ) 
            (pause_segment @ begin_segment) 
          :: []
            
        | _ -> [])
        
      ~proc_before_return: (function 
        | (ax,_,p) when A.has M.async ax -> 
          
          let ret_var = 
            match Procedure.returns p with 
            | (r,_) :: _ -> r 
            | _ -> failwith "invalid async procedure."
          in
          
          (* Resume the caller's segment if ending async segment. *)
          Ls.ifthenelse
            ~expr:(E.not_ << E.ident <| is_sync ) 
            (end_segment @ resume_segment)          
          :: Ls.assume (E.sel (E.ident value) [E.ident task_idx] |=| E.ident ret_var)
          :: Ls.assume (if has_rounds then E.sel (E.ident round) [E.ident task_idx] |=| E.ident "#k" else E.bool true)
          :: (ready_expr (E.ident task_idx) |:=| E.ident is_sync)
          :: []
          
        | _ -> [])

      ~per_stmt_map: (fun (ax,_,(_,ps,rs,_,bd)) -> function
        | s when Ls.has_attr M.begin_seq s -> begin_seq_code @ [s]
        | s when Ls.has_attr M.end_seq s -> s :: end_seq_code
        
        | s when Ls.has_attr "wait" s -> begin
          
          if not (A.has "async" ax) then
            warn "non-async procedure contains wait.";
          
          let t, x = match Ls.get_attr "wait" s with
            | _, Left e :: Left f :: [] -> e, Some f
            | _, Left e :: [] -> e, None
            | _ -> failwith "Invalid wait statement."
          in
                    
          (* Q: how to extract the task-type variables here? *)
          let _ = 
            List.filter
              (const true)
              (ps @ rs @ (List.map D.to_id_type << Option.reduce fst [] <| bd))
          in

          (* ensure we are in a subsequent round *)
          (if has_rounds then
            Ls.ifthenelse 
              ~expr:(E.ident "#k" |<| E.sel (E.ident round) [t]) 
              [E.ident "#k" |:=| E.sel (E.ident round) [t]]
            else Ls.assume (E.bool true))
            
          :: Ls.ifthenelse 
            ~expr:(E.not_ <| ready_expr t) (
              [Ls.ifthenelse 
                ~expr:(E.ident is_sync) 
                ~els:end_segment 
                (pause_segment @ [E.ident is_sync |:=| E.bool false])]

              @ begin_segment

              (* TODO mark ALL tasks as ready *)
              @ [ready_expr t |:=| E.bool true]
            )

          (* assign to the result variable, if provided *)
          :: Option.reduce (fun x -> [x |:=| E.sel (E.ident value) [t]]) [] x
          
        end

    		| ls, S.Call (ax,n,ps,rs) when is_async_proc n ->
          
          let t =
            match rs with 
            | r :: _ -> r 
            | _ -> failwith "Invalid async call." 
          in
          
          (* Is this async procedure to be called immediately? *)
          let imm = 
            if A.has "async" ax then begin
              match List.assoc "async" ax with
              | Left e :: [] when e = E.bool true -> true
              | _ -> false
            end
            else true
          in

          (* Emit a regular call... *)
          (E.ident t |:=| E.ident fresh_task_idx)
          :: Ls.incr (E.ident fresh_task_idx) 1
          :: Ls.call ~attrs:(A.strip M.async ax) n
            ~params:(ps@[E.ident t; E.bool imm; E.ident seq_idx]) ~returns:rs 
          :: []

    		| ls, S.Call (ax,n,ps,rs) when not (A.has M.leavealone ax) ->      

          (* Just pass along the sequentialization index. *)
    			(ls, S.Call (ax, n, ps@[E.ident seq_idx], rs)) :: []


        (* TODO translate the t.value *)
    		| s -> [s]
      )
      pgm
      

