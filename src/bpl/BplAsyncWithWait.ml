(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

let async_to_seq pgm =
  
	let guess = fun x -> sprintf "%s.guess" x
	and save = fun x -> sprintf "%s.save" x
	and next = fun x -> sprintf "%s.next" x 
  and seq_idx = sprintf "#s"
  and seq_idx_local = sprintf "#s.me"
  and is_sync = "#is_sync"
  and ready = "#ready"
  and value = "#value"
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
  
  let async_calls = 
    Pg.fold_stmts (fun cs s -> 
      match s with
      | _, S.Call (ax,n,_,_) when A.has M.async ax && not (List.mem_assoc n cs) -> 
        begin match Pg.find_proc pgm n with
        | [_,ps,rs,_,_] -> 
          (n,(ps,rs))::cs
        | _ -> 
  				warn "Cannot resolve call to procedure `%s'." n;
          cs
        end
      | _ -> cs) [] pgm
  in
  
  let is_async_proc p = List.exists ((=) p << fst) async_calls in
          
  let begin_seq_code =
    [ Ls.havoc guess_gs ]
    @ ( next_gs $::=$ guess_gs )
    @ (E.ident seq_idx |:=| E.num 0)

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
      ~prepend_global_decls: (
        D.var seq_idx T.Int 
        :: D.var ready (T.map [T.Int] T.Bool)
        :: D.var value (T.map [T.Int] T.Int)
        :: next_decls
      )
        
			~new_proc_params: 
        (fun (ax,_,_) -> 
          if A.has M.leavealone ax then [] 
          else if A.has "async" ax then [is_sync, T.Bool; seq_idx_local, T.Int]
          else [seq_idx_local, T.Int])
          
			~new_local_decls:
        (fun (ax,_,p) ->
          if A.has M.async ax then save_decls @ guess_decls
          else if A.has M.entrypoint ax then guess_decls
          else [])
          
      ~proc_body_prefix: (function 
        | (ax,_,p) when A.has M.async ax -> 
          Ls.ifthenelse 
            ~expr:(E.not_ << E.ident <| is_sync ) 
            (pause_segment @ begin_segment) :: []
        | _ -> [])
        
      ~proc_before_return: (function 
        | (ax,_,p) when A.has M.async ax -> 
          Ls.ifthenelse ~expr:(E.not_ << E.ident <| is_sync ) (
            end_segment 
            @ resume_segment
          ) :: []
        | _ -> [])

      ~per_stmt_map: (const <| function
        | s when Ls.has_attr M.begin_seq s -> begin_seq_code @ [s]
        | s when Ls.has_attr M.end_seq s -> s :: end_seq_code
        
        | s when Ls.has_attr "wait" s -> begin
          
          let t, x = match Ls.get_attr "wait" s with
            | _, Left e :: Left f :: [] -> e, Some f
            | _, Left e :: [] -> e, None
            | _ -> failwith "Invalid wait statement."
          in
          
          Ls.ifthenelse ~expr:(E.not_ <| E.sel (E.ident ready) [t]) (
            [Ls.ifthenelse ~expr:(E.ident is_sync) ~els:end_segment pause_segment]

            @ begin_segment

            (* TODO mark ALL tasks as ready *)
            @ (E.sel (E.ident ready) [t] |:=| E.bool true)

            (* TODO ensure we are in a subsequent round *)
            @ [] )

          (* assign to the result variable, if provided *)
          :: Option.reduce (fun x -> x |:=| E.sel (E.ident value) [t]) [] x
          
        end

    		| ls, S.Call (ax,n,ps,rs) when is_async_proc n ->
          
          (* Is this async procedure to be called immediately? *)
          let imm = 
            if A.has "async" ax then begin
              match List.assoc "async" ax with
              | Left e :: [] when e = E.bool true -> true
              | _ -> false
            end
            else false
          in

          Ls.call ~attrs:(A.strip M.async ax) n
            ~params:(ps@[E.bool imm; E.ident seq_idx]) ~returns:rs :: []

    		| ls, S.Call (ax,n,ps,rs) when not (A.has M.leavealone ax) ->      

          (* Just pass along the sequentialization index. *)
    			[ ls, S.Call (ax, n, ps@[E.ident seq_idx], rs)]

    		| s -> [s]
      )
      pgm
      

