(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

type instrument_style = AtCallsite | SeparateProc

(* Note: performance seems to be a few seconds better in Boogie-SI 
  with AtCallsite, yet very slightly better in Boogie-FI with SeparateProc. *)

let style = AtCallsite

(** An encoding of the depth-first task-scheduling order, a restriction of 
 * the unordered (i.e., "bag") semantics.*)
let async_to_seq pgm =
  
	let guess = fun x -> sprintf "%s.guess" x
	and save = fun x -> sprintf "%s.save" x
	and next = fun x -> sprintf "%s.next" x 
  and async = fun x -> sprintf "%s.async" x
  and seq_idx = sprintf "#s"
  and seq_idx_local = sprintf "#s.me"
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
    match style with
    | SeparateProc -> begin
      Pg.fold_stmts (fun cs s -> 
        match s with
        | _, S.Call (ax,n,_,_) when A.has M.async ax && not (List.mem_assoc n cs) ->
          begin
            try let (_,ps,rs,_,_) = Pg.find_proc n pgm in (n,(ps,rs))::cs
            with Not_found -> 
              warn "Cannot resolve call to procedure `%s'." n;
              cs
          end
        | _ -> cs) [] pgm
    end
    | AtCallsite -> []
  in
  
  let async_decls = List.map (fun (n,(ps,rs)) ->
    D.proc (async n)
      ~attrs:[]
      ~params:ps
      ~returns:rs
      ~decls:(save_decls@guess_decls)
      ~body:(        
        pause_segment
        @ begin_segment
	      @ [ Ls.call ~attrs:[] n 
            ~params:((List.map (E.ident << fst) ps)@[E.ident seq_idx]) 
            ~returns:(List.map fst rs) ]
        @ end_segment
        @ resume_segment
        @ [Ls.return ()]
    )) async_calls
  in
        
  let begin_seq_code =
    Ls.havoc guess_gs
    :: (E.ident seq_idx |:=| E.num 0)
    :: (next_gs $::=$ guess_gs)

  and end_seq_code =
    List.map (fun g -> Ls.assume (g $=$ guess g)) gs
    @ (gs $::=$ next_gs)
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
        (D.var seq_idx T.Int :: next_decls)
        
      ~append_global_decls: async_decls

			~new_proc_params: 
        (fun (ax,_,_) -> 
          if A.has M.leavealone ax then [] 
          else [seq_idx_local, T.Int])
          
			~new_local_decls:
        (fun (ax,_,p) ->
          if Procedure.exists_expr 
            (function Expression.Id x -> List.mem x save_gs | _ -> false) p
          then save_decls @ guess_decls
          else if A.has M.entrypoint ax then guess_decls
          else [])

      ~per_stmt_map: (const <| function
        | s when Ls.has_attr M.begin_seq s -> begin_seq_code @ [s]
        | s when Ls.has_attr M.end_seq s -> s :: end_seq_code

    		| ls, S.Call (ax,n,ps,rs) when A.has M.async ax ->
    			if rs <> [] then
    				warn "Found async call (to procedure `%s') with assignments." n;

    			Ls.add_labels ls (
            (* Map global variables in argument expressions 
            * to their "saved" values *)
            let ps = List.map (E.map (fun e -> 
              match e with
              | E.Id x when List.mem x gs -> E.Id (save x)
              | _ -> e
            )) ps 
            in 
        
            match style with
            | SeparateProc ->
              [Ls.call ~attrs:(A.strip M.async ax) (async n) ~params:ps ~returns:rs]
          
            | AtCallsite -> begin
              pause_segment
              @ begin_segment
      	      @ [Ls.call ~attrs:[] n ~params:(ps@[E.ident seq_idx]) ~returns:rs]
              @ end_segment
              @ resume_segment
            end)

        (* Pass along the sequentialization index. *)
    		| ls, S.Call (ax,n,ps,rs) when not (A.has M.leavealone ax) ->      
    			[ ls, S.Call (ax, n, ps@[E.ident seq_idx], rs)]

    		| s -> [s]
      )
      pgm
      

