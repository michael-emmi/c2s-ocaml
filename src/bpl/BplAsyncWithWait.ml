(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

(** An encoding of the depth-first task-scheduling order, a restriction of 
 * the unordered (i.e., "bag") semantics.*)

let id_of_expr = function
| E.Id x -> x
| _ -> failwith "id_of_expr: unexpected expression."

let async_to_seq pgm =
  
  let add_debug_info = true in
  
  let print_val var =
    if add_debug_info then 
      Ls.call "boogie_si_record_int" ~attrs:[A.unit M.leavealone] ~params:[E.ident var]
    else Ls.skip ()
  in
  
  
	let guess = fun x -> sprintf "%s.guess" x
  and gsave = fun x -> sprintf "%s.guess.save" x
	and save = fun x -> sprintf "%s.save" x
	and next = fun x -> sprintf "%s.next" x 
  and seq_idx = sprintf "#s"
  and seq_idx_local = sprintf "#s.me"

  and round_var = "#k"
  
  and fst_of x = sprintf "%s.fst" x
  and snd_of x = sprintf "%s.snd" x
  in
	
	let gs_decls = 
    List.filter ((=) D.V << D.kind &&&& (not << D.has M.leavealone))
		<| Program.decls pgm in
	let gs = List.map D.name gs_decls in
	
	let next_decls = List.map (D.rename next) gs_decls
	and guess_decls = List.map (D.rename guess) gs_decls
	and save_decls = List.map (D.rename save) gs_decls
  and gsave_decls = List.map (D.rename gsave) gs_decls
  in

	let next_gs = List.map next gs
	and save_gs = List.map save gs
	and gsave_gs = List.map gsave gs
	and guess_gs = List.map guess gs
	in
        
  let pause_segment = (save_gs $::=$ gs) @ (gsave_gs $::=$ guess_gs)
  and resume_segment = (gs $::=$ save_gs) @ (guess_gs $::=$ gsave_gs)

  and begin_segment = 
    (gs $::=$ next_gs) @ [Ls.havoc guess_gs] @ (next_gs $::=$ guess_gs)
    @ [Ls.incr (E.ident seq_idx) 1]
  and end_segment = List.map (fun g -> Ls.assume (g $=$ guess g)) gs
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
      ~ignore_attrs: [M.leavealone]
      
      ~prepend_global_decls: 
        ([D.var seq_idx T.Int] @ next_decls @ guess_decls)
        
      ~append_global_decls: []

			~proc_body_prefix: 
        (fun (ax,_,_) -> 
          if A.has M.entrypoint ax then [] 
          else [
            (seq_idx $:=$ seq_idx_local) ;
            print_val seq_idx_local 
          ])
          
			~new_proc_params: 
        (fun (ax,_,_) -> 
          if A.has M.leavealone ax || A.has M.entrypoint ax then [] 
          else [seq_idx_local, T.Int])
          
			~new_local_decls:
        (fun (ax,_,p) ->
          let task_vars = 
            Option.cat
            << List.map (function D.Var (_,x, T.T ("task",[t]),_) -> Some (x,t) | _ -> None) 
            <| Procedure.decls p 
          in
          (if Procedure.exists_expr 
            (function Expression.Id x -> List.mem x save_gs | _ -> false) p
          then (save_decls @ gsave_decls)
          (* else if A.has M.entrypoint ax then guess_decls *)
          else [])
          @ List.flatten (List.map (fun (x,ty) -> 
            [D.var (fst_of x) ty; D.var (snd_of x) T.Int]) task_vars))

      ~per_stmt_map: (const <| function
        | s when Ls.has_attr M.begin_seq s -> begin_seq_code @ [s]
        | s when Ls.has_attr M.end_seq s -> s :: end_seq_code

    		| ls, S.Call (ax,n,ps,rs) when A.has M.async ax -> begin
          (* if rs <> [] then
            warn "Found async call (to procedure `%s') with assignments." n; *)
            
          match A.get M.async ax with
          | Left t :: [] ->

      			Ls.add_labels ls (
              (* Map global variables in argument expressions 
              * to their "saved" values *)
              let ps = List.map (E.map (fun e -> 
                match e with
                | E.Id x when List.mem x gs -> E.Id (save x)
                | _ -> e
              )) ps 
              in 
              
              let rs = [ fst_of (id_of_expr t) ; snd_of (id_of_expr t) ] in
        
              pause_segment
              @ begin_segment
      	      @ [Ls.call ~attrs:[] n ~params:(ps@[E.ident seq_idx]) ~returns:rs]
              @ end_segment
              @ resume_segment )
              
          | [] -> 

      			Ls.add_labels ls (
              (* Map global variables in argument expressions 
              * to their "saved" values *)
              let ps = List.map (E.map (fun e -> 
                match e with
                | E.Id x when List.mem x gs -> E.Id (save x)
                | _ -> e
              )) ps 
              in 
        
              pause_segment
              @ begin_segment
      	      @ [Ls.call ~attrs:[] n ~params:(ps@[E.ident seq_idx]) ~returns:[]]
              @ end_segment
              @ resume_segment )
              
          | _ -> failwith "Wait-Elimination expects attribute with zero/one expression."
        end

        (* Pass along the sequentialization index. *)
    		| ls, S.Call (ax,n,ps,rs) when not (A.has M.leavealone ax) ->      
    			[ ls, S.Call (ax, n, ps@[E.ident seq_idx], rs)]

        | (ls,s) when Ls.has_attr "wait" (ls,s) -> begin
      
          match Ls.get_attr "wait" (ls,s) with
          | _, Left x :: Left t :: [] ->
            
            Ls.add_labels ls (
              end_segment
              @ begin_segment
              @ [x |:=| E.ident (fst_of (id_of_expr t))]
              @ [ Ls.ifthenelse 
                  ~expr:(E.ident round_var |<| E.ident (snd_of (id_of_expr t))) 
                  [E.ident round_var |:=| E.ident (snd_of (id_of_expr t))] ]
            )
        
          | _, Left t :: [] ->
            
            Ls.add_labels ls (
              end_segment
              @ begin_segment
              @ [ Ls.ifthenelse 
                  ~expr:(E.ident round_var |<| E.ident (snd_of (id_of_expr t))) 
                  [E.ident round_var |:=| E.ident (snd_of (id_of_expr t))] ]
            )
      
          | _ -> failwith "Wait-Elimination expects wait attribute with one/two expressions."
        end
        
    		| s -> [s]
      )
      pgm
      

