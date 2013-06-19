open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

let stage_id = "Seq"
let err_flag = sprintf "%s.flag" stage_id

let inline_procs = 
  Program.translate
		~replace_global_decls: 
			( fun d -> match d with 
        | D.Proc (ax,_,_) when A.has "leavealone" ax -> [d]
        | D.Proc (ax,n,p) -> [D.Proc (A.add (A.num "inline" 1) ax, n, p)] 
        | _ -> [d] )
      
let boogie_si_mode = true
        
let seq_framework pgm = 
  
  let entrypoints = 
    List.filter (D.has "entrypoint") <| Program.procs pgm
  in
  
  if entrypoints = [] then
    warn "No {:entrypoint} procedures found.";
  
  let is_init_axiom = 
    function D.Axiom (ax,_) when A.has "init" ax -> true | _ -> false 
  in

  let init_axioms = 
    List.flatten 
    << List.map (function D.Axiom (ax,e) when A.has "init" ax -> [e] | _ -> [])
    <| pgm 
  in

  (* Add an error flag. *)
  Program.translate
    ~new_global_decls:[D.Var ([A.unit "leavealone"],err_flag,T.Bool,None)]
      
  (* Add a wrapper around each entry point. *)
  << Program.translate
    ~new_global_decls:(List.map ( 
      function D.Proc (ax,n,(_,ps,rs,_,_)) ->
        let args = List.mapi (fun i (_,t) -> sprintf "p%i" i, t) ps
        and rets = List.mapi (fun i (_,t) -> sprintf "r%i" i, t) rs in

        D.proc (sprintf "%s.%s" stage_id n)
          ~attrs:[ A.unit "leavealone"; A.unit "entrypoint" ]
          ~decls:(List.map (uncurry D.var) (args @ rets))
          ~body:(
            (E.ident err_flag |:=| E.bool false)
            @ [ Ls.skip ~attrs:[A.unit "initial"] () ]
            @ [ Ls.call n
                ~attrs:[A.unit "formerentrypoint"]
                ~params:(List.map (E.ident << fst) args) 
                ~returns:(List.map fst rets) ]
            @ [ Ls.skip ~attrs:[A.unit "validity"] () ]
            @ [ if boogie_si_mode 
                (* Boogie's /stratifiedInline mode checks whether an entrypoint
                   procedure can return. *)
                then Ls.assume (E.ident err_flag) 
                else Ls.assert_ (!| (E.ident err_flag)) ]
            @ [ Ls.return () ]
          )
      | _ -> failwith "unexpected declaration." 
    ) entrypoints)
  
  (* Remove existing entrypoint annotations. *)
  << List.map (D.strip "entrypoint")
  
  (* Add those hacky "init" axioms to the entrypoint procedures. *)
  << Program.translate
    ~proc_body_prefix: (fun (ax,_,_) -> 
      if A.has "entrypoint" ax
      then List.map Ls.assume init_axioms
      else []
    )
      
  (* Replace asserts with error-flag assignments. *)
  << Program.translate
    ~per_stmt_map: (fun n s -> 
      match s with
      | ls, S.Assert ([],e) ->
        Ls.add_labels ls (E.ident err_flag |:=| (E.ident err_flag ||| (E.negate e)))
      | _ -> [s] 
    )

  (* Get rid of those hacky "init" axioms :-) *)
  << List.filter (not << is_init_axiom)
  <| pgm