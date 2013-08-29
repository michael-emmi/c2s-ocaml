open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers
    
(* Add a wrapper around each {:entrypoint} procedure P, of the form
 *
 * procedure {:entrypoint} #Top.P ( .. ) { 
 *    assume {:beginseq} true;
 *    call {:formerentrypoint} <rets> := P(<args>);
 *    assume {:endseq} true;
 *    return;
 * } 
 *
 * ... and strip the {:entrypoint} attribute from P. *)


let wrap_entrypoint_procedures pgm = 
  
  let entrypoints = 
    List.filter (D.has M.entrypoint) <| Program.procs pgm
  in
  
  if entrypoints = [] then
    warn "No {:%s} procedures found." M.entrypoint;
      
  Program.translate
    ~append_global_decls:(List.map ( 
      function D.Proc (ax,n,(_,ps,rs,_,_)) ->
        let args = List.mapi (fun i (_,t) -> sprintf "p%i" i, t) ps
        and rets = List.mapi (fun i (_,t) -> sprintf "r%i" i, t) rs in

        D.proc (sprintf "#Top.%s" n)
          ~attrs:[ A.unit M.entrypoint ]
          ~decls:(List.map (uncurry D.var) (args @ rets))
          ~body:(
            [ Ls.skip ~attrs:[A.unit M.begin_seq] () ]
            @ [ Ls.call n
                ~attrs:[A.unit M.formerentrypoint]
                ~params:(List.map (E.ident << fst) args) 
                ~returns:(List.map fst rets) ]
            @ [ Ls.skip ~attrs:[A.unit M.end_seq] () ]
            @ [ Ls.return () ]
          )
      | _ -> failwith "unexpected declaration." 
    ) entrypoints)
  
  (* Remove existing entrypoint annotations. *)
  << List.map (D.strip M.entrypoint)
  
  <| pgm