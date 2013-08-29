open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations
        
(* Move declarations of the form "axiom {:init} E" to the beginning of each
 * {:entrypoint} procedure, as "assume E".  *)

let init_axioms_at_entry_points pgm = 
    
  if not (List.exists (D.has "entrypoint") <| Program.procs pgm) then
    warn "No {:entrypoint} procedures found.";
  
  let is_init_axiom = 
    function D.Axiom (ax,_) when A.has "init" ax -> true | _ -> false 
  in

  let init_axioms = 
    List.collect (function D.Axiom (ax,e) when A.has "init" ax -> Some e | _ -> None)
    <| pgm 
  in      
  
  Program.translate
    ~proc_body_prefix: (fun (ax,_,_) -> 
      if A.has "entrypoint" ax
      then List.map Ls.assume init_axioms
      else [] )

  << List.filter (not << is_init_axiom)
  <| pgm