open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

let default_entry_points = ["main"; "Main"]

let identify_entry_points pgm = 
  
  match List.filter (D.has M.entrypoint) <| Program.procs pgm with
  | _::_ -> pgm
  
  | [] -> begin
    warn (sprintf "No {:%s} procedures found; looking for defaults.." 
      M.entrypoint);
    match 
      List.map (D.name) 
      << List.filter (fun d -> List.mem (D.name d) default_entry_points) 
      <| Program.procs pgm
    with
    | [] ->
      warn << sprintf "No default entry points found (among %s)."
        <| (String.concat ", " default_entry_points);
      pgm

    | eps -> 
      warn << sprintf "Using '%s' as default entry point(s)." <| (String.concat ", " eps);
      Program.translate
        ~replace_global_decls:(function
          | D.Proc (ax,name,p) when List.mem name eps ->
            D.Proc( A.unit M.entrypoint :: ax, name, p ) :: []
          | d -> d :: [] )
        pgm
  end