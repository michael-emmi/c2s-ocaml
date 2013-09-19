open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

let err_flag = "#e"

let asserts_to_error_flag ?(no_asserts = false) pgm = 
  
  let entrypoints = 
    List.filter (D.has "entrypoint") <| Program.procs pgm
  in
  
  if entrypoints = [] then
    warn "No {:entrypoint} procedures found.";
    
  let last_stmt =
    if no_asserts
    then []
    else [Ls.assert_(E.bool false)]
  in
    
  Program.translate

    (* Add an error flag. *)
    ~prepend_global_decls:[ D.var err_flag T.Bool ]
  
    (* Initialize the error flag to false. *)
    ~proc_body_prefix: (fun (ax,_,_) -> 
      if A.has "entrypoint" ax
      then [E.ident err_flag |:=| E.bool false]
      else []
    )

    (* Assume the error flag just before entry-point returns. *)
    ~proc_before_return: (fun (ax,_,_) -> 
      if A.has "entrypoint" ax
      then [Ls.assume (E.ident err_flag)] @ last_stmt
      else []
    ) 
        
    (* Replace asserts with error-flag assignments. *)
    ~per_stmt_map: (fun n s -> 
      match s with
      | ls, S.Assert ([],e) ->
        Ls.add_labels ls [E.ident err_flag |:=| (E.ident err_flag ||| (E.negate e))]
      | _ -> [s] 
    )

  <| pgm