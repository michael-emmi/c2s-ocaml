open Prelude
open Printf
open BplAst
open BplUtils
open BplUtils.Extensions
open BplUtils.Abbreviations

let check_for_assertions p =  
  if Program.exists_stmt (function (_, Statement.Assert _) -> true | _ -> false ) p 
  then warn "Boogie's SI-mode may not handle assertions correctly!";
  p
  
let ensure_si_procedures p =
  let boogie_si_regexp = Str.regexp "boogie_si_record_\\([A-Za-z]+\\)" in

  (flip List.append <| p)
  << List.map (fun n -> 
    ignore <| Str.string_match boogie_si_regexp n 0;
    let t = Type.t (Str.matched_group 1 n) in
    info "Adding missing procedure declaration `%s'." n;
    D.Proc ([A.unit "leavealone"],n,([],["x",t],[],[],None)) )
  << List.filter (fun n -> Program.find_proc p n = [])
  << Program.fold_stmts (fun cs -> 
      function 
      | (_,Statement.Call (_,n,_,_)) when Str.string_match boogie_si_regexp n 0 ->
        List.add_uniq n cs
      | _ -> cs ) []
  <| p    
  
let for_boogie_si = 
  check_for_assertions
  << (fun p -> Program.map_procs (ProcedureExt.fix_modifies p) p)
  << BplAsserts.asserts_to_error_flag ~no_asserts:true
  << (fun p -> Program.map_procs (ProcedureExt.add_return_assign_decls p) p)
  << (fun p -> Program.map_stmts_ctx (const <| LabeledStatementExt.complete_returns p) p)
  << ensure_si_procedures
  
let for_boogie_fi =
  Program.add_inline_attribute ~ignore_attrs: ["entrypoint"; "leavealone"]
  << (fun p -> Program.map_procs (ProcedureExt.fix_modifies p) p)	
  << BplAsserts.asserts_to_error_flag ~no_asserts:false
  << ensure_si_procedures