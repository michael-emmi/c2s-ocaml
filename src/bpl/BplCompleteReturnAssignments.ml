open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

(* Declarations for variables introduced because of 
   return-assignment completion. *)
let add_return_assign_decls pgm (tx,ps,rs,sx,bd) =
  tx, ps, rs, sx, Option.map (fun (ds,ss) ->
    let ignore_var_name i =	sprintf "#ret_%n_%s" i << Type.stringify
    and max_rets = Program.fold_procs (flip <| fun (_,_,rs,_,_) -> max (List.length rs)) 0
    and incomplete = LabeledStatement.incomplete_calls pgm ss in
  	ds @ begin
      Option.cat
    	<< List.map
    		(fun (i,t) ->
    			 if List.exists
    				 (fun ts -> i < List.length ts && List.nth ts i = t)
    				 incomplete
    			 then Some (Declaration.var (ignore_var_name i t) t)
    			 else None)		
    	<| List.product 
        (List.range 0 (max_rets pgm))
        [ Type.Bool; Type.Int; Type.Map ([],[Type.Int],Type.Int) ]
    end, ss
  ) bd


let complete_returns pgm = 
  id
  << Program.map_stmts_ctx (const <| LabeledStatement.complete_returns pgm)
  << Program.map_procs (add_return_assign_decls pgm)
  <| pgm