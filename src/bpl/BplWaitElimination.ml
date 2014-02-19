(** Eliminate Wait 
 ** - translates `x := wait t` statements (written `assume {:wait x t} true` in
 **   Boogie) by introducing integer task identifiers, and two maps from IDs to
 **   `bool` -- to say whether the task has completed -- and `int` -- to store
 **   the result of a completed task.
 **)

open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

let eliminate_wait pgm =
  
  let result_var = "#result" in
  let done_var = "#done" in
  let unique_id_var = "#uid" in
  let self_var = "#myid" in
  
	Program.translate
    ~ignore_attrs: [M.leavealone]
    
    ~prepend_global_decls:
      ( D.var result_var (T.map [T.Int] T.Int)
        :: D.var done_var (T.map [T.Int] T.Bool)
        :: D.var unique_id_var T.Int :: [] )
        
    ~new_proc_mods: (fun (_,_,(_,_,_,_,body)) -> 
        match body with
        | Some _ -> [result_var; done_var; unique_id_var]
        | _ -> [])
        
		~new_proc_params: 
      (fun (ax,_,_) -> 
        if A.has M.leavealone ax then [] 
        else [self_var, T.Int])
        
    ~replace_local_decls:
      (fun (ax,_,_) d ->
        if A.has M.leavealone ax then []
        else begin 
          match d with
          | D.Var (_,x,T.T ("task",_),_) -> (D.var x T.Int) :: []
          | _ -> d :: [] 
        end)
                
    ~proc_before_return: (function 
      | (_,_,(_,_,rs,_,_)) -> 
        (E.sel (E.ident done_var) [E.ident self_var] |:=| E.bool true)
        :: (match rs with
            | [] -> []
            | [ret_var, _] -> 
              [E.sel (E.ident result_var) [E.ident self_var] |:=| E.ident ret_var]
            | _ ->
              warn "Wait-Elimination does not handle multiple return variables.";
              [] ))

    ~per_stmt_map: (fun (_,_,_) -> function
      | s when Ls.has_attr M.begin_seq s ->
        s
        :: (E.ident unique_id_var |:=| E.num 0)
        :: (Ls.assume (E.forall ["t",T.Int] (E.sel (E.ident done_var) [E.ident "t"] |=| E.bool false)))
        :: []
    
      | ls, S.Call (ax,n,ps,rs) when A.has "async" ax -> begin
        
        let aa = A.get "async" ax in
        let ax = A.unit "async" :: A.strip "async" ax in
          
        match aa with
        | [] -> 
          (ls, S.Call (ax,n,ps@[E.num 0],rs)) :: []

        | Left t :: [] ->

          (Ls.incr ~labels:ls (E.ident unique_id_var) 1)
          :: (t |:=| E.ident unique_id_var)
          :: ([], S.Call (ax,n,ps@[t],rs)) :: []
        
        | _ -> failwith "Wait-Elimination expects zero/one argument :async annotation."
      end
        
      | ls, S.Call (ax,n,ps,rs) ->
        (ls, S.Call (ax,n,ps@[E.num 0],rs)) :: []
      
      | (ls,s) when Ls.has_attr "wait" (ls,s) -> begin
        
        match Ls.get_attr "wait" (ls,s) with
        | _, Left x :: Left t :: [] ->
          Ls.assume ~labels:ls (E.sel (E.ident done_var) [t])
          :: (x |:=| E.sel (E.ident result_var) [t]) :: []
          
        | _, Left t :: [] ->
          Ls.assume ~labels:ls (E.sel (E.ident done_var) [t]) :: []
        
        | _ -> failwith "Wait-Elimination expects wait attribute with one/two expressions."
      end

      (* TODO translate the t.value *)
  		| s -> [s]
    )
    pgm
      

