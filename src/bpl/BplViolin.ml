open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

let instrument k p =
  let prefix = "Violin" in
  
  let clock_var = sprintf "%s.time" prefix
  and local_clock_var = "t0" 
  and barrier_var = sprintf "%s.ret" prefix
  and open_var = sprintf "%s.%s.open" prefix
  and done_var = sprintf "%s.%s.done" prefix
  and jump_label = sprintf "%s.%s.End" prefix
  and check_proc = sprintf "%s.CheckInvariant" prefix
  in
	
  (* Take the set of procedures annotated with {:method} as the methods. *)
	let methods = 
		Option.cat
		<| List.map (
			function D.Proc (ax,n,(_,ps,rs,_,_)) | D.Impl (ax,n,(_,ps,rs,_,_)) 
			when A.has "method" ax -> Some (n, List.length ps, List.length rs)
			| _ -> None				
		) p 
	in
  
  (* Take the set of constants of type "val" as the values. *)
  let vals =
    Option.cat
    << List.map (function 
      | D.Const (_,_,x,T.T (t,[]),_) when t = "val" -> Some x
      | _ -> None )
    <| Program.decls p
  in
	
	if List.length methods < 1 then
		failwith "This program has no methods! \
			Annotate some procedures with {:method}.";
	
	Program.translate

    (* Add [local_time] parameter to each procedure. *)
		~new_proc_params: 
      ( function 
        | (_,"Main",_) -> [] 
        | _ -> [local_clock_var, T.Int]
      )
    
    (* Make all returns jump to a unified exit point. *)
		~per_stmt_map: 
      ( fun (ax,n,_) -> begin
        function
        | ls, S.Return when A.has "method" ax ->
          Ls.goto [ jump_label n ] :: []          
        | ls -> ls :: []
        end )
		
    (* Begin each method with [ yield; time >= local_time ] *)
		~proc_body_prefix:
      ( function        
        | (ax,n,_) when A.has "method" ax ->
          [ Ls.yield () ; 
            Ls.assume (E.ident clock_var |>=| E.ident local_clock_var )
          ]
          
        | _ -> [] )
			
    (* Suffix each method with counter increments and decrements. *)
		~proc_body_suffix: 
      ( function 
        | (ax,n,(_,ps,rs,_,_)) when A.has "method" ax -> 
          
          let idxs = List.map fst ps @ List.map fst rs @ [local_clock_var]
          in
          [ Ls.assign ~labels:[jump_label n]
              [Lv.ident barrier_var]
              [E.bool true];
            Ls.decr (E.nested_sel (E.ident <| open_var n) (List.map E.ident idxs));
            Ls.incr (E.nested_sel (E.ident <| done_var n)
              (List.map E.ident idxs @ [ E.ident clock_var |+| E.num 1 ]));
            Ls.call check_proc ~params:(
              List.flatten << List.flatten 
              << List.map (fun (m,nargs,nrets) -> 
                (List.map (fun vs -> 
                  List.map (fun i -> 
                    E.nested_sel (E.ident <| open_var m) (vs@[E.num i])
                  ) <| List.range 0 k
                ) <| (List.words (nargs) <| List.map E.ident vals))

                @ (List.flatten << List.map (fun vs -> 
                  List.map (fun i -> 
                    List.map (fun j -> 
                      E.nested_sel (E.ident <| done_var m) (vs@[E.num i; E.num j])
                    ) <| List.range i k
                  ) <| List.range 0 k
                ) <| (List.words (nargs+nrets) <| List.map E.ident vals))
              ) <| methods
            );
            Ls.return ()
          ]
        
        | _ -> [] )        
    
	<| p