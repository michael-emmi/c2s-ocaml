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
			when A.has "method" ax -> Some (n, ps, rs)
			| _ -> None				
		) p 
	in
  
  (* Take the set of constants annotated with {:value} as the values. *)
  let vals =
    Option.cat
    << List.map (function 
      | D.Const (ax,_,x,t,_) when A.has "value" ax -> Some (x,t)
      | _ -> None )
    <| Program.decls p
  in
  
  let type_checks =
    List.for_all2 (fun (_,t) v -> 
      try List.assoc v vals = t
      with Not_found -> failwith "Problem with method arg type-checking!" )
  in
	
	if List.length methods < 1 then
		failwith "This program has no methods! \
			Annotate some procedures with {:method}.";
	
	Program.translate

    (* Add [local_time] parameter to each procedure. *)
		~new_proc_params: 
      ( function 
        | (ax,_,_) when A.has "method" ax -> [local_clock_var, T.Int]
        | _ -> []
      )
    
    (* Make all returns jump to a unified exit point. *)
		~per_stmt_map: 
      ( fun (ax,n,p) (ls,s) -> match s with
        | S.Return when A.has "method" ax -> [Ls.goto ~labels:ls [jump_label n]]
        | _ -> [ls,s]
      )
		
    (* Begin each method with [ yield; time >= local_time ] *)
		~proc_body_prefix:
      ( function        
        | (ax,n,_) when A.has "method" ax ->
          [ Ls.yield () ; 
            Ls.assume (E.ident clock_var |>=| E.ident local_clock_var )
          ]          
        | _ -> [] )
			
    (* Suffix each method with counter increments and decrements. *)
    ~proc_before_return: 
      ( function 
        | (ax,n,(_,ps,rs,_,_)) when A.has "method" ax -> 
          let args = List.map fst ps
          and rets = List.map fst rs
          in
          [ Ls.assign ~labels:[jump_label n]
              [Lv.ident barrier_var]
              [E.bool true];
            
            (* Decrement the "open" counter, increment the "done" counter. *)
            Ls.decr (E.nested_sel (E.ident <| open_var n) 
                      (List.map E.ident <| args @ [local_clock_var])) 1;
            Ls.incr (E.nested_sel (E.ident <| done_var n)
                      (List.map E.ident <| args @ rets @ [local_clock_var; clock_var])) 1;
                      
            (* Call the "CheckInvariant" procedure. *)
            Ls.call check_proc ~params:(
              List.flatten << List.flatten 
              << List.map (fun (m,args,rets) -> 
                (List.map (fun vs -> 
                  List.map (fun i -> 
                    E.nested_sel (E.ident <| open_var m) (vs@[E.num i])
                  ) <| List.range 0 k
                ) << List.map (List.map E.ident)
                  << List.filter (type_checks args)
                  << List.words (List.length args) 
                  <| List.map fst vals)

                @ (List.flatten << List.map (fun vs -> 
                  List.map (fun i -> 
                    List.map (fun j -> 
                      E.nested_sel (E.ident <| done_var m) (vs@[E.num i; E.num j])
                    ) <| List.range i k
                  ) <| List.range 0 k
                ) << List.map (List.map E.ident)
                  << List.filter (type_checks (args@rets))
                  << List.words (List.length args + List.length rets) 
                  <| List.map fst vals)
              ) <| methods
            );
            Ls.return ()
          ]
        
        | _ -> [] )        
    
	<| p