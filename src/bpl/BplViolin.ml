open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

module M = BplMarkers

let instrument p =

  let methods = 
    List.collect (function 
      | D.Axiom (ax,_) when A.has "method" ax -> begin
        match A.get "method" ax with
        | [Right a; Right b] -> Some (b,a)
        | _ -> warn "unexpected {:method ..} attribute."; None
      end
      | _ -> None
    )
    <| Program.decls p in

  let specs = 
    List.collect (function 
      | D.Func (ax,n,_,ps,_,_) when A.has "spec" ax -> Some (n, List.map (Option.some << fst) ps)
      | _ -> None
    )
    <| Program.decls p in
    
  let inits =
    List.collect (function 
      | D.Proc (ax,n,_) when A.has "init" ax -> Some n
      | _ -> None
    )
    <| Program.decls p in

  if inits = [] then 
    warn "did not find initialization procedures.";

  Program.translate
    ~new_local_decls: (fun (ax,n,p) ->
      if List.mem_assoc n methods then [D.var "$myop" T.Int] else []
    )
    ~proc_body_prefix: (fun (ax,n,p) -> 
      if List.mem_assoc n methods then [
        Ls.call (sprintf "%s.start" <| List.assoc n methods)
          ~params:(List.map (E.ident << fst) <| Procedure.params p)
          ~returns:["$myop"]
      ] else if A.has "entrypoint" ax || n = "main" then
        List.map Ls.call inits
      else []
    )
    ~proc_before_return: (fun (ax,n,p) ->
      if List.mem_assoc n methods then [
        Ls.call (sprintf "%s.finish" <| List.assoc n methods)
          ~params:(E.ident "$myop" :: List.map (E.ident << fst) (Procedure.returns p))
      ]
      else []
    )
    ~per_stmt_map: (fun (ax,n,p) s ->
      match s with
      | _, S.Assert (ax,_) when A.has "spec" ax -> begin
        match A.get "spec" ax with
        | [Right a] -> 
          if List.mem_assoc a specs then 
            s :: Ls.assert_ (E.fn a << List.map E.ident <| List.assoc a specs) :: []
          else begin
            warn "did not find specification function.";
            s :: []
          end
        | _ -> warn "unexpected {:spec ..} attribute."; s :: []
      end
      | _ -> s :: []
    )
    p


(* HACK : inject the {:method} attribute for certain procedure names. *)
let inject_method_attribute =
  Program.translate
    ~replace_global_decls:( 
      let method_names = [
        "push"; "pop";
        "Push"; "Pop";
        "enqueue"; "dequeue";
        "Enqueue"; "Dequeue"
      ] in
      function
      | D.Proc (ax,name,p) when List.mem name method_names ->
        D.Proc (A.unit "method" :: ax, name, p) :: []
      | d -> d :: []
    )
    
let instrument_with_method_attributes k p =
  let prefix = "Violin" in
  
  let clock_var = sprintf "%s.time" prefix
  and init_clock_var = "#t0" 
  and local_clock_var = "#t" 
  and barrier_var = sprintf "%s.ret" prefix
  and open_var = sprintf "%s.%s.open" prefix
  and done_var = sprintf "%s.%s.done" prefix
  in
  
  let open_expr m t ps =
    List.fold_left 
      (fun e x -> E.sel e [E.ident x])
      (E.ident (open_var m)) 
    << List.cons t
    << List.map fst
    <| ps
  in
  
  let done_expr m t1 t2 ps rs =
    List.fold_left 
      (fun e x -> E.sel e [E.ident x])
      (E.ident (done_var m)) 
    << List.cons t1
    << List.cons t2
    << List.map fst
    <| ps @ rs
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
  (* let vals =
    Option.cat
    << List.map (function 
      | D.Const (ax,_,x,t,_) when A.has "value" ax -> Some (x,t)
      | _ -> None )
    <| Program.decls p
  in *)

  let valid_time_func =
    List.hd << D.parse <| "
      function Violin.valid_time(t: int) returns (bool) {
        t >= 0 && t <= Violin.TIME_BOUND 
      }
    "
  in

  let init_proc =
    D.proc "Violin.Init"
      ~attrs:[]
      ~params:[]
      ~returns:[]
      ~decls:[]
      ~body:(
        [ E.ident "Violin.time" |:=| E.num 0 ;
          E.ident "Violin.ret" |:=| E.bool false ]
        @ List.map (fun (m,ps,rs) -> 
            let vars = ["t",T.Int] @ ps in
            Ls.assume (E.forall vars (open_expr m "t" ps |=| E.num 0)) )
          methods
        @ List.map (fun (m,ps,rs) ->
            let vars = ["t1",T.Int; "t2",T.Int] @ ps @ rs in
            Ls.assume (E.forall vars (done_expr m "t1" "t2" ps rs |=| E.num 0)) )
          methods
      )
  in
  
  let tick_proc =
    List.hd <| D.parse "
      procedure Violin.Tick() {
        while (*) {
          assume Violin.time < Violin.TIME_BOUND;
          assume {:yield} true;
          if (Violin.ret) {
            Violin.time := Violin.time + 1;
            Violin.ret := false;
          }
        }
      }      
    "
  in
    
  let check_proc =
    D.proc "Violin.CheckInvariant"
      ~attrs:[]
      ~params:[]
      ~returns:[]
      ~decls:[]
      ~body:[]    
  in
    
  let new_global_decls =
    [ D.const "Violin.TIME_BOUND" T.Int ;
      D.axiom (E.ident "Violin.TIME_BOUND" |=| E.num k ) ;
      D.var clock_var T.Int ;
      D.var barrier_var T.Bool ;
      valid_time_func ;
      init_proc ;
      tick_proc ;
      check_proc ]
      
    @ List.map 
      ( fun (m,ps,_) -> 
        D.var (open_var m) 
        << List.fold_left (fun t _ -> T.map [T.Int] t) (T.map [T.Int] T.Int)
        <| ps )
      methods
    
    @ List.map
      (fun (m,ps,rs) -> 
        D.var (done_var m)
        << List.fold_left (fun t _ -> T.map [T.Int] t) (T.map [T.Int] << T.map [T.Int] <| T.Int)
        <| ps @ rs )
      methods
  in
  
  (* let type_checks =
    List.for_all2 (fun (_,t) v -> 
      try List.assoc v vals = t
      with Not_found -> failwith "Problem with method arg type-checking!" )
  in *)
	
	if List.length methods < 1 then
		failwith "This program has no methods! \
			Annotate some procedures with {:method}.";
	
	Program.translate
    
    ~append_global_decls:( new_global_decls )

    (* Add [local_time] parameter to each procedure. *)
		~new_proc_params: 
      ( function 
        | (ax,_,_) when A.has "method" ax -> [init_clock_var, T.Int]
        | _ -> []
      )

    ~new_local_decls:(const [ D.var local_clock_var T.Int ])
    
    (* Make all returns jump to a unified exit point. *)
    (* ~per_stmt_map:
      ( fun (ax,n,p) (ls,s) -> match s with
        | S.Return when A.has "method" ax -> [Ls.goto ~labels:ls [jump_label n]]
        | _ -> [ls,s]
      ) *)
      
    ~per_stmt_map: ( fun (ax,n,p) -> function
      | ls, S.Call (ax,n,ps,rs) when List.mem n (List.map fst3 methods) ->   
  			(ls, S.Call (ax, n, ps@[E.ident clock_var], rs)) :: []
      | s -> s :: []
      )
		
    (* Begin each method with [ yield; time >= local_time ] *)
		~proc_body_prefix:
      ( function        
        | (ax,n,(_,ps,_,_,_)) when A.has "method" ax ->
          [ Ls.assume (E.ident local_clock_var |>=| E.ident init_clock_var) ;
            Ls.assume (E.ident local_clock_var |<=| E.ident clock_var) ;
            Ls.incr (open_expr n local_clock_var ps) 1;
          ]
        | (ax,_,_) when A.has M.entrypoint ax ->
          Ls.call (D.name init_proc) :: []

        | _ -> [] )
			
    (* Suffix each method with counter increments and decrements. *)
    ~proc_before_return: 
      ( function 
        | (ax,n,(_,ps,rs,_,_)) when A.has "method" ax -> 
          
          [ Ls.assign (* ~labels:[jump_label n] *)
              [Lv.ident barrier_var]
              [E.bool true];
            
            (* Decrement the "open" counter, increment the "done" counter. *)
            Ls.decr (open_expr n local_clock_var ps) 1;
            Ls.incr (done_expr n local_clock_var clock_var ps rs) 1;

            (* Call the "CheckInvariant" procedure. *)
            Ls.call (D.name check_proc) (* ~params:(
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
            ) *);
          ]
          
        | (ax,_,_) when A.has M.entrypoint ax ->
          Ls.async (D.name tick_proc) [] :: []
        
        | _ -> [] )    

	<| p
  
let instrument_barriers k =
  instrument_with_method_attributes k
  << inject_method_attribute
