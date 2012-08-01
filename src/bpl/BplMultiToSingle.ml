open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Abbreviations

module Tr = BplSeqFramework

let stage_id = "M2S"

let multi_to_single p =

  let gvars =
    List.map D.name
    << List.filter (function 
      | D.Var (ax,_,_,_) when not (A.has "leavealone" ax) -> true
      | _ -> false)
    <| Program.decls p
  in
    
  let pid_t = Type.T ("pid",[]) in
  let pid_var = "self" in
  let node_vec_t t = Type.Map ([], [pid_t], t) in
  
  let root_node = 
    match Program.find p "Main" with
    | Some (D.Proc (ax,_,_)) when A.has "node" ax -> begin
      match A.get "node" ax with
      | [Left e] -> e
      | _ -> failwith (sprintf "Expected expression in :node annotation.")
    end      
    | _ -> failwith (sprintf "Expected %s procedure with :node annotation." "Main")
  in
  
  let call_to_node s = 
    match s with 
    | S.Call (ax,_,_,_) when A.has "node" ax -> begin
      match A.get "node" ax with
      | [Left e] -> Some e
      | _ -> failwith (sprintf "Unexpected node in `%s'." (S.to_string s))
    end
    | _ -> None
  in
  
  let used_pids = 
    Program.fold_stmts 
    ( fun ns (_,s) -> 
      match call_to_node s with
      | Some e -> 
        List.fold_left (flip List.add_uniq) ns (E.ids_of e)
      | _ -> ns )
    [] p
  in  
  
	let call s = 
		match s with
    | ls, S.Call (ax,_,_,_) when A.has "leavealone" ax -> [s] 
    | ls, S.Call (ax,n,ps,rs) when n = "Main" ->
      [ ls, S.Call (ax,n,ps@[root_node],rs) ]
    | ls, ((S.Call (ax,n,ps,rs)) as s) -> begin
      match call_to_node s with
      | Some e -> [ ls, S.Call (A.strip "node" ax,n,ps@[e],rs) ]
      | _ -> [ ls, S.Call (ax,n,ps@[E.ident "self"],rs) ]
    end      
		| _ -> [s]    
  in 
	
	let vectorize_expr = function
	| E.Id x when List.mem x gvars -> E.sel (E.ident x) [E.ident "self"]
	| e -> e
	in 
					
  Program.translate
    ~new_global_decls: (
      [ D.type_ "pid" ]
      @ List.map ( fun p -> D.uniq_const p pid_t ) used_pids
      @ [ D.axiom << E.forall ["p",pid_t] << E.disj 
          << List.map (fun pid -> "p" $=$ pid) 
          <| used_pids ]
    )
    
  	~replace_global_decls: 
  		( fun d -> match d with 
        | D.Var (_,_,_,Some _) ->
          failwith "ToDo: handle where clause in FiFoseq translation."
            
        | D.Var (ax,g,t,wh) when not (A.has "leavealone" ax) -> 
          [ D.Var (ax, g, node_vec_t t, wh) ]
          
        | D.Proc (ax,n,p) -> [ D.Proc (A.strip "node" ax, n, p) ]

  		  | _ -> [d] )
			
		~new_proc_params: 
			(fun (ax,n,_) ->
				if A.has "leavealone" ax then []
        else if List.mem n [] then []
				else [pid_var, pid_t] )
          
    ~per_expr_map:
      (fun n -> vectorize_expr)
        
    ~per_stmt_map: (fun n -> call)
        
    p
