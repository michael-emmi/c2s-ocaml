(** Utilities for Boogie programs. *)

open Prelude
open Printf
open BplAst

module Type = struct
  include Type
  
  let rec stringify = 
    function
		| Map (_,ts,t) ->
			  sprintf "$%s$"
			  << String.concat "#"
			  << List.map stringify
			  << (flip List.tailcons) t
			  <| ts
		| t -> to_string t
end

module Expression = struct
	include Expression
	
	let sel e es = Sel (e,es)
  
  let ids_of e = 
    fold (fun ids e -> 
      match e with 
      | Id i -> List.add_uniq i ids
      | _ -> ids ) [] e

  let negate e = 
    match e with
    | Not e' -> e'
    | _ -> Not e
	
	let sum es = 
		match es with
		| e::es -> List.fold_left (fun x y -> Bin (BinaryOp.Plus, x, y)) e es
		| _ -> num 0
	
	let parse = ParsingUtils.parse_string
		BplParser.expression_top
		BplLexer.token
end

module Statement = struct
	include Statement
	module A = Attribute
  
	let skip ax = Assume ([A.unit "skip"]@ax, Expression.bool true)
  let yield ax = Assume ([A.unit "yield"]@ax, Expression.bool true)
	let async ax n ps = Call ([A.unit "async"]@ax,n,ps,[])
	  
  let is_skip = function Assume (ax,_) when A.has "skip" ax -> true | _ -> false
  let is_yield = function Assume (ax,_) when A.has "yield" ax -> true | _ -> false
  let is_short_yield = 
    function Assume (ax,_) when A.has "yield" ax -> begin
      match A.get "yield" ax with
      | [Left e] when e = Expression.num 1 -> true
      | _ -> false
    end
    | _ -> false
	let is_async = function Call (ax,_,_,_) when A.has "async" ax -> true	| _ -> false    

end

module rec LabeledStatementExt : sig
  include module type of LabeledStatement with type t = LabeledStatement.t
  
  val attrs : t -> Attribute.t list
  val has_attr : Identifier.t -> t -> bool
  
  val incr : Expression.t -> t
  
  val skip : ?labels:Identifier.t list -> ?attrs:Attribute.t list -> unit -> t
  val yield : ?labels:Identifier.t list -> ?attrs:Attribute.t list -> unit -> t
  val async : ?labels:Identifier.t list -> ?attrs:Attribute.t list -> 
    Identifier.t -> Expression.t list -> t
  
  val is_skip : t -> bool
  val is_yield : t -> bool
  val is_short_yield : t -> bool
  val is_async : t -> bool
  
  val add_labels : Identifier.t list -> t list -> t list
  
  val modifies : t list -> Identifier.t list
  val called : t list -> Identifier.t list
  val incomplete_calls : Program.t -> t list -> Type.t list list
  val complete_returns : Program.t -> t -> t list
  
  val parse : string -> t list
  
end = struct
	include LabeledStatement
  module A = Attribute
	module S = Statement
	
	let parse = ParsingUtils.parse_string
		BplParser.labeled_statements_top
		BplLexer.token
	
  let skip ?labels:ls ?attrs:ax () = Option.list ls, S.skip (Option.list ax)
	let yield ?labels:ls ?attrs:ax () = Option.list ls, S.yield (Option.list ax)
	let async ?labels:ls ?attrs:ax p ps = Option.list ls, S.async (Option.list ax) p ps

	let call ?labels:ls ?attrs:ax ?params:ps ?returns:xs p = 
    Option.list ls, Statement.Call (A.strip "async" (Option.list ax),p,Option.list ps,Option.list xs)
  
  let is_skip = S.is_skip << snd
	let is_yield = S.is_yield << snd
  let is_short_yield = S.is_short_yield << snd
  let is_async = S.is_async << snd
  
  let attrs (_,s) =
    match s with
    | S.Assume (ax,_) | S.Assert (ax,_) | S.Call (ax,_,_,_) -> ax
    | _ -> []
  let has_attr a  = List.mem_assoc a << attrs
  
	let incr e = 
		assign 
			[Lvalue.from_expr e] 
			[Expression.Bin (BinaryOp.Plus, e, Expression.num 1) ] 

	let rec add_labels ls' = 
		function [] -> (ls', Statement.skip []) :: []
		| (ls, s) :: ss -> (ls@ls', s) :: ss

	let modifies =
		fold_stmts
			( fun ms s -> match s with
			  | _, Statement.Assign (xs,_) -> List.union (List.map Lvalue.name xs) ms
			  | _, Statement.Call (_,_,_,xs) -> List.union xs ms
			  | _ -> ms )
			[]

  let calls =
    fold_stmts
      (fun ps s ->
         match s with
         | (_, Statement.Call (_,pn,xs,ys)) ->
             List.union [pn,xs,ys] ps
         | _ -> ps)
      []

  let called =
    StringSet.uniqify_list
    << List.map Tup3.fst
    << calls    
    
  let incomplete_calls pgm = 
  	List.map fst
  	<< List.filter (uncurry (<>) << Tup2.map List.length List.length)
  	<< Option.cat
   	<< List.map (fun (pn,_,ys) ->
  					 Option.map ( Tup2.ekam ys
  								  << snd
  								  << Procedure.signature )
  					 <| ProgramExt.find_proc pgm pn )
  	<< calls
	
  (* Complete the return assignments of a call p(e1,..,ek) into
     call x1,..,xn := p(e1,..,ek) . *)
  let complete_returns pgm s =
    let ignore_var_name i =	sprintf "__ignore_%n_%s" i << Type.stringify in  
  	match s with
    | ls, Statement.Call (ax,n,es,xs) -> begin
      match ProgramExt.find_proc pgm n with
      | Some p -> begin
        let _, ts = Procedure.signature p in
        if List.length xs = List.length ts 
          then [s]
        else if List.length xs > 0 
          then failwith (sprintf "Unmatched return assignment for `%s'." n)
        else begin
          warn (sprintf "Missing return assignments for `%s'; attempting to add them." n);
          [ ls, Statement.Call (ax,n,es,List.mapi ignore_var_name ts) ]
        end
      end
      | _ -> begin
        warn (sprintf "Unable to resolve procedure `%s'." n); 
        [s]
      end
    end
    | _ -> [s]

end

and DeclarationExt : sig
  include module type of Declaration with type t = Declaration.t

  val parse : string -> t list
  val post_parsing : t list -> t list
  
end = struct
	include Declaration

  let ensure_procedures_end_with_return = 
    function
    | Proc (ax,n,(tx,ps,rs,sx,ds,ss)) as d ->
        if ss = [] then d
        else begin match List.last ss with
        | _, Statement.Return -> d
        | _ -> Proc (ax,n,(tx,ps,rs,sx,ds,ss@[LabeledStatement.return ()]))
        end 
    | d -> d
  
  let post_parsing p = List.map ensure_procedures_end_with_return p
  
	let parse s = 
    post_parsing
    << ParsingUtils.parse_string
      BplParser.declarations_top
		  BplLexer.token
    <| s
end

and ProcedureExt : sig
  include module type of Procedure with type t = Procedure.t
  val mods : t -> Identifier.t list
  val fix_modifies : Program.t -> t -> t
  val add_return_assign_decls : Program.t -> t -> t
  
end = struct
  include Procedure
  
  (* Variables modified by a procedure. *)
  let mods (_,ps,rs,_,ds,ss) =
		(flip List.minus) (List.map fst ps)
		<< (flip List.minus) (List.map fst rs)
		<< (flip List.minus) (List.map Declaration.name ds)
    <| LabeledStatementExt.modifies ss    
    
  (* Recalculate the modifies clause based on global variables 
     which are	actually (recursively) modified in a procedure. *)  
  let fix_modifies pgm ((tx,ps,rs,sx,ds,ss) as p) =
    let sx = List.filter (function Specification.Modifies _ -> false | _ -> true) sx 
    and ms = 
      ProgramExt.fold_over_calls pgm 
      ( fun ms p -> List.union ms (mods p) )
      ( mods p )
      ss
    in tx, ps, rs, sx @ (if ms = [] then [] else [Specification.Modifies (false,ms)]), ds, ss

  (* Declarations for variables introduced because of 
     return-assignment completion. *)
  let add_return_assign_decls pgm (tx,ps,rs,sx,ds,ss) =
    let ignore_var_name i =	sprintf "__ignore_%n_%s" i << Type.stringify
    and max_rets = ProgramExt.fold_procs (flip <| fun (_,_,rs,_,_,_) -> max (List.length rs)) 0
    and incomplete = LabeledStatementExt.incomplete_calls pgm ss in
    let ds' = 
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
    in tx,ps,rs,sx,ds@ds',ss
end

and ProgramExt : sig
  include module type of Program with type t = Program.t
  
  val fold_over_calls : t -> ('a -> Procedure.t -> 'a) -> 'a -> LabeledStatement.t list -> 'a
  val exists_stmt : (LabeledStatement.t -> bool) -> t -> bool

  val find : t -> Identifier.t -> Declaration.t option
  val find_proc : t -> Identifier.t -> Procedure.t option
  
  val map_exprs : (Declaration.t -> Expression.t -> Expression.t) -> t -> t
  val map_stmts : (Declaration.t -> LabeledStatement.t -> LabeledStatement.t list) -> t -> t
  
  val translate : 
    ?ignore_attrs: string list ->
    ?replace_global_decls: (Declaration.t -> Declaration.t list) ->
    ?new_global_decls: Declaration.t list ->
    ?new_proc_params: (Declaration.t -> (Identifier.t * Type.t) list) ->
    ?new_proc_rets: (Declaration.t -> (Identifier.t * Type.t) list) ->
    ?new_local_decls: (Declaration.t -> Declaration.t list) ->
    ?proc_body_prefix: (Declaration.t -> LabeledStatement.t list) ->
    ?proc_body_suffix: (Declaration.t -> LabeledStatement.t list) ->
    ?per_stmt_map: (Declaration.t -> LabeledStatement.t -> LabeledStatement.t list) ->
    ?per_expr_map: (Declaration.t -> Expression.t -> Expression.t) ->
    t -> t
  
  val parse : string -> t
  val post_parsing : t -> t
  val pre_boogie : t -> t
end = struct
	include Program
  
  module A = Attribute
  module D = Declaration
  module Ls = LabeledStatementExt
			
	let find p n = List.first ((=) n << Declaration.name) p
  
	let find_proc p n =
		Option.seq (function Declaration.Proc (_,_,p) -> Some p | _ -> None)
		<| List.first ((=) n << Declaration.name 
			&&&& (function Declaration.Proc _ -> true | _ -> false)) p
  
  let rec fold_over_calls pgm fn a ss =
    let rec foc st a ss =
      List.fold_left (fun a (n,p) -> 
        fn (foc (n::st) a (Procedure.stmts p)) p) a
      << Option.cat
      << List.map (Option.seq 
        ( function 
          | Declaration.Proc (_,n,p) when not (List.mem n st) -> Some (n, p) 
          | _ -> None )
        << find pgm )
      <| LabeledStatementExt.called ss
    in foc [] a ss
    
  let exists_stmt fn pgm =
    fold_stmts (fun a s -> a || fn s) false pgm    

  let map_stmts fn =
    List.map (
      function 
      | D.Proc (ax,n,p) as d -> 
        let _, p = Procedure.map_fold_stmts (fun _ s -> (), fn d s) () p 
        in D.Proc (ax,n,p)
      | d -> d
    )  

	let map_exprs fn = 
		List.map (fun d ->
			match d with
      | D.Axiom (ax,e) -> D.Axiom (ax, Expression.map (fn d) e)
			| D.Func (ax,f,tx,ps,r,e) -> D.Func (ax,f,tx,ps,r, Option.map (Expression.map (fn d)) e)
			| D.Var (ax,n,t,e) -> D.Var (ax,n,t,Option.map (Expression.map (fn d)) e)
			| D.Proc (ax,n,p) -> D.Proc (ax,n,Procedure.map_exprs (fn d) p)
			| D.Impl (ax,n,p) -> D.Impl (ax,n,Procedure.map_exprs (fn d) p)
			| d -> d
		)     

	let translate
    ?ignore_attrs
		?(replace_global_decls = List.unit)
		?(new_global_decls = [])
		?(new_proc_params = const [])
		?(new_proc_rets = const [])
		?(new_local_decls = const [])
		?(proc_body_prefix = const [])
		?(proc_body_suffix = const [])
		?(per_stmt_map = const List.unit)
		?(per_expr_map = const id) =
    
    let ignore ax = 
      Option.reduce (List.exists (flip Attribute.has <| ax))
      false ignore_attrs
    in
		
		List.append new_global_decls
		<< List.flatten << map (
			function (Declaration.Proc (ax,n,((ts,ps,rs,es,ds,ss))) as d) 
      when not (ignore ax) ->
				let ps' = ps @ new_proc_params d
				and rs' = rs @ new_proc_rets d
				and ds' = ds @ new_local_decls d in
        
        (* Add the suffix just before each return statement.
          Note: we assume each procedure ends with a return. *)
        let ss' = 
          List.append (proc_body_prefix d)
          << snd << LabeledStatement.map_fold_stmts
            ( fun () s -> 
              match s with
              | ls, Statement.Return -> (), proc_body_suffix d @ [s]
              | _ -> (), [s] )
            () <| ss 
            
				in Declaration.Proc (ax,n,(ts,ps',rs',es,ds',ss')) :: []
			| d -> d :: [] )
      
		<< List.flatten 
    << map (fun d -> if ignore (D.attrs d) then [d] else replace_global_decls d)

    << map (fun d -> 
      
      (* Translate each statement.  Note: we must ignore certain annotated
        declarations, as well as certain annotated statements. *)
      if ignore (D.attrs d) then d else match d with
      | D.Proc (ax,n,p) -> 
        let _, p = Procedure.map_fold_stmts 
          (fun _ s -> (), if ignore (Ls.attrs s) then [s] else per_stmt_map d s) 
          () p 
        in D.Proc (ax,n,p)
      | d -> d )
      
    << map (fun d ->
      
      (* Translate each expression.  Note: we must ignore certain annotated
        declarations, but not annotated statements. *)      
      if ignore (D.attrs d) then d else match d with
      | D.Axiom (ax,e) -> D.Axiom (ax, Expression.map (per_expr_map d) e)
			| D.Func (ax,f,tx,ps,r,e) -> D.Func (ax,f,tx,ps,r, Option.map (Expression.map (per_expr_map d)) e)
			| D.Var (ax,n,t,e) -> D.Var (ax,n,t,Option.map (Expression.map (per_expr_map d)) e)
			| D.Proc (ax,n,p) -> D.Proc (ax,n,Procedure.map_exprs (per_expr_map d) p)
			| D.Impl (ax,n,p) -> D.Impl (ax,n,Procedure.map_exprs (per_expr_map d) p)
    | d -> d )
    
  let check_for_assertions p =  
    if exists_stmt (function (_, Statement.Assert _) -> true | _ -> false ) p 
    then warn "Boogie's SI-mode may not handle assertions correctly!";
    p
  
  let post_parsing p = DeclarationExt.post_parsing p
  
  let pre_boogie p = 
    check_for_assertions
    << map_procs (ProcedureExt.fix_modifies p)
    << map_procs (ProcedureExt.add_return_assign_decls p)
    << map_stmts (const <| LabeledStatementExt.complete_returns p)
    <| p
	
	let parse p = 
    post_parsing
    << ParsingUtils.parse_string
      BplParser.program_top
      BplLexer.token
    <| p
end

module Extensions = struct
  module Attribute = Attribute
  module Type = Type
  module BinaryOp = BinaryOp
  module Expression = Expression
  module Lvalue = Lvalue
  module Statement = Statement
  module LabeledStatement = LabeledStatementExt
  module Declaration = DeclarationExt
  module Specification = Specification
  module Procedure = ProcedureExt
  module Program = ProgramExt
  
end

module Abbreviations = struct
  module A = Extensions.Attribute
  module T = Extensions.Type
  module Bop = Extensions.BinaryOp
  module E = Extensions.Expression
  module Lv = Extensions.Lvalue
  module S = Extensions.Statement
  module Ls = Extensions.LabeledStatement
  module D = Extensions.Declaration
  module Sp = Extensions.Specification
  module Pc = Extensions.Procedure
  module Pg = Extensions.Program
end  

module Operators = struct
  open Abbreviations
	
	let (|:=|) x e = [ Ls.assign [Lv.from_expr x] [e] ]
	let (|::=|) xs = 
		List.flatten << List.map (uncurry (|:=|)) << List.combine xs
	
	let ($:=$) x y = E.ident x |:=| E.ident y
	let ($::=$) xs ys = List.map E.ident xs |::=| List.map E.ident ys

	(* let (|:=|) xs es = LabeledStatement.assign (List.map Lv.from_expr xs) es *)
	(* let ($:=$) xs ys = List.map E.ident xs |:=| List.map E.ident ys *)
	(* let ($:=?$) xs () = LabeledStatement.assign (List.map Lv.ident xs) (E.choice xs) *)
	let ($:=?$) xs () = [ Ls.stmt (Statement.Havoc xs) ]

	let (|&|) e f = E.conj [e;f]
	let (|||) e f = E.disj [e;f]
	let (|=>|) e f = E.Bin (Bop.Imp, e, f)

	let ($&|) x f = E.conj [E.ident x; f]

	let ($&$) x y = E.conj [E.ident x; E.ident y]
	let ($&$) x y = E.conj [E.ident x; E.ident y]
	let (!$) x = E.Not (E.ident x)

	let (!|) e = E.Not e
	let (|=|) x y = E.Bin (Bop.Eq, x, y)
	let (|!=|) x y = E.Bin (Bop.Neq, x, y)
	let (|<|) x y = E.Bin (Bop.Lt, x, y)
	let (|>|) x y = E.Bin (Bop.Gt, x, y)
	let (|<=|) x y = E.Bin (Bop.Lte, x, y)
	let (|>=|) x y = E.Bin (Bop.Gte, x, y)
		
	let (|+|) x y = E.Bin (Bop.Plus, x, y)
	let (|-|) x y = E.Bin (Bop.Minus, x, y)
	let (|*|) x y = E.Bin (Bop.Times, x, y)

	let lift_to_ids op = curry (uncurry op << Tup2.mapp E.ident)

	let ($=$) = lift_to_ids (|=|)
	let ($==$) xs = E.conj << List.map (uncurry ($=$)) << List.combine xs
	let ($<$) = lift_to_ids (|<|)
	let ($>=$) = lift_to_ids (|>=|)
end
