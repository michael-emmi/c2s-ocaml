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
  
	let skip = Assume ([], Expression.bool true)
  let annot ax = Assume (ax, Expression.bool true)
	let yield = Call ([A.unit "yield"], "yield", [], [])
	let post n ps = Call ([A.unit "async"],n,ps,[])
	
  let is_annot ax = 
    function Assume (ax', _) -> List.exists (fun a -> List.mem a ax) ax' 
    | _ -> false
  
	let is_yield = 
		function Call (ax,"yield",[],[]) when A.has "yield" ax -> true
		| _ -> false
		
	let is_short_yield =
		function Call (ax,"yield",[],[]) when A.has "yield" ax -> begin
			match A.get "yield" ax with
			| [Left e] when e = Expression.num 1 -> true
			| _ -> false
		end 
		| _ -> false
		
	let is_async = 
		function Call (ax,_,_,_) when A.has "async" ax -> true
		| _ -> false    

end

module LabeledStatement = struct
	include LabeledStatement
	module S = Statement
	
	let parse = ParsingUtils.parse_string
		BplParser.labeled_statements_top
		BplLexer.token
	
	let skip = stmt (Statement.skip)
	let havoc xs = stmt (Statement.Havoc xs)
	let assign xs es = stmt (Statement.Assign (xs,es))
	let assert_ e = stmt (Statement.Assert ([],e))
	let assume e = stmt (Statement.Assume ([],e))
	let ifthenelse e ss ts = stmt (Statement.If (Some e,ss,ts))
	let ifthen e ss = ifthenelse e ss []
	let ifstar ss = stmt (Statement.If (None,ss,[]))
	let whiledo e es ss = stmt (Statement.While (e,es,ss))
	let whilestar ss = stmt (Statement.While (None,[],ss))
	let call p ps xs = stmt (Statement.Call ([],p,ps,xs))
	let return = stmt Statement.Return
	let post p ps = stmt (Statement.post p ps)
	let yield = stmt (Statement.yield)
  
  let annotate ax = stmt (Statement.Assume (ax, Expression.bool true))
  let is_annot ax (_,s) = Statement.is_annot ax s
	
	let is_yield (_,s) = Statement.is_yield s
	let is_short_yield (_,s) = Statement.is_short_yield s
	let is_async (_,s) = Statement.is_async s

	let incr e = 
		assign 
			[Lvalue.from_expr e] 
			[Expression.Bin (BinaryOp.Plus, e, Expression.num 1) ] 

	let rec add_labels ls' = 
		function [] -> (ls', Statement.skip) :: []
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
  					 <| Program.find_proc pgm pn )
  	<< calls
	
  (* Complete the return assignments of a call p(e1,..,ek) into
     call x1,..,xn := p(e1,..,ek) . *)
  let complete_returns pgm s =
    let ignore_var_name i =	sprintf "__ignore_%n_%s" i << Type.stringify in  
  	match s with
    | ls, Statement.Call (ax,n,es,xs) -> begin
      match Program.find_proc pgm n with
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

module Declaration = struct
	include Declaration

  let ensure_procedures_end_with_return = 
    function
    | Proc (ax,n,(tx,ps,rs,sx,ds,ss)) as d ->
        if ss = [] then d
        else begin match List.last ss with
        | _, Statement.Return -> d
        | _ -> Proc (ax,n,(tx,ps,rs,sx,ds,ss@[LabeledStatement.return]))
        end 
    | d -> d
  
  let post_parsing = List.map ensure_procedures_end_with_return
  
	let parse = 
    post_parsing
    << ParsingUtils.parse_string
      BplParser.declarations_top
		  BplLexer.token
end

module rec ProcedureExt : sig
  include module type of Procedure
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
    <| LabeledStatement.modifies ss    
    
  (* Recalculate the modifies clause based on global variables 
     which are	actually (recursively) modified in a procedure. *)  
  let fix_modifies pgm ((tx,ps,rs,sx,ds,ss) as p) =
    let sx = List.filter 
      (function Specification.Modifies _ -> false | _ -> true) sx
    and ms = 
      ProgramExt.fold_over_calls pgm 
      ( fun ms -> List.union ms << mods )
      ( mods p )
      ss
    in tx, ps, rs, sx @ [Specification.Modifies (false,ms)], ds, ss

  (* Declarations for variables introduced because of 
     return-assignment completion. *)
  let add_return_assign_decls pgm (tx,ps,rs,sx,ds,ss) =
    let ignore_var_name i =	sprintf "__ignore_%n_%s" i << Type.stringify
    and max_rets = ProgramExt.fold_procs (flip <| fun (_,_,rs,_,_,_) -> max (List.length rs)) 0
    and incomplete = LabeledStatement.incomplete_calls pgm ss in
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
  include module type of Program
  val fold_over_calls : t -> ('a -> Procedure.t -> 'a) -> 'a -> LabeledStatement.t list -> 'a
  val exists_stmt : (LabeledStatement.t -> bool) -> t -> bool
  val parse : string -> t
  val post_parsing : t -> t
  val pre_boogie : t -> t
end = struct
	include Program
  
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
      <| LabeledStatement.called ss
    in foc [] a ss
    
  let exists_stmt fn pgm =
    fold_stmts (fun a s -> a || fn s) false pgm
    
  let check_for_assertions p =  
    if exists_stmt (function (_, Statement.Assert _) -> true | _ -> false ) p 
    then warn "Boogie's SI-mode may not handle assertions correctly!";
    p
  
  let post_parsing p = Declaration.post_parsing p
  
  let pre_boogie p = 
    check_for_assertions
    << map_procs (ProcedureExt.fix_modifies p)
    << map_procs (ProcedureExt.add_return_assign_decls p)
    << map_stmts (const <| LabeledStatement.complete_returns p)
    <| p
	
	let parse p = 
    post_parsing
    << ParsingUtils.parse_string
      BplParser.program_top
      BplLexer.token
    <| p
end

module Procedure = ProcedureExt
module Program = ProgramExt

module Abbreviations = struct
  module A = Attribute
  module T = Type
  module Bop = BinaryOp
  module E = Expression
  module Lv = Lvalue
  module S = Statement
  module Ls = LabeledStatement
  module D = Declaration
  module Sp = Specification
  module Pc = Procedure
  module Pg = Program
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
