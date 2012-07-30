(** Utilities for Boogie programs. *)

open Prelude
open BplAst

module Declaration = struct
	include Declaration
	let parse = ParsingUtils.parse_string
		BplParser.declarations_top
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
	open Statement
	
	let parse = ParsingUtils.parse_string
		BplParser.labeled_statements_top
		BplLexer.token

	module S = Statement
	
	let skip = stmt (S.skip)
	let havoc xs = stmt (S.Havoc xs)
	let assign xs es = stmt (S.Assign (xs,es))
	let assert_ e = stmt (S.Assert ([],e))
	let assume e = stmt (S.Assume ([],e))
	let ifthenelse e ss ts = stmt (S.If (Some e,ss,ts))
	let ifthen e ss = ifthenelse e ss []
	let ifstar ss = stmt (S.If (None,ss,[]))
	let whiledo e es ss = stmt (S.While (e,es,ss))
	let whilestar ss = stmt (S.While (None,[],ss))
	let call p ps xs = stmt (S.Call ([],p,ps,xs))
	let return = stmt S.Return
	let post p ps = stmt (S.post p ps)
	let yield = stmt (S.yield)
  
  let annotate ax = stmt (S.Assume (ax, Expression.bool true))
  let is_annot ax (_,s) = S.is_annot ax s
	
	let is_yield (_,s) = S.is_yield s
	let is_short_yield (_,s) = S.is_short_yield s
	let is_async (_,s) = S.is_async s

	let incr e = 
		assign 
			[Lvalue.from_expr e] 
			[Expression.Bin (BinaryOp.Plus, e, Expression.num 1) ] 

	let rec add_labels ls' = 
		function [] -> (ls', S.skip) :: []
		| (ls, s) :: ss -> (ls@ls', s) :: ss

	let modifies =
		fold_stmts
			( fun ms s -> match s with
			  | _, Assign (xs,_) -> List.union (List.map Lvalue.name xs) ms
			  | _, Call (_,_,_,xs) -> List.union xs ms
			  | _ -> ms )
			[]

	let calls =
		fold_stmts
			(fun ps s ->
				 match s with
				 | (_, Call (_,pn,xs,ys)) ->
					   List.union [pn,xs,ys] ps
				 | _ -> ps)
			[]

	let called =
		StringSet.uniqify_list
		<< List.map Tup3.fst
		<< calls
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

module Program = struct
	include Program
	
	let rec fold_over_calls pgm fn a p =
		let rec foc st a d =
			match d with
			| D.Proc (_,n,p) ->
				  (flip fn) p
				  << List.fold_left (foc (n::st)) a
				  << List.filter (not << (flip List.mem) (n::st) << D.name)
				  << Option.cat
				  << List.map (find pgm)
				  <| LabeledStatement.called (Procedure.stmts p)
			| _ -> a
		in foc [] a (D.Proc ([],"",p))
    
  let exists_stmt fn pgm =
    fold_stmts (fun a s -> a || fn s) false pgm
	
	let parse = ParsingUtils.parse_string
		BplParser.program_top
		BplLexer.token
end

module Sp = Specification
module S = Statement
module Ls = LabeledStatement
module D = Declaration
module E = Expression
module T = Type


module Operators = struct
	module A = Attribute
	module T = Type
	module D = Declaration
	module E = Expression
	module Lv = Lvalue
	module S = Statement
	module Ls = LabeledStatement
	module Bop = BinaryOp
	
	let (|:=|) x e = [ Ls.assign [Lv.from_expr x] [e] ]
	let (|::=|) xs = 
		List.flatten << List.map (uncurry (|:=|)) << List.combine xs
	
	let ($:=$) x y = E.ident x |:=| E.ident y
	let ($::=$) xs ys = List.map E.ident xs |::=| List.map E.ident ys

	(* let (|:=|) xs es = Ls.assign (List.map Lv.from_expr xs) es *)
	(* let ($:=$) xs ys = List.map E.ident xs |:=| List.map E.ident ys *)
	(* let ($:=?$) xs () = Ls.assign (List.map Lv.ident xs) (E.choice xs) *)
	let ($:=?$) xs () = [ Ls.stmt (S.Havoc xs) ]

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


(** Recalculate the [modifies] clause based on global variables which are
	actually (recursively) modified by each procedure. *)
let fix_modifies pgm =
	Program.map_procs
		( fun ((tx,ps,rs,sx,ds,ss) as proc) ->
			  tx, ps, rs, 

			  List.filter
				  (function Sp.Modifies _ -> false | _ -> true)
				  sx
				  
			  @ ( List.reduce (fun ms -> Sp.Modifies (false,ms) :: []) []
				  << Program.fold_over_calls pgm
				  ( flip <| fun (_,ps,rs,_,ds,ss) ->
						List.union
						<< (flip List.minus) (List.map fst ps)
						<< (flip List.minus) (List.map fst rs)
						<< (flip List.minus) (List.map D.name ds)
						<| LabeledStatement.modifies ss )
				  []
				  <| proc ),

			  ds, ss )
		pgm

let ignore_var_name i =
	let rec stringify_type t =
		match t with
		| Type.Map (_,ts,t) ->
			  Printf.sprintf "$%s$"
			  << String.concat "#"
			  << List.map stringify_type
			  << (flip List.tailcons) t
			  <| ts
		| t -> Type.to_string t
	in
	Printf.sprintf "__ignore_%n_%s" i
	<< stringify_type

let incomplete_calls pgm proc =
	List.map fst
	<< List.filter (uncurry (<>) << Tup2.map List.length List.length)
	<< Option.cat
 	<< List.map (fun (pn,_,ys) ->
					 Option.map ( Tup2.ekam ys
								  << snd
								  << Procedure.signature )
					 <| Program.find_proc pgm pn )
	<< LabeledStatement.calls
	<| Procedure.stmts proc

let max_rets =
	Program.fold_procs
		(fun n (_,_,rs,_,_,_) -> max n (List.length rs))
		0

(** Declarations for variables introduced because of incomplete return
	assignments. *)
let return_assign_decls pgm proc =
	let incomplete = incomplete_calls pgm proc in
	Option.cat
	<< List.map
		(fun (i,t) ->
			 if List.exists
				 (fun ts ->
					  i < List.length ts
					  && List.nth ts i = t)
				 incomplete
			 then
				 Some (D.Var (
						   [],
						   ignore_var_name i t,
						   t, None ))
			 else None)
		
	<< List.product (List.range 0 (max_rets pgm))
	<| [ Type.Bool;
		 Type.Int;
		 Type.Map ([],[Type.Int],Type.Int)
	   ] 
	
(** Transform [n]-ary return parameter [call p(e1,..,ek)] into [call x1,..,xn
	:= p(e1,..,ek)]. *)
let dont_ignore_returns pgm s =
	match s with
	| (ls,S.Call (ax,n,es,xs)) -> begin
 		  match Program.find_proc pgm n with
		  | Some p ->
				let _, ts = Procedure.signature p in

				if List.length xs = 0
					&& List.length ts > 0
				then begin
					Printf.eprintf ( "Warning: Boogie call to `%s' is missing"
									 ^^ " return assignments; will attempt to"
									 ^^ " add them.\n" ) n;

					let xs = List.mapi ignore_var_name ts in 
					(ls, S.Call (ax,n,es,xs)) :: []
				end
				else if List.length xs = List.length ts then
					(ls, S.Call (ax,n,es,xs)) :: []

				else
					failwith
					<| Printf.sprintf
							"Unmatched return assignment for procedure `%s'."
							n
							
		  | _ -> warn
				<| Printf.sprintf "Could not resolve procedure `%s'." n;
				s :: []
	  end
	| _ -> s :: []

(** Apply whatever transformations necessary for the back-end in use. *)
let prepare_for_back_end pgm =
  (fun p -> 
    if Program.exists_stmt (function (_, S.Assert _) -> true | _ -> false ) p 
    then (warn "Boogie's SI-mode may not handle assertions correctly!"; p)
    else p )
	<< fix_modifies
	<< Program.map_stmts (const <| dont_ignore_returns pgm)
	<< Program.map_procs
		(fun ((tx,ps,rs,sx,ds,ss) as proc) ->
			 tx, ps,rs,sx, ds @ (return_assign_decls pgm proc), ss)
	<| pgm

