(** Abstract syntax tree for concurrent programs. *)

open Prelude
open Printf

let indent_size = 4

module Identifier = struct
    type t = string

	let rename old new_ id = if id = old then new_ else id

	open PrettyPrinting
	let print = text
	let print_seq = sep << punctuate comma << List.map print
	let to_string = id
end

module Attribute = struct
	type t = string * string list
	open PrettyPrinting
	let print (a,vs) =
		oper "{:"
		<-> text a
		<+> (sep << punctuate comma <| List.map text vs )
		<-> oper ":}"

	let print_seq = sep << List.map print
	let to_string = render << print
end

module Type = struct
	type t = Void | Bool | Int
			 | Intv of int * int
			 | T of Identifier.t
			 | Tuple of t list
			 | Map of t list * t

	let compress ts =
		match ts with
		| [] -> Void
		| t::[] -> t
		| ts -> Tuple ts

	let rec flatten t = match t with
		| Void -> []
		| Bool | Int | Intv _ | T _ | Map _ -> t :: []
		| Tuple ts -> List.flatten (List.map flatten ts)

	let arity = List.length << flatten

	open PrettyPrinting
	let rec print t = match t with 
		| Void -> keyword "void"
		| Bool -> keyword "bool"
		| Int -> keyword "int"
		| Intv (a,b) ->
			  keyword "int"
			  <-> parens (int a <-> oper ".." <-> int b)
		| T n -> Identifier.print n
		| Tuple ts -> parens (print_seq ts)
		| Map (ts,t) -> brackets (print_seq ts) <+> print t
	and print_seq ts = sep << punctuate comma << List.map print <| ts
	let to_string = render << print
end

module Literal = struct
	type t = Bool of bool
			 | Num of int

	open PrettyPrinting
	let print = function
		| Bool b -> text (if b then "true" else "false")
		| Num i -> int i
	let to_string = render << print
end

module BinaryOp = struct
	type t = Iff | Imp | Or | And
			 | Eq | Neq | Lt | Gt | Lte | Gte
			 | Plus | Minus | Times | Div | Mod
			
	let is_boolean_op = function
		| Iff | Imp | Or | And -> true
		| _ -> false
			
	let boolean_op_to_caml_op = function
		| Iff -> (=)
		| Imp -> (fun a b -> if a then b else true)
		| Or -> (||)
		| And -> (&&)
		| _ -> failwith "boolean_op_to_caml_op w/ non-Boolean operator."
			
	open PrettyPrinting
	let to_string = function
		| Iff -> "<=>"
		| Imp -> "=>"
 		| Or -> "|"
		| And -> "&"
		| Eq -> "="
		| Neq -> "!="
		| Lt -> "<"
		| Gt -> ">"
		| Lte -> "<="
		| Gte -> ">="
		| Plus -> "+"
		| Minus -> "-"
		| Times -> "*"
		| Div -> "/"
		| Mod -> "%"
	let print = oper << to_string
end

module Expression = struct
	type q = Forall | Exists
	type t =
		Id of Identifier.t	
		| Lit of Literal.t
		| Not of t
		| Neg of t
		| Bin of BinaryOp.t * t * t
		| Choice
		| Schoose of t list
		| Tuple of t list

		| Sel of t * t list
		| Upd of t * t list * t

		| Self

		| Old of t
		| FnApp of Identifier.t * t list
		| Q of q
			  * (Identifier.t * Type.t) list
			  * t

	let ident s = Id s
	let bool b = Lit (Literal.Bool b)
	let num n = Lit (Literal.Num n)
	let sel e fs = Sel (e,fs)
	let upd e fs g = Upd (e,fs,g)
	let choice ls = List.map (const Choice) ls
	let eq e f = Bin (BinaryOp.Eq, e, f)
	let neq e f = Bin (BinaryOp.Neq, e, f)
	let forall xs e = Q (Forall, xs, e)
	let exists xs e = Q (Exists, xs, e)
	
	let rec map fn e = fn <| match e with
		| Not e -> Not (map fn e)
		| Neg e -> Neg (map fn e)
		| Bin (op,e,f) -> Bin (op, map fn e, map fn f)
		| Schoose es -> Schoose (List.map (map fn) es)
		| Tuple es -> Tuple (List.map (map fn) es)
		| Sel (e,es) -> Sel (map fn e, List.map (map fn) es)
		| Upd (e,es,f) -> Upd (map fn e, List.map (map fn) es, map fn f)
		| Old e -> Old (map fn e)
		| FnApp (f,es) -> FnApp (f, List.map (map fn) es)
		| Q (q,xs,e) -> Q (q,xs,map fn e)
		| _ -> e
		
	let rec occurs fn e =
		fn e or begin match e with
		| Id _ | Lit _ | Self | Choice -> false
		| Not e | Neg e | Old e | Q (_,_,e) -> occurs fn e
		| Bin (op,e,f) -> occurs fn e or occurs fn f
		| Schoose es | Tuple es | FnApp (_,es) -> List.exists (occurs fn) es
		| Sel (e,es) -> occurs fn e or List.exists (occurs fn) es
		| Upd (e,es,f) -> 
			occurs fn e or List.exists (occurs fn) es or occurs fn f
		end

	let rec det pol e =
		match e with
		| Id _ | Lit _ | Self -> e
		| Choice -> bool pol
		| Not e -> Not (det (not pol) e)
		| Neg e -> Neg (det pol e)
		| Bin (op,e,f) -> Bin (op, det pol e, det pol f)
		| Schoose es -> Schoose (List.map (det pol) es)
		| Tuple es -> Tuple (List.map (det pol) es)
		| Sel (e,es) -> Sel (det pol e, List.map (det pol) es)
		| Upd (e,es,f) -> Upd (det pol e, List.map (det pol) es, det pol f)
		| Old e -> Old (det pol e)
		| FnApp (f,es) -> FnApp (f, List.map (det pol) es)
		| Q (q,xs,e) -> Q (q,xs,det pol e)

	let rec simplify =
		map (function Bin (op,e,f) -> begin
				match e, op, f with
				| Lit (Literal.Bool b1), _, Lit (Literal.Bool b2) 
				when BinaryOp.is_boolean_op op -> 
					Lit (Literal.Bool (b1 = b2))

				| Lit (Literal.Bool b), BinaryOp.Iff, e 
				| e, BinaryOp.Iff, Lit (Literal.Bool b) -> 
					if b then e else Not e

				| Lit (Literal.Bool b), BinaryOp.Imp, e -> 
					if b then e else bool true

				| e, BinaryOp.Imp, Lit (Literal.Bool b) -> 
					if b then bool true else Not e

				| Lit (Literal.Bool b), BinaryOp.And, e 
				| e, BinaryOp.And, Lit (Literal.Bool b) ->
					if b then e else bool false
					
				| Lit (Literal.Bool b), BinaryOp.Or, e 
				| e, BinaryOp.Or, Lit (Literal.Bool b) ->
					if b then bool true else e

				| _ -> Bin (op,e,f)
			end
			 | e -> e)

	let map_ident fn =
		map (function Id x -> Id (fn x) | e -> e)
		
	let conj = function
		| [] -> bool true
		| e::[] -> e
		| e::es -> List.fold_left (fun e f -> Bin (BinaryOp.And, e, f)) e es
	let disj = function
		| [] -> bool false
		| e::[] -> e
		| e::es -> List.fold_left (fun e f -> Bin (BinaryOp.Or, e, f)) e es

	open PrettyPrinting
	let rec print e = match e with
		| Id x -> Identifier.print x
		| Lit c -> Literal.print c
		| Not e -> parens (oper "!" <-> print e)
		| Neg e -> oper "-" <-> parens (print e)
		| Bin (op,e,e') -> 
			  parens ( print e <+> BinaryOp.print op <+> print e' )
		| Choice -> oper "*"
		| Schoose es -> keyword "schoose" <+> brackets (print_seq es)
		| Tuple es -> parens (print_seq es)
		| Sel (e,es) -> print e <-> brackets (print_seq es)
		| Upd (e,es,f) ->
			  print e <-> brackets (print_seq es <+> oper ":=" <+> print f)
		| Self -> keyword "self"
		| Old e -> keyword "old" <-> parens (print e)
		| FnApp (f,es) -> Identifier.print f <-> parens (print_seq es)
		| Q (q,xs,e) ->
			  parens (
				  keyword (match q with Forall -> "forall" | _ -> "exists")
				  <+> ( sep << punctuate comma
						<< List.map (fun (x,t) ->
										 Identifier.print x
										 <-> colon <+> Type.print t)
						<| xs )
				  <+> oper "::"
				  <+> print e )

	and print_seq es = sep << punctuate comma << List.map print <| es
	let to_string = render << print
end
module E = Expression

module Lvalue = struct
	type t = Id of Identifier.t
			 | Sel of t * Expression.t list
	let rec name lv = match lv with Id x -> x | Sel (x,_) -> name x
	let sel x es = Sel (Id x,es)
	let ident i = Id i
	let rec from_expr = function
		| Expression.Id x -> Id x
		| Expression.Sel (e,es) -> Sel (from_expr e, es)
		| e -> failwith
			  << sprintf
					"CpAst.Lvalue.from_expr: unexpected expression `%s'."
			  <| Expression.to_string e
	let rec to_expr lv = match lv with
		| Id x -> Expression.Id x
		| Sel (x,es) -> Expression.Sel (to_expr x, es)

	let lift fn = from_expr << fn << to_expr

	let rec map fn lv =
		fn <| match lv with
		| Id x -> Id x
		| Sel (x,es) -> Sel (map fn x, es)
		
	let rec map_exprs fn lv = match lv with
		| Id _ -> lv
		| Sel (x,es) -> Sel ( map_exprs fn x,
							  List.map (Expression.map fn) es )

	let rec map_ident fn lv = match lv with
		| Id x -> Id (fn x)
		| Sel (x,es) -> Sel (map_ident fn x, List.map (E.map_ident fn) es)

	open PrettyPrinting
	let rec print lv = match lv with
		| Id i -> Identifier.print i
		| Sel (x,es) ->
			  print x <-> brackets (Expression.print_seq es)
	let print_seq = sep << punctuate comma << List.map print 
	let to_string = render << print
end
module Lv = Lvalue

module rec Statement : sig
	type t =
		Skip
		| Assign of Lvalue.t list * Expression.t list * Expression.t option
		| Assert of Expression.t
		| Assume of Expression.t
		| Goto of Identifier.t list
		| Dead of Identifier.t list
		| Ite of Expression.t * LabeledStatement.t list *
			  LabeledStatement.t list
		| While of
			  Expression.t
			  * Expression.t list
			  * LabeledStatement.t list
		| Call of Identifier.t * Expression.t list * Lvalue.t list
		| Return of Expression.t list
		| Post of Identifier.t * Expression.t list * Expression.t option
		| New of Expression.t
		| Yield

	val is_atomic : t -> bool
	val to_string : t -> string

	val print : t -> PrettyPrinting.doc
end = struct
	type t =
		Skip
		| Assign of Lvalue.t list * Expression.t list * Expression.t option
		| Assert of Expression.t
		| Assume of Expression.t
		| Goto of Identifier.t list
		| Dead of Identifier.t list
		| Ite of Expression.t * LabeledStatement.t list *
			  LabeledStatement.t list
		| While of
			  Expression.t
			  * Expression.t list
			  * LabeledStatement.t list
		| Call of Identifier.t * Expression.t list * Lvalue.t list
		| Return of Expression.t list
		| Post of Identifier.t * Expression.t list * Expression.t option
		| New of Expression.t
		| Yield

	let is_atomic = function
		| Skip | Goto _ | Return _ | Assign _ | Assert _ | Assume _
		| Call _ | Dead _ | Post _ | New _ | Yield -> true
		| _ -> false

	let rec print s =
		let open PrettyPrinting in
		match s with
		| Skip -> keyword "skip"
		| Goto ids -> keyword "goto" <+> Identifier.print_seq ids
		| Return es -> keyword "return" <+> Expression.print_seq es
		| Assign (lvs,es,ce) ->
			  let constrain = match ce with
				  | None -> empty
				  | Some e ->
						keyword "constrain"
						<+> parens (Expression.print e)
			  in
			  Lvalue.print_seq lvs
			  <+> oper ":="
			  <+> Expression.print_seq es
			  <+> constrain
		| Ite (e,ss,ss') ->
			  let cases = LabeledStatement.as_cases
				  (LabeledStatement.S ([],s)) in

			  ( vcat
				<< List.mapi
				(fun i (e,ss) ->
					 ( match i, e with
					   | 0, Some e ->
							 keyword "if"
							 <+> Expression.print e
							 <+> keyword "then"
					   | i, None when i = List.length cases - 1 ->
							 keyword "else"
					   | _, Some e ->
							 keyword "else"
							 <+> keyword "if"
							 <+> Expression.print e
							 <+> keyword "then"
					   | _ -> failwith
							 "CpAst.Statement.print: Unexpected cases.." )
						 $-$ indent indent_size (LabeledStatement.print_seq ss)
				)
				<| cases )
				  $-$ ( sep << List.make (const (keyword "fi"))
						<| List.length cases - 1 )
				  
		(* 			  keyword "if" *)
		(* 			  <+> Expression.print e *)
		(* 			  <+> keyword "then" *)
		(* 			  $-$ indent indent_size (LabeledStatement.print_seq ss) *)
		(* 			  $-$ keyword "else" *)
		(* 			  $-$ indent indent_size (LabeledStatement.print_seq ss') *)
		(* 			  $-$ keyword "fi" *)
		| While (e,es,ss) ->
			  ( if es = [] then
					keyword "while"
					<+> Expression.print e
					<+> keyword "do"
				else begin
					keyword "while"
						$+$ indent indent_size ( vcat
												 << List.map
												 ( (<+>) (keyword "invariant")
												   << Expression.print )
												 <| es )
						$+$ keyword "do"
				end )
				  $-$ indent indent_size (LabeledStatement.print_seq ss)
				  $-$ keyword "done"
		| Assert e -> keyword "assert" <+> Expression.print e
		| Assume e -> keyword "assume" <+> Expression.print e 
		| Call (p,es,rs) ->
			  let assign = match rs with
				  | [] -> empty
				  | _ -> Lvalue.print_seq rs <+> oper ":="
			  in
			  keyword "call"
			  <+> assign
			  <+> Identifier.print p
			  <+> parens (Expression.print_seq es)
		| Dead ds ->
			  keyword "dead" <+> Identifier.print_seq ds
		| Post (p,es,e) ->
			  keyword "post"
			  <+> Identifier.print p
			  <+> parens (Expression.print_seq es)
			  <+> option Expression.print e
		| New x -> keyword "new" <+> Expression.print x
		| Yield -> keyword "yield"
				  
	let to_string = PrettyPrinting.render << print
end
	
and LabeledStatement : sig
	type t = S of Identifier.t list * Statement.t
			 | C of string

	val comment : string -> t
	val stmt : Statement.t -> t

	val map : (Statement.t -> Statement.t) -> t -> t

	val map_exprs : (Expression.t -> Expression.t) -> t list -> t list
	val map_lvals : (Lvalue.t -> Lvalue.t) -> t list -> t list
	val map_ident : (Identifier.t -> Identifier.t) -> t list -> t list

	val add_labels : Identifier.t list -> t list -> t list
	val add_params : Identifier.t list -> Expression.t list -> t -> t

	val one_label_per_stmt : t -> t list
	val one_target_per_goto : t -> t list
	val no_dead_stmts : t -> t list
	val no_constrain_clauses : t -> t list

	val fold_left_rec : ('a -> t -> 'a) -> 'a -> t list -> 'a

	val contains_rec : (t -> bool) -> t list -> bool

	val map_stmts : (t -> t list) -> t list -> t list

	val case : (Expression.t * t list) list -> t
	val as_cases : t -> (Expression.t option * t list) list

	val cons_label : Identifier.t -> t -> t
		
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t = S of Identifier.t list * Statement.t
			 | C of string

	open Statement

	let map fn = function S (ls,s) -> S (ls,fn s) | C c -> C c

	let map_exprs_shallow fn = 
		function
		| Assign (ls,es,e) ->
			  Assign (List.map (Lvalue.map_exprs fn) ls,
						List.map (E.map fn) es,
						Option.map (E.map fn) e)
		| Assert e -> Assert (E.map fn e)
		| Assume e -> Assume (E.map fn e)
		| Ite (e,ss,ts) -> Ite (E.map fn e, ss, ts)
		| While (e,es,ss) -> While (E.map fn e, List.map (E.map fn) es, ss)
		| Call (p,es,ls) ->
			Call (p, List.map (E.map fn) es, List.map (Lvalue.map_exprs fn) ls)
		| Return es -> Return (List.map (E.map fn) es)
		| Post (p,es,e) ->
			  Post ( p, List.map (E.map fn) es,
					   Option.map (E.map fn) e )
		| New e -> New (E.map fn e)
		| s -> s

	let map_lvals_shallow fn = function
		| Assign (xs,es,c) -> Assign (List.map (Lvalue.map fn) xs, es, c)
		| Call (p,es,xs) -> Call (p, es, List.map (Lvalue.map fn) xs)
		| s -> s

	let comment c = C c
	let stmt s = S ([],s)
	let rec add_labels ls' = function
		| [] -> S(ls', Statement.Skip) :: []
		| S (ls, s) :: ss -> S (ls@ls', s) :: ss
		| C c :: ss -> C c :: add_labels ls' ss
	let add_params xs' es' = function
		| S (ls, Call (p,es,xs)) -> 
			S (ls, Call (p,es@es',xs@(List.map Lvalue.ident xs')))
		| S (ls, Post (p,es,e)) -> S (ls, Post (p,es@es',e))
		| S (ls, Return es) -> S (ls, Return (es@es'))
		| S (ls, Assign (xs,es,c)) ->
			  S (ls, Assign (xs@(List.map Lvalue.ident xs'),es@es',c))
		| S (ls, Goto xs) -> S (ls, Goto (xs@xs'))
		| S (ls, Dead xs) -> S (ls, Dead (xs@xs'))
		| s -> s

	let rec fold_left_rec fn =
		List.fold_left
			( fun a s ->
(* 				  Printf.printf "FLR: %s\n" (to_string s); *)
				  ( match s with
					| S (ls, Ite (e,tss,ess)) ->
						  (flip <| fold_left_rec fn) ess
						  << (flip <| fold_left_rec fn) tss
					| S (ls, While (e,es,ss)) ->
						  (flip <| fold_left_rec fn) ss
					| _ -> id )
				  <| fn a s )

	let contains_rec p = fold_left_rec (fun b s -> b or p s) false

	let rec map_stmts fn ss =
		List.flatten << List.map (map_stmt fn) <| ss
	and map_stmt fn s = 
		match s with
		| S (ls,s) when is_atomic s -> fn (S (ls,s))
		| S (ls,Ite (e,tss,ess)) ->
			  fn (S (ls, Ite ( e, map_stmts fn tss, map_stmts fn ess )))
		| S (ls,While (e,es,ss)) ->
			  fn (S (ls, While (e, es, map_stmts fn ss)))
		| C c -> fn (C c)
		| _ -> assert false

	let map_exprs fn =
		map_stmts (List.unit << map (map_exprs_shallow fn))

	let map_lvals fn =
		map_stmts (List.unit << map (map_lvals_shallow fn))

	let map_ident fn =
		map_stmts (
			List.unit
			<< map (map_lvals_shallow (Lvalue.map_ident fn))
			<< map (map_exprs_shallow (Expression.map_ident fn)) )

	let cons_label l = function
		| S (ls,s) -> S (l::ls,s)
		| c -> c

	let rec case cs = match cs with
		| (e,ss)::cs -> S ([], Statement.Ite (e, ss, [case cs]))
		| [] -> S ([], Statement.Skip)

	let as_cases s =
		let rec collect cs ss =
			match ss with
			| [S (ls, Statement.Ite (e,ts,us))] ->
				  if ls <> [] then
					  Printf.eprintf
						  ( "Warning: Losing labels %s in " 
							^^ "BplAst.LabeledStatement.as_cases.\n" )
						  (String.concat ", " ls);
				  collect ((Some e,ts) :: cs) us

			| _ -> (None, ss) :: cs
		in List.rev << collect [] <| [s]

	let one_label_per_stmt = function
		| S ([],s) -> [S ([],s)]
		| S (ls,s) ->
			  List.map (fun l -> S ([l],Statement.Skip)) (List.init ls)
			  @ [S ([List.last ls],s)]
		| s -> [s]

	let rec one_target_per_goto s = match s with
		| S (ls,Goto (l::ids)) when List.length ids > 0 ->
			  [S (ls,Ite (
					  Expression.Choice,
					  [S ([],Goto [l])],
					  one_target_per_goto (S ([],Goto ids)) ))]
		| _ -> [s]

	let no_dead_stmts = function
		| S (ls,Dead _) -> [S (ls,Skip)]
		| s -> [s]

	let no_constrain_clauses = function
		| S (ls,Assign (lvs,es,Some e)) ->
			  [ S (ls,Assign (lvs,es,None)) ; S ([],Assume e) ]
		| s -> [s]

	open PrettyPrinting
	let print = function
		| S (ids,s) -> sep
			  ( List.map (fun l -> Identifier.print l <-> colon) ids
				@ [Statement.print s] )
		| C c -> oper "//" <+> text c
	let print_seq = vcat << punctuate semi << List.map print
	let to_string = render << print
end
module Ls = LabeledStatement

module Function = struct
	type t = (Identifier.t option * Type.t) list
			* Type.t
			* Expression.t option

	let signature (ps,t,_) = List.map snd ps, t
	let map_exprs fn (ps,t,e) = ps, t, Option.map (E.map fn) e
			
	open PrettyPrinting
	let print (ps,t,e) =
		( parens << sep << punctuate comma
		  << List.map (
			  uncurry (<+>) << Tup2.map
				  (option ((flip (<->)) colon << Identifier.print))
				  Type.print )
		  <| ps )
		<-> colon <+> Type.print t
			<+> option ((<+>) (oper "=") << Expression.print) e
end

module rec Procedure : sig
	type t = Declaration.t list
			* (Identifier.t option * Type.t) list
			* Expression.t list
			* Expression.t list
			* Declaration.t list
			* LabeledStatement.t list

	val resolve : t -> Identifier.t -> Type.t option
	val signature : t -> Type.t list * Type.t list
	val stmts : t -> LabeledStatement.t list
	val add_decls : Declaration.t list -> t -> t
	val add_params : Declaration.t list -> t -> t
	val add_rets : (Identifier.t option * Type.t) list -> t -> t
	val fold_stmts : ('a -> LabeledStatement.t -> 'a) -> 'a -> t -> 'a
	val map_stmts : (LabeledStatement.t -> LabeledStatement.t list) -> t -> t
	val map_exprs : (Expression.t -> Expression.t) -> t -> t
	val map_lvals : (Lvalue.t -> Lvalue.t) -> t -> t
			
	val to_string : Attribute.t list -> Identifier.t -> t -> string
	val print : Attribute.t list -> Identifier.t -> t -> PrettyPrinting.doc
end = struct
	type t = Declaration.t list
			* (Identifier.t option * Type.t) list
			* Expression.t list
			* Expression.t list
			* Declaration.t list
			* LabeledStatement.t list
			
	let resolve (ps,_,_,_,ds,_) x =
		match List.filter (
			((=) x << Declaration.name) 
			&&&& (((=) "var" |||| (=) "const") << Declaration.kind) )
		(ps@ds) with
		| (Declaration.Var (_,v,t) | Declaration.Const (_,v,t,_))::_ -> 
			Some t
		| _ -> None

	let signature (ps,ts,_,_,_,_) =
		List.map Declaration.typ ps, List.map snd ts
	let stmts (_,_,_,_,_,ss) = ss
	let add_decls ds' (ps,ts,rs,es,ds,ss) = ps,ts,rs,es,ds@ds',ss
	let add_params ps' (ps,ts,rs,es,ds,ss) = ps@ps',ts,rs,es,ds,ss
	let add_rets ts' (ps,ts,rs,es,ds,ss) = ps,ts@ts',rs,es,ds,ss

	let map_stmts fn (ps,ts,rs,es,ds,ss) =
		ps,ts,rs,es,ds, Ls.map_stmts fn ss

	let map_exprs fn (ps,ts,rs,es,ds,ss) =
		ps, ts, List.map (E.map fn) rs, List.map (E.map fn) es, ds,
		Ls.map_exprs fn ss

	let map_lvals fn (ps,ts,rs,es,ds,ss) =
		ps, ts, rs, es, ds, Ls.map_lvals fn ss

	let fold_stmts fn a (_,_,_,_,_,ss) = Ls.fold_left_rec fn a ss
		
	open PrettyPrinting
	let print ax n (ps,ts,rs,es,ds,ss) =
		let psig = 
			parens ( sep << punctuate comma
					 << List.map Declaration.print <| ps )
			<+> colon
			<+> ( match ts with
				  | [] -> Type.print Type.Void
				  | [None, t] -> Type.print t
				  | _ -> begin
						
						parens ( sep << punctuate comma
						 << List.map ( function
 									   | None, t -> Type.print t
									   | Some x, t ->
											 Identifier.print x
											 <-> colon
											 <+> Type.print t )
						 <| ts )
					end)

		and specs =
			List.map ((<+>) (keyword "requires") << Expression.print) rs
			@ List.map ((<+>) (keyword "ensures") << Expression.print) es

		and body = 
			Declaration.print_seq ds
				$-$ LabeledStatement.print_seq ss
		in

		match specs with
		| [] ->
			  keyword "proc"
			  <+> Attribute.print_seq ax
			  <+> Identifier.print n
			  <+> psig <+> keyword "begin"
				  $-$ (indent indent_size body)
				  $-$ keyword "end"
		| _ -> 
			  keyword "proc"
			  <+> Attribute.print_seq ax
			  <+> Identifier.print n
			  <+> psig
				  $-$ (indent indent_size (vcat specs))
				  $-$ keyword "begin"
				  $-$ (indent indent_size body)
				  $-$ keyword "end"

	let to_string ax n = render << print ax n
end

and Declaration : sig
	type t =
		| Type of Attribute.t list * Identifier.t * Identifier.t list
			  * Type.t option
		| Var of Attribute.t list * Identifier.t * Type.t
		| Const of Attribute.t list * Identifier.t * Type.t * Expression.t option
		| Func of Attribute.t list * Identifier.t * Function.t
		| Proc of Attribute.t list * Identifier.t * Procedure.t
		| Inv of Expression.t

	val has_attr : string -> t -> bool
	val attr :string -> t -> string list
	val name : t -> Identifier.t
	val typ : t -> Type.t
	val kind : t -> string
	val rename : (Identifier.t -> Identifier.t) -> t -> t
	val retype : (Type.t -> Type.t) -> t -> t
	val to_const : t -> t

	val map :
		(Identifier.t * Type.t -> Identifier.t * Type.t)
		-> (Identifier.t * Type.t * Expression.t option 
			-> Identifier.t * Type.t * Expression.t option)
		-> (Identifier.t * Function.t -> Identifier.t * Function.t)
		-> (Identifier.t * Procedure.t -> Identifier.t * Procedure.t)
		-> (Expression.t -> Expression.t)
		-> t -> t

	val map_exprs : (Expression.t -> Expression.t) -> t -> t
	val map_stmts : (LabeledStatement.t -> LabeledStatement.t list) -> t -> t
	val map_lvals : (Lvalue.t -> Lvalue.t) -> t -> t

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t =
		| Type of Attribute.t list * Identifier.t * Identifier.t list
			  * Type.t option
		| Var of Attribute.t list * Identifier.t * Type.t
		| Const of Attribute.t list * Identifier.t * Type.t * Expression.t option
		| Func of Attribute.t list * Identifier.t * Function.t
		| Proc of Attribute.t list * Identifier.t * Procedure.t
		| Inv of Expression.t

	let has_attr a = function
		| Type (ax,_,_,_) | Var (ax,_,_) | Const (ax,_,_,_)
		| Func (ax,_,_) | Proc (ax,_,_) -> List.mem_assoc a ax
		| _ -> false

	let attr a d = match d with
		| Type (ax,_,_,_) | Var (ax,_,_) | Const (ax,_,_,_)
		| Func (ax,_,_) | Proc (ax,_,_) when List.mem_assoc a ax ->
			  List.assoc a ax
		| _ -> []

	let kind = function
		| Type _ -> "type"
		| Var _ -> "var"
		| Const _ -> "const"
		| Func _ -> "func"
		| Proc _ -> "proc"
		| Inv _ -> "inv"
	let name = function
		| Type (_,id,_,_) | Var (_,id,_) | Const (_,id,_,_)
		| Func (_,id,_) | Proc (_,id,_) -> id
		| Inv _ -> ""
	let typ = function
		| Type (_,x,_,_) -> Type.T x
		| Var (_,_,t) | Const (_,_,t,_) -> t
		| Func (_,_,f) -> snd <| Function.signature f
		| Proc (_,_,p) -> Type.compress << snd <| Procedure.signature p
		| Inv _ -> Type.Bool
	let rename fn = function
		| Type (ax,n,tx,t) -> Type (ax, fn n, tx, t)
		| Var (ax,n,t) -> Var (ax, fn n, t) 
		| Const (ax,n,t,e) -> Const (ax, fn n, t, e)
		| Func (ax,n,f) -> Func (ax, fn n, f)
		| Proc (ax,n,p) -> Proc (ax, fn n, p)
		| d -> d
	let retype fn = function
		| Type (ax,n,tx,t) -> Type (ax, n, tx, Option.map fn t)
		| Var (ax,id,t) -> Var (ax, id, fn t) 
		| Const (ax,id,t,e) -> Const (ax, id, fn t, e)
		| d -> d
	let to_const = function
		| Var (ax,id,t) ->
			Const (ax,id,t,None)
		| d -> d

	let map_stmts fn d = match d with
		| Proc (ax,n,p) when not (has_attr "leavealone" d) ->
			  Proc (ax,n,Procedure.map_stmts fn p)
		| d -> d

	let map_exprs fn d = match d with
		| Func (ax,n,f) -> Func (ax, n, Function.map_exprs fn f)
		| Proc (ax,n,p) when not (has_attr "leavealone" d) ->
			  Proc (ax, n, Procedure.map_exprs fn p)
		| Inv e -> Inv (Expression.map fn e)
		| d -> d

	let map_lvals fn d = match d with
		| Proc (ax,n,p) when not (has_attr "leavealone" d) ->
			  Proc (ax, n, Procedure.map_lvals fn p)
		| d -> d

	let map fv fc ff fp fi d = match d with
		| Type (ax,x,tx,t) -> Type (ax,x,tx,t)
		| Var (ax,x,t) -> let x', t' = fv (x,t) in Var (ax,x',t')
		| Const (ax,x,t,e) -> 
			let x', t', e' = fc (x,t,e) in Const (ax,x',t',e')
		| Func (ax,n,f) -> let n', f' = ff (n,f) in Func (ax,n',f')
		| Proc (ax,n,p) when not (has_attr "leavealone" d) ->
			  let n', p' = fp (n,p) in Proc (ax,n',p')
		| Inv e -> Inv (fi e)
		| d -> d

	open PrettyPrinting
	let print d = match d with
		| Type (ax,x,tx,t) ->
			  keyword "type"
			  <+> Attribute.print_seq ax
			  <+> Identifier.print x
			  <+> sep (List.map Identifier.print tx)
			  <+> option ((<+>) (oper "=") << Type.print) t
		| Var (ax,x,t) ->
			  keyword (kind d)
			  <+> Attribute.print_seq ax
			  <+> Identifier.print x <-> colon
			  <+> Type.print t
		| Const (ax,x,t,e) ->	  
			  keyword (kind d)
			  <+> Attribute.print_seq ax
			  <+> Identifier.print x <-> colon
			  <+> Type.print t
			  <+> option (fun e -> oper "=" <+> Expression.print e) e
		| Func (ax,n,f) ->
			  keyword "func"
			  <+> Attribute.print_seq ax
			  <+> Identifier.print n <+> Function.print f
		| Proc (ax,n,p) -> empty $+$ Procedure.print ax n p
		| Inv e -> keyword "inv" <+> Expression.print e

	let rec print_seq ds =
		match ds with
		| [] -> empty
		| ((Var (ax,x,t) | Const (ax,x,t,None)) as d)::ds ->
			  let ds, dss =
				  List.take_while
					  (function (Var (_,_,t') | Const (_,_,t',None)) -> t'=t | _ -> false)
					  ds in
			  keyword (kind d)
			  <+> Attribute.print_seq ax
			  <+> (Identifier.print_seq << List.cons x <| List.map name ds)
			  <-> colon
			  <+> Type.print t
			  $-$ print_seq dss
		| d::ds -> print d $-$ print_seq ds

	let to_string = render << print
end

module Program = struct
	type t = Declaration.t list

	module D = Declaration
	module E = Expression
	module Lv = Lvalue
	module S = Statement
	module Ls = LabeledStatement

	let decls = id
	let procs = List.filter (fun d -> Declaration.kind d = "proc")
	
	let resolve pgm x =
		Option.seq ( 
			function (Declaration.Var (_,_,t) | Declaration.Const (_,_,t,_)) -> 
				Some t 
			| _ -> None	)
		<< List.first 
			( ((=) "const" |||| (=) "var" << Declaration.kind) 
				&&&& ((=) x << Declaration.name) )
		<| pgm

	let find_decl p n =
		List.first ( (=) n << Declaration.name ) p
		
	let find_proc p n =
		Option.map (function D.Proc (_,_,p) -> p | _ -> assert false)
		<< List.first ( ((=) "proc" << Declaration.kind)
					   &&&& 
					   ((=) n << Declaration.name) )
		<| p

	let is_global_var p n =
		Option.is_some
		<< List.first ( ((=) "var") << Declaration.kind
						&&&& ((=) n << Declaration.name) )
		<| p

	let is_global p n =
		Option.is_some
		<< List.first ( ( ((=) "var" |||| (=) "const") << Declaration.kind)
						&&&& ((=) n << Declaration.name) )
		<| p

	let add_decls = flip (@)
	let rem_decls ds = List.filter (not << (flip List.mem) ds << D.name)
	let map_decls fn = List.flatten << List.map fn
	let map_stmts fn = List.map (Declaration.map_stmts fn) 
	let map_exprs fn = List.map (Declaration.map_exprs fn)
	let map_lvals fn = List.map (Declaration.map_lvals fn)

(* 	let translate fd fe fl fs =
		map_stmts fs
		<< map_lvals fl
		<< map_exprs fe
		<< map_decls fd *)
		
	(* ToDo: make the suffix happen before return statements? *)
	let translate
		?(rem_global_decls = [])
		?(map_global_decls = List.unit)
		?(rename_global_decls = id)
		?(add_global_decls = [])
		?(add_proc_params = const [])
		?(add_local_decls = const [])
		?(add_proc_rets = const [])
		?(proc_body_prefix = const [])
		?(proc_body_suffix = const [])
		?(per_stmt_map = fun i -> List.unit)
		?(per_expr_map = fun i -> id) =

		add_decls add_global_decls
		<< map_decls (fun d ->
			match d with
			| D.Proc (ax,n,((ps,ts,rs,es,ds,ss) as p)) -> 
				let ps' = ps @ add_proc_params (n,p)
				and ts' = ts @ add_proc_rets (n,p)
				and ds' = ds @ add_local_decls (n,p)
				and ss' = proc_body_prefix (n,p) @ ss @ proc_body_suffix (n,p)
				in D.Proc (ax,n,(ps',ts',rs,es,ds',ss')) :: [] 
			| d -> d :: [])
		<< List.map (fun d -> Declaration.map_lvals (Lv.lift << per_expr_map <| D.name d) d)
		<< List.map (fun d -> Declaration.map_exprs (per_expr_map <| D.name d) d)
		<< List.map (fun d -> Declaration.map_stmts (per_stmt_map <| D.name d) d)
		<< map_decls map_global_decls
		<< map_decls (List.unit << Declaration.rename rename_global_decls)
		<< rem_decls rem_global_decls
		

	open PrettyPrinting
	let print p = Declaration.print_seq p $+$ empty
	let to_string = render << print
end

