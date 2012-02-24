(** Abstract syntax tree for Boogie programs. *)

open Prelude
open Printf

let indent_size = 3

module Identifier = struct
    type t = string

	let rename fn = fn

	open PrettyPrinting
	let print = text
	let print_seq = sep << punctuate comma << List.map print
	let print_decls = function
		| [] -> empty
		| ds -> keyword "decl" <+> print_seq ds <-> semi
	let to_string = id
end

module Type = struct
	type t = Bool | Int | Bv of int
			 | T of Identifier.t * t list
			 | Map of Identifier.t list * t list * t

	open PrettyPrinting
	let print_type_args = List.reduce (angles << Identifier.print_seq ) empty
	let rec print t = match t with
		| Bool -> keyword "bool"
		| Int -> keyword "int"
		| Bv n -> keyword "bv" <-> int n
		| T (x,tx) ->
			  Identifier.print x
			  <+> sep (List.map print tx)
		| Map (ax,ts,t) ->
			  print_type_args ax <+> brackets (print_seq ts) <+> print t
	and print_seq ts = sep << punctuate comma << List.map print <| ts
	let print_typed_ids =
		sep << punctuate comma
		<< List.map
			(fun (x,t) -> Identifier.print x <-> colon <+> print t)
	let to_string = render << print
end

module Literal = struct
	type t = True
			 | False
			 | Num of int
			 | Bv of int

	open PrettyPrinting
	let print = function
		| True -> keyword "true"
		| False -> keyword "false"
		| Num n -> int n
		| Bv n -> failwith "Literal.print: Bv"
	let to_string = render << print
end

module BinaryOp = struct
	type t = Iff | Imp | Or | And
			 | Eq | Neq | Lt | Gt | Lte | Gte | Sub
			 | Conc | Plus | Minus | Times | Div | Mod

	open PrettyPrinting
	let to_string = function
		| Iff -> "<==>"
		| Imp -> "==>"
		| Or -> "||"
		| And -> "&&"
		| Eq -> "=="
		| Neq -> "!="
		| Lt -> "<"
		| Gt -> ">"
		| Lte -> "<="
		| Gte -> ">="
		| Sub -> "<:"
		| Conc -> "++"
		| Plus -> "+"
		| Minus -> "-"
		| Times -> "*"
		| Div -> "/"
		| Mod -> "%"
	let print = oper << to_string
end	

module rec Expression : sig
	type q = Forall | Exists
	type t = Lit of Literal.t
			 | Id of Identifier.t	
			 | Old of t
			 | FnApp of Identifier.t * t list
			 | Not of t
			 | Neg of t
			 | Bin of BinaryOp.t * t * t
			 | Sel of t * t list
			 | Upd of t * t list * t
			 | Q of q
				   * Identifier.t list
				   * (Identifier.t * Type.t) list
				   * Attribute.t list
				   * Trigger.t list
				   * t

	val ident : Identifier.t -> t
	val bool : bool -> t
	val num : int -> t

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type q = Forall | Exists
	type t = Lit of Literal.t
			 | Id of Identifier.t	
			 | Old of t
			 | FnApp of Identifier.t * t list
			 | Not of t
			 | Neg of t
			 | Bin of BinaryOp.t * t * t
			 | Sel of t * t list
			 | Upd of t * t list * t
			 | Q of q
				   * Identifier.t list
				   * (Identifier.t * Type.t) list
				   * Attribute.t list
				   * Trigger.t list
				   * t

 	let ident s = Id s
	let bool b = Lit (if b then Literal.True else Literal.False)
	let num n = Lit (Literal.Num n)

	open PrettyPrinting
	let rec print e = match e with
		| Lit c -> Literal.print c
		| Id x -> Identifier.print x
		| Old e -> keyword "old" <-> parens (print e)
		| FnApp (f,es) -> Identifier.print f <-> parens (print_seq es)
		| Not e -> parens (oper "!" <-> print e)
		| Neg e -> oper "-" <-> (parens <| print e)
		| Bin (op,e,e') ->
			  parens ( print e <+> BinaryOp.print op <+> print e' )
		| Sel (e,es) -> print e <-> brackets (print_seq es)
		| Upd (e,es,f) ->
			  print e
			  <-> brackets ( print_seq es <+> oper ":=" <+> print f )
		| Q (q,tx,xs,ax,ts,e) ->
			  parens (
				  keyword (match q with Forall -> "forall" | _ -> "exists")
				  <+> Type.print_type_args tx
				  <+> Type.print_typed_ids xs
				  <+> oper "::"
				  <+> Attribute.print_seq ax
				  <+> Trigger.print_seq ts
				  <+> print e )

	and print_seq es = sep << punctuate comma << List.map print <| es
	let to_string = render << print
end

and Attribute : sig
	type t = Identifier.t * ((Expression.t, string) either) list
	val to_string : t -> string
	val bool : string -> bool -> t
	val num : string -> int -> t
	val string : string -> string -> t
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t = Identifier.t * ((Expression.t, string) either) list
	let bool id b = id, [Left (Expression.bool b)]
	let num id n = id, [Left (Expression.num n)]
	let string id s = id, [Right s]
	open PrettyPrinting
	let print (id,ax) =
		braces (
			colon <-> text id
		    <+> ( sep << punctuate comma
				  << List.map (Either.reduce
								   Expression.print
								   (double_quotes << text))
				  <|ax ) )
	let print_seq = sep << List.map print
	let to_string = render << print
end

and Trigger : sig
	type t = Expression.t list

	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
	val to_string : t -> string
end = struct
	type t = Expression.t list

	open PrettyPrinting
	let print =
		braces << sep << punctuate comma << List.map Expression.print
	let print_seq = sep << List.map print
	let to_string = render << print
end


module Lvalue = struct
	type t = Id of Identifier.t
			 | Sel of t * Expression.t list

	let rec name lv = match lv with Id x -> x | Sel (lv,_) -> name lv
	let ident x = Id x

	open PrettyPrinting
	let rec print lv = match lv with
		| Id x -> Identifier.print x
		| Sel (x,es) ->
			  print x <-> brackets (Expression.print_seq es)
	let print_seq = sep << punctuate comma << List.map print
	let to_string = render << print
end
	
module rec Statement : sig
	type t = Assert of Expression.t
			 | Assume of Expression.t
			 | Havoc of Identifier.t list
			 | Assign of Lvalue.t list * Expression.t list
			 | Call of Identifier.t * Expression.t list * Identifier.t list
			 | If of Expression.t option
				   * LabeledStatement.t list
				   * LabeledStatement.t list
			 | While of Expression.t option
				   * (Expression.t * bool) list
				   * LabeledStatement.t list
			 | Break of Identifier.t option
			 | Return
			 | Goto of Identifier.t list

	val is_atomic : t -> bool
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end = struct
	type t = Assert of Expression.t
			 | Assume of Expression.t
			 | Havoc of Identifier.t list
			 | Assign of Lvalue.t list * Expression.t list
			 | Call of Identifier.t * Expression.t list * Identifier.t list
			 | If of Expression.t option
				   * LabeledStatement.t list
				   * LabeledStatement.t list
			 | While of Expression.t option
				   * (Expression.t * bool) list
				   * LabeledStatement.t list
			 | Break of Identifier.t option
			 | Return
			 | Goto of Identifier.t list

	let is_atomic = function
		| Assert _ | Assume _ | Havoc _ | Assign _
		| Call _ | Break _ | Return | Goto _ -> true
		| _ -> false

	open PrettyPrinting
	let rec print s = match s with
		| Assert e -> keyword "assert" <+> Expression.print e <-> semi
		| Assume e -> keyword "assume" <+> Expression.print e  <-> semi
		| Havoc xs -> keyword "havoc" <+> Identifier.print_seq xs <-> semi
		| Assign (xs,es) ->
			  Lvalue.print_seq xs
			  <+> oper ":="
			  <+> Expression.print_seq es <-> semi

		| Call (p,es,rs) ->
			  let assign = match rs with
				  | [] -> empty
				  | _ -> Identifier.print_seq rs <+> oper ":="
			  in
			  keyword "call"
			  <+> assign
			  <+> Identifier.print p
			  <+> parens (Expression.print_seq es)
			  <-> semi

		| If (_,_,[_, If _]) as s ->

			  let cases = LabeledStatement.as_cases ([],s) in

			  vcat
			  << List.mapi
				  (fun i (e,ss) ->
					   ( if i = 0 then begin
							 keyword "if"
							 <+> parens (Option.reduce Expression.print
											 (oper "*") e)
						 end
						 else if i = List.length cases - 1 then begin
							 keyword "else"
						 end
						 else begin
							 keyword "else" <+> keyword "if"
							 <+> parens (Option.reduce Expression.print
											 (oper "*") e)
						 end )
					   <+> lbrace
					   $-$ indent indent_size (LabeledStatement.print_seq ss)
					   $-$ rbrace)
			  <| cases

		| If (e,ts,us) ->
			  keyword "if"
			  <+> parens (Option.reduce Expression.print (oper "*") e)
			  <+> lbrace
			  $-$ indent indent_size (LabeledStatement.print_seq ts)
			  $-$ rbrace
			  <+> ( match us with
					| [] -> empty
					| _ -> keyword "else"
						  <+> lbrace
						  $-$ indent indent_size
								(LabeledStatement.print_seq us)
						  $-$ rbrace )


		| While (e,ivs,ss) ->
			  let print_invariant (e,f) =
				  (if f then keyword "free" else empty)
				  <+> keyword "invariant"
				  <+> Expression.print e
				  <-> semi
			  in
			  keyword "while"
			  <+> parens (Option.reduce Expression.print (oper "*") e)
			  $-$ indent indent_size (vcat <| List.map print_invariant ivs)
			  $-$ lbrace
			  $-$ indent indent_size (LabeledStatement.print_seq ss)
			  $-$ rbrace

		| Break id ->
			  keyword "break"
			  <+> Option.reduce Identifier.print empty id
			  <-> semi

		| Return -> keyword "return" <-> semi
		| Goto ids -> keyword "goto" <+> Identifier.print_seq ids <-> semi

	let to_string = render << print
end
	
and LabeledStatement : sig
	type t = Identifier.t list * Statement.t

	val as_cases : t -> (Expression.t option * t list) list
	val map_stmts : (t -> t list) -> t list -> t list
	val modifies : t list -> Identifier.t list
	val called : t list -> Identifier.t list
	val calls : t list ->
		(Identifier.t * Expression.t list * Identifier.t list) list
		
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t = Identifier.t list * Statement.t

	open Statement

	let as_cases s =
		let rec collect cs ss =
			match ss with
			| [ls, If (e,ts,us)] ->
				  if ls <> [] then
					  Printf.eprintf
						  ( "Warning: Losing labels %s in " 
							^^ "BplAst.LabeledStatement.as_cases.\n" )
						  (String.concat ", " ls);
				  collect ((e,ts) :: cs) us

			| _ -> (None, ss) :: cs
		in List.rev << collect [] <| [s]

	let rec fold_left_rec fn =
		List.fold_left
			( fun a s ->
				  ( match s with
					| (ls, If (e,tss,ess)) ->
						  (flip <| fold_left_rec fn) ess
						  << (flip <| fold_left_rec fn) tss
					| (ls, While (e,_,ss)) ->
						  (flip <| fold_left_rec fn) ss
					| _ -> id )
				  <| fn a s )

	let modifies =
		fold_left_rec
			( fun ms s -> match s with
			  | [], Assign (xs,_) -> List.union (List.map Lvalue.name xs) ms
			  | [], Call (_,_,xs) -> List.union xs ms
			  | _ -> ms )
			[]

	let calls =
		fold_left_rec
			(fun ps s ->
				 match s with
				 | (_, Call (pn,xs,ys)) ->
					   List.union [pn,xs,ys] ps
				 | _ -> ps)
			[]
		
	module StringSet = Set.Make(
		struct
			type t = string
			let compare = compare
		end)
		
	let called =
		StringSet.uniqify_list
		<< List.map (Tup3.fst)
		<< calls

	let rec map_stmts fn ss =
		List.flatten <| List.map (map_stmt fn) ss

	and map_stmt fn s =
		match s with
		| (ls,s) when is_atomic s -> fn (ls,s)
		| (ls,If (e,tss,ess)) ->
			  fn (ls, If (e, map_stmts fn tss, map_stmts fn ess))
		| (ls,While (e,ivs,ss)) ->
			  fn (ls, While (e, ivs, map_stmts fn ss))
		| _ -> assert false

	open PrettyPrinting
	let print (ids,s) =
		sep	(
			List.map (fun l -> Identifier.print l <-> colon) ids
			@ [Statement.print s] )
	let print_seq = vcat << List.map print
	let to_string = render << print
end


module Specification = struct
	type t = Requires of bool * Expression.t
			 | Modifies of bool * Identifier.t list
			 | Ensures of bool * Expression.t

	open PrettyPrinting
	let print = function
		| Requires (f,e) ->
			  ( if f then keyword "free" else empty )
			  <+> keyword "requires" <+> Expression.print e
			  <-> semi
		| Modifies (f,e) ->
			  ( if f then keyword "free" else empty )
			  <+> keyword "modifies" <+> Identifier.print_seq e
			  <-> semi
		| Ensures (f,e) ->
			  ( if f then keyword "free" else empty )
			  <+> keyword "ensures" <+> Expression.print e
			  <-> semi
	let print_seq = vcat << List.map print
	let to_string = render << print
end

module rec Procedure : sig
	type t =  Identifier.t list
			* (Identifier.t * Type.t) list
			* (Identifier.t * Type.t) list
			* Specification.t list
			* Declaration.t list
			* LabeledStatement.t list

	val signature : t -> Type.t list * Type.t list
	val stmts : t -> LabeledStatement.t list
	val map_stmts : (LabeledStatement.t -> LabeledStatement.t list) -> t -> t
	val to_string : Attribute.t list -> Identifier.t -> t -> string
	val print : Attribute.t list -> Identifier.t -> t -> PrettyPrinting.doc
		
end = struct
	type t = Identifier.t list
			* (Identifier.t * Type.t) list
			* (Identifier.t * Type.t) list
			* Specification.t list
			* Declaration.t list
			* LabeledStatement.t list

	let signature (_,ps,rs,_,_,_) = List.map snd ps, List.map snd rs
	let stmts (_,_,_,_,_,ss) = ss
	let map_stmts fn (tx,ps,rs,sx,ds,ss) =
		tx,ps,rs,sx,ds, LabeledStatement.map_stmts fn ss

	open PrettyPrinting
	let print ats n (ts,ps,rs,es,ds,ss) =
		keyword "procedure"
		<+> Attribute.print_seq ats
		<+> Identifier.print n
		<+> Type.print_type_args ts
		<+> parens (Type.print_typed_ids ps) 
		$-$ ( match rs with [] -> empty
			  | _ -> indent indent_size (
					keyword "returns" <+> parens (Type.print_typed_ids rs) ))
		<-> ( match ds,ss with [],[] -> semi | _ -> empty )
			$+$ indent indent_size (Specification.print_seq es)
			$-$ ( match ds,ss with [],[] -> empty
				  | _ -> lbrace
						$-$ indent indent_size (Declaration.print_seq ds)
						$-$ indent indent_size (LabeledStatement.print_seq ss)
						$-$ rbrace )
	let to_string ax n = render << print ax n
end
	
and Declaration : sig
	type t = 
		| TypeCtor of Attribute.t list
			  * bool
			  * Identifier.t
			  * Identifier.t list
		| TypeSyn of Attribute.t list
			  * Identifier.t
			  * Identifier.t list
			  * Type.t
		| Const of Attribute.t list * bool
			  * Identifier.t * Type.t * unit (* OrderSpec *)
		| Func of Attribute.t list
			  * Identifier.t * Identifier.t list 
			  * (Identifier.t option * Type.t) list
			  * (Identifier.t option * Type.t)
			  * Expression.t option
		| Axiom of Attribute.t list * Expression.t
		| Var of Attribute.t list
			  * Identifier.t * Type.t
			  * Expression.t option
		| Proc of Attribute.t list
			  * Identifier.t
			  * Procedure.t
		| Impl of Attribute.t list
			  * Identifier.t * Identifier.t list
			  * (Identifier.t * Type.t) list
			  * (Identifier.t * Type.t) list
			  * t list * LabeledStatement.t list

	val name : t -> Identifier.t
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t =
		| TypeCtor of Attribute.t list
			  * bool
			  * Identifier.t
			  * Identifier.t list
		| TypeSyn of Attribute.t list
			  * Identifier.t
			  * Identifier.t list
			  * Type.t
		| Const of Attribute.t list * bool
			  * Identifier.t * Type.t * unit (* OrderSpec *)
		| Func of Attribute.t list
			  * Identifier.t * Identifier.t list 
			  * (Identifier.t option * Type.t) list
			  * (Identifier.t option * Type.t)
			  * Expression.t option
		| Axiom of Attribute.t list * Expression.t
		| Var of Attribute.t list
			  * Identifier.t * Type.t
			  * Expression.t option
		| Proc of Attribute.t list
			  * Identifier.t
			  * Procedure.t
		| Impl of Attribute.t list
			  * Identifier.t * Identifier.t list
			  * (Identifier.t * Type.t) list
			  * (Identifier.t * Type.t) list
			  * t list * LabeledStatement.t list

	open PrettyPrinting

	let name = function
		| TypeCtor _ -> "type"
		| TypeSyn _ -> "type"
		| Axiom _ -> "*axiom*"
		| Const (_,_,n,_,_)
		| Func (_,n,_,_,_,_) 
		| Var (_,n,_,_)
		| Proc (_,n,_)
		| Impl (_,n,_,_,_,_,_) -> n
			  
	let print_function_arg = function
		| None, t -> Type.print t
		| Some x, t -> Identifier.print x <-> colon <+> Type.print t
			
	let print = function
		| TypeCtor (ax,f,n,tx) ->
			  keyword "type"
			  <+> Attribute.print_seq ax
			  <+> (if f then keyword "finite" else empty)
			  <+> Identifier.print n
			  <+> sep (List.map Identifier.print tx)
			  <-> semi

		| TypeSyn (ax,n,tx,t) ->
			  keyword "type"
			  <+> Attribute.print_seq ax
			  <+> Identifier.print n
			  <+> sep (List.map Identifier.print tx)
			  <+> oper "="
			  <+> Type.print t
			  <-> semi

		| Const (ats,u,c,t,_) ->
			  keyword "const"
			  <+> Attribute.print_seq ats
			  <+> ( if u then text "unique" else empty )
			  <+> Identifier.print c <-> colon
			  <+> Type.print t
			  <-> semi

		| Func (ax,f,tx,ps,r,e) ->
			  let fsig = keyword "function"
				  <+> Attribute.print_seq ax
				  <+> Identifier.print f
				  <+> Type.print_type_args tx
				  <+> ( parens << sep << punctuate comma
						<< List.map print_function_arg <| ps )
				  <+> keyword "returns"
				  <+> parens (print_function_arg r)
			  in begin match e with
			  | None -> fsig <-> semi
			  | Some e -> fsig $+$ (indent indent_size <<  braces)
					(Expression.print e)
			  end

		| Axiom (ax,e) -> 
			  keyword "axiom"
			  <+> Attribute.print_seq ax
			  <+> Expression.print e
			  <-> semi

		| Var (ats,x,t,e) ->
			  keyword "var"
			  <+> Attribute.print_seq ats
			  <+> Identifier.print x <-> colon
			  <+> Type.print t
			  <+> Option.reduce
					((<+>) (keyword "where") << Expression.print)
					empty e
			  <-> semi

		| Proc (ax,n,p) -> Procedure.print ax n p

		| Impl (ats,n,ts,ps,rs,ds,ss) ->
			  keyword "implementation"
			  <+> Attribute.print_seq ats
			  <+> Identifier.print n
			  <+> Type.print_type_args ts
			  <+> parens (Type.print_typed_ids ps)
			  $-$ ( match rs with [] -> empty
					| _ -> indent indent_size (
						  keyword "returns"
						  <+> parens (Type.print_typed_ids rs) ))
			  $-$ lbrace
			  $-$ indent indent_size (Declaration.print_seq ds)
			  $-$ indent indent_size (LabeledStatement.print_seq ss)
			  $-$ rbrace

	let to_string = render << print
	let print_seq = vcat << List.map print
end

module Program = struct
	module D = Declaration
	module Ls = LabeledStatement
	module S = Specification

	type t = D.t list

	let map fn ds = List.map fn ds
	let map_procs fn =
		map ( function D.Proc (ax,n,p) -> D.Proc (ax,n,fn p)
			  | d -> d )
	let map_stmts fn = map_procs (Procedure.map_stmts fn)

	let fold_procs fn =
		List.fold_left
			(fun a d ->
				 match d with
				 | D.Proc (ax,n,p) -> fn a p
				 | _ -> a)
			
	let find p n = List.first ((=) n << D.name) p
	let find_proc p n =
		Option.seq (function D.Proc (_,_,p) -> Some p | _ -> None)
		<| List.first ((=) n << D.name) p

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

	open PrettyPrinting
	let print p = (vcat << List.map D.print <| p) $+$ empty
	let to_string = render << print
end

