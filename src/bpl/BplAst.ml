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
	
	val conj: t list -> t
	val disj: t list -> t
	
	val forall: (Identifier.t * Type.t) list -> t -> t
	val exists: (Identifier.t * Type.t) list -> t -> t
	
	val map : (t -> t) -> t -> t
	val fold : ('a -> t -> 'a) -> 'a -> t -> 'a
	val map_fold : ('a -> t -> 'a * t) -> 'a -> t -> 'a * t

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
	
	let is_term = function
		| Lit _ | Id _ | Old _ | FnApp _ 
		| Not _ | Neg _ | Sel _ | Upd _ -> true
		| _ -> false

	open BinaryOp
	
	let conj = function
		| [] -> bool true
		| e :: [] -> e
		| e :: es -> List.fold_left (fun e f -> Bin (And, e, f)) e es
	let disj = function
		| [] -> bool false
		| e :: [] -> e
		| e :: es -> List.fold_left (fun e f -> Bin (Or, e, f)) e es
		
	let forall xs e = Q (Forall, [], xs, [], [], e)
	let exists xs e = Q (Exists, [], xs, [], [], e)
	
	let rec map_fold fn a e = 
		uncurry fn <| match e with
		| Not e -> let a, e = map_fold fn a e in a, Not e
		| Neg e -> let a, e = map_fold fn a e in a, Neg e
		| Bin (op,e,f) -> 
			let a, e = map_fold fn a e in
			let a, f = map_fold fn a f in
			a, Bin (op,e,f)
		| Sel (e,es) -> 
			let a, e = map_fold fn a e in
			let a, es = List.map_fold_left (map_fold fn) a es in
			a, Sel (e, es)
		| Upd (e,es,f) -> 
			let a, e = map_fold fn a e in
			let a, es = List.map_fold_left (map_fold fn) a es in
			let a, f = map_fold fn a f in 
			a, Upd (e,es,f)
		| Old e -> let a, e = map_fold fn a e in a, Old e
		| FnApp (f,es) -> 
			let a, es = List.map_fold_left (map_fold fn) a es in
			a, FnApp (f,es)
		| Q (q,xs,tx,ax,ts,e) -> 
			let a, e = map_fold fn a e in
			a, Q (q,xs,tx,ax,ts,e)
		| _ -> a, e		
		
	let map fn = map_fold_to_map map_fold fn
	let fold fn = map_fold_to_fold map_fold fn
		
	open PrettyPrinting
	let rec print e = match e with
		| Lit c -> Literal.print c
		| Id x -> Identifier.print x
		| Old e -> keyword "old" <-> parens (print e)
		| FnApp (f,es) -> Identifier.print f <-> parens (print_seq es)
		| Not e -> oper "!" <-> print_auto_parens e
		| Neg e -> oper "-" <-> print_auto_parens e
		| Bin (op,e,e') -> 
			print_auto_parens e <+> BinaryOp.print op <+> print_auto_parens e'
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

	and print_auto_parens e = if is_term e then print e else parens (print e)

	and print_seq es = sep << punctuate comma << List.map print <| es
	let to_string = render << print
end

and Attribute : sig
	type t = Identifier.t * ((Expression.t, string) either) list
	val bool : string -> bool -> t
	val num : string -> int -> t
	val string : string -> string -> t

	val name : t -> string
	val add : t -> t list -> t list
	val has : string -> t list -> bool
	val strip : string -> t list -> t list
	val to_string : t -> string

	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t = Identifier.t * ((Expression.t, string) either) list
	let bool id b = id, [Left (Expression.bool b)]
	let num id n = id, [Left (Expression.num n)]
	let string id s = id, [Right s]

	let name = fst
	let has id = List.exists (fun (a,_) -> a = id)
	let add (id,vs) ax = if not (has id ax) then List.cons (id,vs) ax else ax
	let strip id = List.filter ((<>) id << fst)

	open PrettyPrinting
	let print (id,ax) =
		braces (
			colon <-> text id
		    <+> ( sep 
				  << List.map (Either.reduce
								   Expression.print
								   text)
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
	module E = Expression

	type t = Id of Identifier.t
			 | Sel of t * E.t list

	let rec name lv = match lv with Id x -> x | Sel (lv,_) -> name lv
	let ident x = Id x
	let rec from_expr = function
		| E.Id x -> Id x
		| E.Sel (e,es) -> Sel (from_expr e, es)
		| e -> failwith 
			<< sprintf "from_expr: unexpected expression `%s'."
			<| E.to_string e
	let rec to_expr = function
		| Id x -> E.Id x
		| Sel (x,es) -> E.Sel (to_expr x, es)
	
	let rec map_fold_exprs fn a = 
		function Sel (lv,es) -> 
			let a, lv = map_fold_exprs fn a lv in
			let a, es = List.map_fold_left (E.map_fold fn) a es in
			a, Sel (lv,es)
		| lv -> a, lv
		
	let map_exprs fn = map_fold_to_map map_fold_exprs fn

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
			 | Call of Attribute.t list * Identifier.t * Expression.t list * Identifier.t list
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

	val map_fold_exprs : ('a -> Expression.t -> 'a * Expression.t) -> 'a -> t -> 'a * t
	val map_exprs : (Expression.t -> Expression.t) -> t -> t

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end = struct
	type t = Assert of Expression.t
			 | Assume of Expression.t
			 | Havoc of Identifier.t list
			 | Assign of Lvalue.t list * Expression.t list
			 | Call of Attribute.t list * Identifier.t * Expression.t list * Identifier.t list
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
		
	module E = Expression
	module Lv = Lvalue
	module Ls = LabeledStatement
		
	let map_fold_exprs fn a = function
		| Assert e -> let a, e = E.map_fold fn a e in a, Assert e
		| Assume e -> let a, e = E.map_fold fn a e in a, Assume e
		| Assign (lvs,es) -> 
			let a, lvs = List.map_fold_left (Lv.map_fold_exprs fn) a lvs in
			let a, es = List.map_fold_left (E.map_fold fn) a es in
			a, Assign (lvs, es)
		| Call (ax,p,es,r) -> 
			let a, es = List.map_fold_left (E.map_fold fn) a es in
			a, Call (ax,p,es,r)
		| If (e,l,r) -> 
			let a, e = Option.map_fold (E.map_fold fn) a e in
			let a, l = List.map_fold_left (Ls.map_fold_exprs fn) a l in
			let a, r = List.map_fold_left (Ls.map_fold_exprs fn) a r in
			a, If (e,l,r)
		| While (e,ivs,ss) -> 
			let a, e = Option.map_fold (E.map_fold fn) a e in
			let a, ivs = List.map_fold_left 
				(Tup2.map_fold (E.map_fold fn) (fun a x -> a, x)) 
				a ivs in
			let a, ss = List.map_fold_left (Ls.map_fold_exprs fn) a ss in
			a, While (e,ivs,ss)
		| s -> a, s
		
	let map_exprs fn = map_fold_to_map map_fold_exprs fn

	open PrettyPrinting
	let rec print s = match s with
		| Assert e -> keyword "assert" <+> Expression.print e <-> semi
		| Assume e -> keyword "assume" <+> Expression.print e  <-> semi
		| Havoc xs -> keyword "havoc" <+> Identifier.print_seq xs <-> semi
		| Assign (xs,es) ->
			  Lvalue.print_seq xs
			  <+> oper ":="
			  <+> Expression.print_seq es <-> semi

		| Call (ax,p,es,rs) ->
			  let assign = match rs with
				  | [] -> empty
				  | _ -> Identifier.print_seq rs <+> oper ":="
			  in
			  keyword "call"
			  <+> Attribute.print_seq ax
			  <+> assign
			  <+> Identifier.print p
			  <+> parens (Expression.print_seq es)
			  <-> semi

		| If (_,_,[_, If _]) as s ->			
			
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
			in

			  let cases = as_cases ([],s) in

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
			  $-$ ( match ivs with [] -> empty 
					| _ -> indent indent_size (vcat <| List.map print_invariant ivs) )
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
	
	val stmt : Statement.t -> t

	val map_fold_stmts : ('a -> t -> 'a * t list) -> 'a -> t list -> 'a * t list
	val map_fold_exprs : ('a -> Expression.t -> 'a * Expression.t) -> 'a -> t -> 'a * t
	val map_exprs : (Expression.t -> Expression.t) -> t -> t
	val fold_stmts : ('a -> t -> 'a) -> 'a -> t list -> 'a
	
	val contains_rec : (t -> bool) -> t list -> bool
		
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t = Identifier.t list * Statement.t

	open Statement
	
	let stmt s = ([],s)

	let rec map_fold_stmts fn a ss =
		Tup2.map id List.flatten 
		<| List.map_fold_left (map_fold_stmt fn) a ss
		
	and map_fold_stmt fn a s =
		match s with
		| (ls,s) when is_atomic s -> fn a (ls,s)
		| (ls,If (e,tss,ess)) ->
			let a, tss = map_fold_stmts fn a tss in
			let a, ess = map_fold_stmts fn a ess in
		  	fn a (ls, If (e, tss, ess))
		| (ls,While (e,ivs,ss)) ->
			let a, ss = map_fold_stmts fn a ss in
		  	fn a (ls, While (e, ivs, ss))
		| _ -> assert false
		
	let fold_stmts fn a = fst << map_fold_stmts (fun a s -> fn a s, [s]) a
	
	let contains_rec p = fold_stmts (fun b s -> b or p s) false
						
	let map_fold_exprs fn a (ls,s) =
		let a, s = Statement.map_fold_exprs fn a s in
		a, (ls,s)
	let map_exprs fn = map_fold_to_map map_fold_exprs fn
		
	open PrettyPrinting
	let print (ids,s) =
		vcat (
			List.map (fun l -> Identifier.print l <-> colon) ids
			@ [Statement.print s] )
	let print_seq = vcat << List.map print
	let to_string = render << print
end


module Specification = struct
	type t = Requires of bool * Expression.t
			 | Modifies of bool * Identifier.t list
			 | Ensures of bool * Expression.t
			 | Posts of bool * Identifier.t list

	let map_fold_exprs fn a = 
		function
		| Requires (fr,e) -> 
			let a, e = Expression.map_fold fn a e in 
			a, Requires (fr,e)
		| Ensures (fr,e) -> 
			let a, e = Expression.map_fold fn a e in 
			a, Ensures (fr,e)
		| sp -> a, sp
	let map_exprs fn = map_fold_to_map map_fold_exprs fn

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
		| Posts (f,e) ->
			  ( if f then keyword "free" else empty )
			  <+> keyword "posts" <+> Identifier.print_seq e
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
	val map_fold_stmts : 
		('a -> LabeledStatement.t -> 'a * LabeledStatement.t list) -> 'a -> t -> 'a * t
	val map_exprs : (Expression.t -> Expression.t) -> t -> t
	val print : bool -> Attribute.t list -> Identifier.t -> t -> PrettyPrinting.doc
	val to_string : bool -> Attribute.t list -> Identifier.t -> t -> string
		
end = struct
	type t = Identifier.t list
			* (Identifier.t * Type.t) list
			* (Identifier.t * Type.t) list
			* Specification.t list
			* Declaration.t list
			* LabeledStatement.t list
			
	module Ls = LabeledStatement

	let signature (_,ps,rs,_,_,_) = List.map snd ps, List.map snd rs
	let stmts (_,_,_,_,_,ss) = ss

	let map_fold_stmts fn a (tx,ps,rs,sx,ds,ss) =
		let a, ss = LabeledStatement.map_fold_stmts fn a ss in
		a, (tx,ps,rs,sx,ds,ss)

	let map_exprs fn (tx,ps,rs,sx,ds,ss) =
		tx,ps,rs,
		List.map (Specification.map_exprs fn) sx,
		ds,
		List.map (Ls.map_exprs fn) ss

	open PrettyPrinting
	let print impl ats n (ts,ps,rs,es,ds,ss) =
		(if impl then keyword "implementation" else keyword "procedure") 
		<+> Attribute.print_seq ats
		<+> Identifier.print n
		<+> Type.print_type_args ts
		<+> parens (Type.print_typed_ids ps) 
		$-$ ( match rs with [] -> empty
			  | _ -> indent indent_size (
					keyword "returns" <+> parens (Type.print_typed_ids rs) ))
		<-> ( match ds,ss with [],[] -> semi | _ -> empty )
			$-$ indent indent_size (Specification.print_seq es)
			$-$ ( match ds,ss with [],[] -> empty
				  | _ -> lbrace
						$-$ ( match ds with [] -> empty 
							  | _ -> indent indent_size (Declaration.print_seq ds) )
						$-$ indent indent_size (LabeledStatement.print_seq ss)
						$-$ rbrace )
	let to_string impl ax n = render << print impl ax n
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
			  * Identifier.t 
			  * Procedure.t

	val name : t -> Identifier.t
	val rename : (Identifier.t -> Identifier.t) -> t -> t
	val kind : t -> string
	val to_const : t -> t
	
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
			  * Identifier.t 
			  * Procedure.t 

	let name = function
		| TypeCtor _ -> "type"
		| TypeSyn _ -> "type"
		| Axiom _ -> "*axiom*"
		| Const (_,_,n,_,_)
		| Func (_,n,_,_,_,_) 
		| Var (_,n,_,_)
		| Proc (_,n,_)
		| Impl (_,n,_) -> n	
		
	let rename fn = function
		| TypeCtor (ax,f,n,tx) -> TypeCtor (ax,f,fn n,tx)
		| TypeSyn (ax,n,tx,t) -> TypeSyn (ax,fn n,tx,t)
		| Const (ax,u,c,t,_) -> Const (ax,u,fn c,t,())
		| Func (ax,n,tx,ps,r,e) -> Func (ax,fn n,tx,ps,r,e)
		| Var (ax,n,t,e) -> Var (ax,fn n,t,e)
		| Proc (ax,n,p) -> Proc (ax,fn n,p)
		| Impl (ax,n,p) -> Impl (ax,fn n,p)
		| d -> d
		
	let kind = function
		| TypeCtor _ | TypeSyn _ -> "type"
		| Axiom _ -> "axiom"
		| Const _ -> "const"
		| Func _ -> "func"
		| Var _ -> "var"
		| Proc _ -> "proc"
		| Impl _ -> "impl"
		
		
	let to_const = function
		| Var (ax,n,t,e) ->
			Const (ax,false,n,t,())
		| d -> d

	open PrettyPrinting
			  
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

		| Proc (ax,n,p) -> Procedure.print false ax n p
		| Impl (ax,n,p) -> Procedure.print true ax n p

	let to_string = render << print
	let print_seq = vcat << List.map print
end

module Program = struct
	module D = Declaration
	module Ls = LabeledStatement
	module S = Specification

	type t = D.t list
	
	let decls = id

	let map_fold fn a ds = List.map_fold_left fn a ds
	let map_fold_procs fn a =
		map_fold ( fun a d ->
			match d with
			| D.Proc (ax,n,p) -> 
				let a, p = fn a (n,p) in
				a, D.Proc (ax,n,p)
		  	| d -> a, d ) a
	let map_fold_stmts fn a = 
		map_fold_procs (fun a (n,p) -> Procedure.map_fold_stmts (fn n) a p) a
		
	let map fn = map_fold_to_map map_fold fn
	let fold fn a = map_fold_to_fold map_fold fn a
	(* let map_procs fn = map_fold_to_map map_fold_procs fn *)
	(* let map_stmts fn = map_fold_to_map map_fold_stmts fn *)

	let map_procs fn = snd << map_fold_procs (fun _ (_,p) -> (), fn p) ()
	let map_stmts : (Identifier.t -> Ls.t -> Ls.t list) -> t -> t = fun fn ->
		snd << map_fold_stmts (fun n _ s -> (), fn n s) ()
	
	let fold_stmts fn a = fst << map_fold_stmts (fun _ a s -> fn a s, [s]) a


	let map_exprs fn = 
		List.map (
			function D.Axiom (ax,e) -> D.Axiom (ax, Expression.map (fn "") e)
			| D.Func (ax,f,tx,ps,r,e) -> 
				D.Func (ax,f,tx,ps,r, Option.map (Expression.map (fn "")) e)
			| D.Var (ax,n,t,e) -> 
				D.Var (ax,n,t,Option.map (Expression.map (fn n)) e)
			| D.Proc (ax,n,p) -> D.Proc (ax,n,Procedure.map_exprs (fn n) p)
			| D.Impl (ax,n,p) -> D.Impl (ax,n,Procedure.map_exprs (fn n) p)
			| d -> d
		) 

	let fold_procs fn =
		List.fold_left
			(fun a d ->
				 match d with
				 | D.Proc (ax,n,p) -> fn a p
				 | _ -> a)
			
	let find p n = List.first ((=) n << D.name) p
	let find_proc p n =
		Option.seq (function D.Proc (_,_,p) -> Some p | _ -> None)
		<| List.first ((=) n << D.name 
			&&&& (function D.Proc _ -> true | _ -> false)) p
		
	let translate
		?(replace_global_decls = List.unit)
		?(new_global_decls = [])
		?(new_proc_params = const [])
		?(new_proc_rets = const [])
		?(new_local_decls = const [])
		?(proc_body_prefix = const [])
		?(proc_body_suffix = const [])
		?(per_stmt_map = const List.unit)
		?(per_expr_map = const id) =
		
		List.append new_global_decls
		<< List.flatten << map (
			function D.Proc (ax,n,((ts,ps,rs,es,ds,ss) as p)) ->
				let ps' = ps @ new_proc_params (n,p)
				and rs' = rs @ new_proc_rets (n,p)
				and ds' = ds @ new_local_decls (n,p)
				and ss' = proc_body_prefix (n,p) @ ss @ proc_body_suffix (n,p)
				in D.Proc (ax,n,(ts,ps',rs',es,ds',ss')) :: []
			| d -> d :: [] )
		<< List.flatten << map replace_global_decls
		<< map_stmts (per_stmt_map)
		<< map_exprs (per_expr_map)	
		
	
		(* ?(rem_global_decls = []) *)
		(* ?(map_global_decls = List.unit) *)
		(* ?(rename_global_decls = id) *)
		(* ?(add_global_decls = []) *)
		(* ?(add_proc_params = const []) *)
		(* ?(add_local_decls = const []) *)
		(* ?(add_proc_rets = const []) *)
		(* ?(proc_body_prefix = const []) *)
		(* ?(proc_body_suffix = const []) *)
		(* ?(per_stmt_map = fun i -> List.unit) *)
		(* ?(per_expr_map = fun i -> id) = *)

		(* add_decls add_global_decls
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
		<< rem_decls rem_global_decls	 *)

	open PrettyPrinting
	let print p = (vcat << List.map D.print <| p) $+$ empty
	let to_string = render << print
end

