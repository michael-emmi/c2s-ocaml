(** Abstract syntax tree for Boolean programs. *)

open Prelude

let indent_size = 3

module Identifier = struct
    type t = string

	open PrettyPrinting
	let print = text
	let print_seq = sep << punctuate comma << List.map print
	let print_decls = function
		| [] -> empty
		| ds -> keyword "decl" <+> print_seq ds <-> semi
	let to_string = id
end

module Type = struct
	type t = Void | Bool of int

	open PrettyPrinting
	let print = function
		| Void -> keyword "void"
		| Bool n -> keyword "bool" <-> if n = 1 then empty else angles (int n)
	let to_string = render << print
end

module Constant = struct
	type t = True | False

	open PrettyPrinting
	let to_string = function True -> "T" | False -> "F"
	let print = text << to_string
end

module BinaryOp = struct
	type t = Or | And | Eq | Neq | Imp

	open PrettyPrinting
	let to_string = function
		| Or -> "|"
		| And -> "&"
		| Eq -> "="
		| Neq -> "!="
		| Imp -> "=>"
	let print = oper << to_string
end	

module Expression = struct
	type t =
		Id of Identifier.t	
		| Const of Constant.t
		| Not of t
		| Bin of BinaryOp.t * t * t
		| Schoose of t list

	let bool b = Const (if b then Constant.True else Constant.False)

	open PrettyPrinting
	let rec print e = match e with
		| Id x -> Identifier.print x
		| Const c -> Constant.print c
		| Not e -> parens (oper "!" <-> print e)
		| Bin (op,e,e') ->
			  parens ( print e <+> BinaryOp.print op <+> print e' )
		| Schoose es ->
			  keyword "schoose" <+> brackets (print_seq es)
	and print_seq es = sep << punctuate comma << List.map print <| es
	let print_decider = Option.reduce print (oper "*")
	let to_string = render << print
end

module rec Statement : sig
	type t =
		Skip
		| Goto of Identifier.t list
		| Return of Expression.t list
		| Assign of Identifier.t list
			  * Expression.t list
			  * Expression.t option
		| Ite of Expression.t option
			  * LabeledStatement.t list
			  * LabeledStatement.t list
		| While of Expression.t option
			  * LabeledStatement.t list
		| Assert of Expression.t option
		| Assume of Expression.t
		| Call of Identifier.t
			  * Expression.t list
			  * Identifier.t list
		| Dead of Identifier.t list

	val is_atomic : t -> bool

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end = struct
	type t =
		Skip
		| Goto of Identifier.t list
		| Return of Expression.t list
		| Assign of Identifier.t list
			  * Expression.t list
			  * Expression.t option
		| Ite of Expression.t option
			  * LabeledStatement.t list
			  * LabeledStatement.t list
		| While of Expression.t option
			  * LabeledStatement.t list
		| Assert of Expression.t option
		| Assume of Expression.t
		| Call of Identifier.t
			  * Expression.t list
			  * Identifier.t list
		| Dead of Identifier.t list

	let is_atomic = function
		| Skip | Goto _ | Return _ | Assign _
		| Assert _ | Assume _ | Call _ | Dead _ -> true
		| _ -> false

	open PrettyPrinting
	let rec print s = match s with
		| Skip -> keyword "skip" <-> semi
		| Goto ids ->
			  keyword "goto"
			  <+> (sep << punctuate comma <| List.map Identifier.print ids)
			  <-> semi
		| Return es ->
			  keyword "return"
			  <+> (sep << punctuate comma <| List.map Expression.print es)
			  <-> semi
		| Assign (ids,es,ce) ->
			  let constrain = match ce with
				  | None -> empty
				  | Some e ->
						keyword "constrain"
						<+> parens (Expression.print e)
			  in
			  (sep << punctuate comma <| List.map Identifier.print ids)
			  <+> oper ":="
			  <+> (sep << punctuate comma <| List.map Expression.print es)
			  <+> constrain
			  <-> semi
		| Ite (e,ss,ss') ->
			  keyword "if"
			  <+> parens (Expression.print_decider e)
			  <+> keyword "then"
			  $-$ indent indent_size (LabeledStatement.print_seq ss)
			  $-$ keyword "else"
			  $-$ indent indent_size (LabeledStatement.print_seq ss')
			  $-$ keyword "fi"
		| While (e,ss) ->
			  keyword "while"
			  <+> parens (Expression.print_decider e)
			  <+> keyword "do"
			  $-$ indent indent_size (LabeledStatement.print_seq ss)
			  $-$ keyword "od"
		| Assert e ->
			  keyword "assert"
			  <+> parens (Expression.print_decider e)
			  <-> semi
		| Assume e ->
			  keyword "assume"
			  <+> ( parens <| Expression.print e )
			  <-> semi
		| Call (p,es,rs) ->
			  let assign = match rs with
				  | [] -> empty
				  | _ -> Identifier.print_seq rs <+> oper ":="
			  in
			  assign
			  <+> Identifier.print p
			  <+> parens (Expression.print_seq es)
			  <-> semi
		| Dead ds -> keyword "dead" <+> Identifier.print_seq ds <-> semi
				  
	let to_string = render << print
end
	
and LabeledStatement : sig
	type t = Identifier.t list * Statement.t

	val map_stmts : (t -> t list) -> t list -> t list
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end = struct
	type t = Identifier.t list * Statement.t

	open Statement
	let rec map_stmts fn ss =
		List.flatten <| List.map (map_stmt fn) ss
	and map_stmt fn s = 
		match s with
		| (ls,s) when is_atomic s -> fn (ls,s)
		| (ls,Ite (e,tss,ess)) ->
			  fn (ls, Ite (e, map_stmts fn tss, map_stmts fn ess))
		| (ls,While (e,ss)) ->
			  fn (ls, While (e, map_stmts fn ss))
		| _ -> assert false

	open PrettyPrinting
	let print (ids,s) =
		sep	(
			List.map (fun l -> Identifier.print l <-> colon) ids
			@ [Statement.print s] )
	let print_seq = vcat << List.map print
	let to_string = render << print
end

module Procedure = struct
	type t = Type.t                    (* return type *)
			* Identifier.t             (* name *)
			* Identifier.t list        (* parameters *)
			* Identifier.t list        (* declarations *)
			* Expression.t option      (* enforce *)
			* LabeledStatement.t list  (* statements *)

	let signature (t,_,ps,_,_,_) =
		List.length ps,
		match t with
		| Type.Void -> 0
		| Type.Bool i -> i
		
	open PrettyPrinting
	let print (typ,name,args,decls,enf,stmts) =
		let enforce = match enf with
			| None -> empty
			| Some e ->
				  keyword "enforce"
				  <+> Expression.print e
				  <-> semi
		in
		Type.print typ 
		<+> Identifier.print name
		<+> ( parens << sep << punctuate comma
			  <| List.map Identifier.print args )
		<+> keyword "begin"
		$-$ indent indent_size (
			Identifier.print_decls decls
			$-$ enforce 
			$-$ LabeledStatement.print_seq stmts
		)
		$-$ keyword "end"
		$+$ empty

	let to_string = render << print
end	

module Program = struct
	type t = Identifier.t list * Procedure.t list

	let add_decls ds' (ds,ps) = ds@ds', ps
	let map_procs fn = Tup2.map id (List.map fn)
	let map_stmts fn =
		map_procs <| fun (ty,name,args,ds,en,ss) ->
			ty, name, args, ds, en,
			LabeledStatement.map_stmts fn ss 
	let type_of_proc (_,ps) n =
		match List.filter (fun (_,n',_,_,_,_) -> n' = n) ps with
		| [] -> failwith <| Printf.sprintf "Procedure `%s' not found." n
		| p :: _ -> Procedure.signature p
			
	open PrettyPrinting
	let print (ds,ps) =
		Identifier.print_decls ds $+$ empty
		$+$ ( vcat <| List.map Procedure.print ps )
	let to_string = render << print
end

