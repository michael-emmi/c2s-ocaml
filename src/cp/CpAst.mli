module Identifier : sig
	type t = string
	val rename : string -> string -> t -> t
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end

module Attribute : sig
	type t = string * string list
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end

module Type : sig
	type t = Void | Bool | Int
			 | Intv of int * int
			 | T of Identifier.t
			 | Tuple of t list
			 | Map of t list * t
	val compress : t list -> t
	val flatten : t -> t list
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Literal : sig
	type t = Bool of bool
			 | Num of int
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module BinaryOp : sig
	type t = Iff | Imp | Or | And
			 | Eq | Neq | Lt | Gt | Lte | Gte
			 | Plus | Minus | Times | Div | Mod
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Expression : sig
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

	val ident : Identifier.t -> t
	val bool : bool -> t
	val num : int -> t
	val sel : t -> t list -> t
	val upd : t -> t list -> t -> t
	val choice : 'a list -> t list
	val eq : t -> t -> t
	val neq : t -> t -> t
	val forall : (Identifier.t * Type.t) list -> t -> t
	val exists : (Identifier.t * Type.t) list -> t -> t
	val occurs : (t -> bool) -> t -> bool
	val det : bool -> t -> t
	val simplify : t -> t
	val conj : t list -> t
	val disj : t list -> t
	val map : (t -> t) -> t -> t
	val map_ident : (Identifier.t -> Identifier.t) -> t -> t
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end

module Lvalue : sig
	type t = Id of Identifier.t
			 | Sel of t * Expression.t list

	val name : t -> Identifier.t
	val ident : Identifier.t -> t
	val sel : Identifier.t -> Expression.t list -> t
	val to_expr : t -> Expression.t
	val from_expr : Expression.t -> t
	val lift : (Expression.t -> Expression.t) -> t -> t

	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
	val to_string : t -> string
end

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

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

and LabeledStatement : sig
	type t = S of Identifier.t list * Statement.t
			 | C of string

	val comment : string -> t
	val stmt : Statement.t -> t
	val case : (Expression.t * t list) list -> t

	val map : (Statement.t -> Statement.t) -> t -> t

	val cons_label : Identifier.t -> t -> t
	val add_labels : Identifier.t list -> t list -> t list
	val add_params : Identifier.t list -> Expression.t list -> t -> t

	val one_label_per_stmt : t -> t list
	val one_target_per_goto : t -> t list
	val no_dead_stmts : t -> t list
	val no_constrain_clauses : t -> t list

	val contains_rec : (t -> bool) -> t list -> bool
	val fold_left_rec : ('a -> t -> 'a) -> 'a -> t list -> 'a
	val map_stmts : (t -> t list) -> t list -> t list
		
	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end

module Function : sig
	type t = (Identifier.t option * Type.t) list
			* Type.t
			* Expression.t option
	val signature : t -> Type.t list * Type.t
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
	val fold_stmts :
		('a -> LabeledStatement.t -> 'a) -> 'a -> t -> 'a
	val map_stmts : (LabeledStatement.t -> LabeledStatement.t list)
		-> t -> t

	val to_string : Attribute.t list -> Identifier.t -> t -> string
	val print : Attribute.t list -> Identifier.t -> t -> PrettyPrinting.doc
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

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
	val print_seq : t list -> PrettyPrinting.doc
end

module Program : sig
	type t = Declaration.t list
	val resolve : t -> Identifier.t -> Type.t option
	val decls : t -> Declaration.t list
	val procs : t -> Declaration.t list
	val is_global : t -> Identifier.t -> bool
	val is_global_var : t -> Identifier.t -> bool
	val find_decl : t -> Identifier.t -> Declaration.t option
	val find_proc : t -> Identifier.t -> Procedure.t option

	val add_decls : Declaration.t list -> t -> t
	val rem_decls : Identifier.t list -> t -> t

	val map_decls : (Declaration.t -> Declaration.t list) -> t -> t
	val map_stmts :
		(LabeledStatement.t -> LabeledStatement.t list) -> t -> t
	val map_exprs : (Expression.t -> Expression.t) -> t -> t
	val map_lvals : (Lvalue.t -> Lvalue.t) -> t -> t
		
	val translate : 
		(* Global declarations. *)
		?rem_global_decls: Identifier.t list ->
		?map_global_decls: (Declaration.t -> Declaration.t list) ->
		?rename_global_decls: (Identifier.t -> Identifier.t) ->
		?add_global_decls: Declaration.t list ->
		(* Procedure declarations. *)
		?add_proc_params: ((Identifier.t * Procedure.t) -> Declaration.t list) ->
		?add_local_decls: ((Identifier.t * Procedure.t) -> Declaration.t list) ->
		?add_proc_rets: ((Identifier.t * Procedure.t) -> (Identifier.t option * Type.t) list)->	
		?proc_body_prefix: ((Identifier.t * Procedure.t) -> LabeledStatement.t list) ->
		?proc_body_suffix: ((Identifier.t * Procedure.t) -> LabeledStatement.t list) ->
		(* Expressions and statements. *)
		?per_stmt_map: (Identifier.t -> LabeledStatement.t -> LabeledStatement.t list) ->
		?per_expr_map: (Identifier.t -> Expression.t -> Expression.t) ->
		t -> t

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end
