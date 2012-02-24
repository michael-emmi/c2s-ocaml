module Identifier : sig
	type t = string

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Type : sig
	type t = Void | Bool of int

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Constant : sig
	type t = True | False

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module BinaryOp : sig
	type t = Or | And | Eq | Neq | Imp

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Expression : sig
	type t =
		Id of Identifier.t
		| Const of Constant.t
		| Not of t
		| Bin of BinaryOp.t * t * t
		| Schoose of t list

	val bool : bool -> t

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
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

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

and LabeledStatement : sig
	type t = Identifier.t list * Statement.t

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Procedure : sig
	type t = Type.t                    (* return type *)
			* Identifier.t             (* name *)
			* Identifier.t list        (* parameters *)
			* Identifier.t list        (* declarations *)
			* Expression.t option      (* enforce *)
			* LabeledStatement.t list  (* statements *)

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end

module Program : sig
	type t = Identifier.t list * Procedure.t list

	val add_decls : Identifier.t list -> t -> t
	val map_stmts :
		(LabeledStatement.t -> LabeledStatement.t list) -> t -> t
	val type_of_proc : t -> Identifier.t -> int * int

	val to_string : t -> string
	val print : t -> PrettyPrinting.doc
end
