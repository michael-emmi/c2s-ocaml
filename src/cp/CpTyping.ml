open Prelude
open Printf


module rec Type : sig
	type t = CpAst.Type.t
end = struct
	include CpAst.Type
end

and Literal : sig
	type t = CpAst.Literal.t
	val type_of : t -> Type.t
end = struct
	include CpAst.Literal
	let type_of = function
		| Bool _ -> CpAst.Type.Bool
		| Num _ -> CpAst.Type.Int
end

and BinaryOp : sig
	type t = CpAst.BinaryOp.t
	val type_of : t -> Type.t
end = struct
	include CpAst.BinaryOp
	let type_of = function
		| Iff | Imp | Or | And | Eq | Neq | Lt | Gt | Lte | Gte -> CpAst.Type.Bool
		| _ -> CpAst.Type.Int
end

and Expression : sig
	type t = CpAst.Expression.t
	val type_of : Program.t -> Procedure.t -> t -> Type.t
end = struct
	include CpAst.Expression
	
	let rec type_of pgm proc e =
		match e with
		| Id x -> begin
			match CpAst.Procedure.resolve proc x, 
				  CpAst.Program.resolve pgm x with
			| Some t, _ -> t
			| _, Some t -> t
			| _ -> failwith <| sprintf "Could not resolve identifier '%s'." x
		end

		| Lit l -> Literal.type_of l
		| Not e | Neg e -> type_of pgm proc e
		| Bin (op,_,_) -> BinaryOp.type_of op
		| Choice -> CpAst.Type.Void
		| Schoose (e::_) -> type_of pgm proc e
		| Schoose [] -> CpAst.Type.Void
		| Tuple es -> CpAst.Type.Tuple (List.map (type_of pgm proc) es)

		| Sel (e,es) -> begin
			match type_of pgm proc e with
			| CpAst.Type.Map (_,t) -> t
			| _ -> failwith "Unexpected type of select expression."
		end
		| Upd (e,_,_) -> type_of pgm proc e
		| Old e -> type_of pgm proc e
		(* | FnApp (f,es) -> begin
			match Program.find_proc with
			| Type.
		end *)
		
		| Q _ -> CpAst.Type.Bool
		
		| _ -> failwith "!!"
		

end

and Procedure : sig
	type t = CpAst.Procedure.t
end = struct
	include CpAst.Procedure
end

and Program : sig
	type t = CpAst.Program.t
	val type_of : t -> CpAst.Identifier.t -> Expression.t -> Type.t
end = struct
	include CpAst.Program
	let type_of pgm in_proc = 
		Expression.type_of pgm (Option.some <| find_proc pgm in_proc)
end