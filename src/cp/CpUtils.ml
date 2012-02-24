(** Utilities for concurrent programs. *)

open Prelude
open Printf
open CpAst

module S = Statement

module Expression = struct
	include Expression
	let parse = ParsingUtils.parse_string
		CpParser.expression_top
		CpLexer.token
end
module E = Expression

module LabeledStatement = struct
	include LabeledStatement
	let parse = ParsingUtils.parse_string
		CpParser.labeled_statements_top
		CpLexer.token
		
	let skip = stmt S.Skip
	let assign xs es e = stmt (S.Assign (xs,es,e))
	let assert_ e = stmt (S.Assert e)
	let assume e = stmt (S.Assume e)
	let ifthenelse e ss ts = stmt (S.Ite (e,ss,ts))
	let ifthen e ss = ifthenelse e ss [stmt S.Skip]
	let whiledo e es ss = stmt (S.While (e,es,ss))
	let call p ps xs = stmt (S.Call (p,ps,xs))
	let return es = stmt (S.Return es)
	let post p ps t = stmt (S.Post (p,ps,t))
	let yield = stmt S.Yield

	let incr e = 
		assign 
			[Lvalue.from_expr e] 
			[Expression.Bin (BinaryOp.Plus, e, Expression.num 1) ] 
			None
end
module Ls = LabeledStatement

module Declaration = struct
	include Declaration
	let parse = ParsingUtils.parse_string
		CpParser.declaration_top
		CpLexer.token
end

module Program = struct
	include Program
	let parse = ParsingUtils.parse_string
		CpParser.program_top
		CpLexer.token
		
	let order_declarations pgm =
		let vars = List.filter ((<>) "proc" << Declaration.kind) pgm
		and procs = List.filter ((=) "proc" << Declaration.kind) pgm 
		in vars @ procs
		
	let ensure_procedures_return pgm =
		
		let return_for proc =
			let ret_stmt = 
				Ls.return << Expression.choice << snd 
				<< Procedure.signature << Option.some 
				<| Program.find_proc pgm proc in

			Printf.eprintf
				"Warning: procedure %s has a non-returning path: adding '%s'.\n"
				proc (Ls.to_string ret_stmt);
			ret_stmt
		in
		
		let rec ensure_last_is_return proc ss = 
			if ss = [] then	[return_for proc]
			else 
				let init, last = List.init ss, List.last ss in
				init @ ( match last with
						| Ls.S (ls, S.Return _) -> [last]
						| Ls.S (ls, S.Ite (e,ts,us)) ->
							[Ls.S (ls, S.Ite (e, 
								ensure_last_is_return proc ts,
								ensure_last_is_return proc us))]
						| Ls.S (ls, S.While (e,es,ts)) ->
							[Ls.S (ls, S.While (e,es,ensure_last_is_return proc ts))]
						| s -> [s; return_for proc] )
		in
	
		map_decls
			(function Declaration.Proc (ax,n,p) ->
				let ps,ts,rs,es,ds,ss = p in
				
				let ss' = ensure_last_is_return n ss in
				
				Declaration.Proc (ax,n,(ps,ts,rs,es,ds,ss')) :: []
				
			| d -> List.unit d)
			pgm

	let ensure_well_formed = 
		id 
		<< ensure_procedures_return
end

module Operators = struct
	module A = Attribute
	module T = Type
	module D = Declaration
	module E = Expression
	module Lv = Lvalue
	module S = Statement
	module Ls = LabeledStatement
	module Bop = BinaryOp

	let (|:=|) xs es = Ls.assign (List.map Lv.from_expr xs) es None 
	let ($:=$) xs ys = List.map E.ident xs |:=| List.map E.ident ys
	let ($:=?$) xs () = Ls.assign (List.map Lv.ident xs) (E.choice xs) None

	let (|&|) e f = E.conj [e;f]
	let (|||) e f = E.disj [e;f]
	let (|=>|) e f = E.Bin (Bop.Imp, e, f)

	let ($&|) x f = E.conj [E.ident x; f]

	let ($&$) x y = E.conj [E.ident x; E.ident y]
	let ($&$) x y = E.conj [E.ident x; E.ident y]
	let (!$) x = E.Not (E.ident x)

	let (!|) e = E.Not e
	let (|=|) x y = E.Bin (Bop.Eq, x, y)
	let (|<|) x y = E.Bin (Bop.Lt, x, y)
	let (|<=|) x y = E.Bin (Bop.Lte, x, y)
	let (|>=|) x y = E.Bin (Bop.Gte, x, y)

		
	let (|+|) x y = E.Bin (Bop.Plus, x, y)
	let (|-|) x y = E.Bin (Bop.Minus, x, y)

	let lift_to_ids op = curry (uncurry op << Tup2.mapp E.ident)

	let ($=$) = lift_to_ids (|=|)
	let ($<$) = lift_to_ids (|<|)
	let ($>=$) = lift_to_ids (|>=|)
end

module Naming = struct
	let base = "auto"
	let uniquifiers : (string * int ref) list ref = ref []
	let naming_scheme = sprintf "%s_%n" base
	
	let gen_name p = 
		naming_scheme <|
		if List.mem_assoc p !uniquifiers then begin
			let r = List.assoc p !uniquifiers in
			let i = !r in
			incr r;
			i
		end
		else begin
			uniquifiers := (p,ref 1) :: !uniquifiers;
			0
		end
		
	let names p =
		List.make 
			naming_scheme
			(if List.mem_assoc p !uniquifiers 
				then !(List.assoc p !uniquifiers)
				else 0)
			
end

let no_mixed_wildcard_expressions pgm =
	Program.translate
		~per_stmt_map: (fun in_proc -> 
			function
			| Ls.S (ls, S.While (e,es,ss)) 
			when e <> E.Choice && E.occurs ((=) E.Choice) e ->
				let ss' = Ls.assume (E.simplify << E.det true <| e) :: ss in
				Ls.S (ls, S.While (E.Choice, es, ss')) :: []
				
			| Ls.S (ls, S.Ite (e,ss,ts))
			when e <> E.Choice && E.occurs ((=) E.Choice) e ->
				let ss' = Ls.assume (E.simplify << E.det true <| e) :: ss in
				Ls.S (ls, S.Ite (E.Choice, ss', ts)) :: []
				
			| s -> s :: [] )
		pgm

let choice_ops_to_aux_variables pgm =
	let new_vars : (string * (Identifier.t * Type.t) list ref) list ref 
		= ref [] in
	
	let add_var p v t = 
		if List.mem_assoc p !new_vars then
			let r = List.assoc p !new_vars in
			r := (v,t) :: !r
		else new_vars := (p,ref [v,t]) :: !new_vars
		
	and get_vars p =
		if List.mem_assoc p !new_vars then
			List.rev << (fun r -> !r) << List.assoc p <| !new_vars
		else []
	in
	
	Program.translate
		~add_local_decls: 
			(fun (n,_) -> List.map (fun (v,t) -> Declaration.Var ([],v,t)) (get_vars n) )

	<< Program.translate
		~per_stmt_map: (fun in_proc ->
			function Ls.S (ls, S.Assign (xs, es, ce)) -> begin
				let xs,es = List.split << List.map 
					(function (x,E.Choice) -> begin
						let v = Naming.gen_name in_proc in
						let t = CpTyping.Program.type_of pgm in_proc (Lvalue.to_expr x) in
						add_var in_proc v t;
						x, E.ident v
					end | (x,e) -> (x,e) )
					<| List.combine xs es					
				in [Ls.S (ls, S.Assign (xs,es,ce))]
			end
			| s -> [s])

	<| pgm

let only_idents_in_call_assigns pgm =
	let new_vars : (string * (Identifier.t * Type.t) list ref) list ref 
		= ref [] in
	
	let add_var p v t = 
		if List.mem_assoc p !new_vars then
			let r = List.assoc p !new_vars in
			r := (v,t) :: !r
		else new_vars := [p,ref [v,t]]
		
	and get_vars p =
		if List.mem_assoc p !new_vars then
			List.rev << (fun r -> !r) << List.assoc p <| !new_vars
		else []
	in			

	Program.translate
		~add_local_decls: 
			(fun (n,_) -> List.map (fun (v,t) -> Declaration.Var ([],v,t)) (get_vars n) )

	<< Program.translate
		~per_stmt_map: (fun in_proc ->
			function Ls.S (ls,S.Call (p,es,xs)) 
			when List.exists (function Lvalue.Id _ -> false | _ -> true) xs ->
			begin				
				match Option.map Procedure.signature <| Program.find_proc pgm p with
				| Some (_,ts) when List.length ts = List.length xs -> begin
					let ys = List.map 
						(fun t -> 
							let v = Naming.gen_name in_proc in
							add_var in_proc v t; 
							v) 
						ts in
					[ Ls.S (ls, S.Call (p,es, List.map Lvalue.ident ys)) ; 
					  Ls.S ([], S.Assign (xs, List.map E.ident ys, None)) ]
				end
				| _ -> failwith 
					<| sprintf "Could not resolve procedure %s/%n/%n." 
						p (List.length es) (List.length xs)
			end
			| s -> [s] )

	<| pgm

