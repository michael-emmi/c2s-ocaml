(** Translation from concurrent programs to Boogie programs. *)

open Prelude
open Printf
open CpAst
open CpUtils
open Operators

let ident = id
(* let nondet = sprintf "__nondet_%n" *)

module T = CpAst.Type
module TT = BplAst.Type
let rec type_ = function
	| T.Bool -> TT.Bool
	| T.Int -> TT.Int
	| T.T x -> TT.T (x,[])
	| T.Map (ts,t) -> TT.Map ([], List.map type_ ts, type_ t)
	| t -> failwith
		  << sprintf "Illegal type `%s' for Boogie programs."
		  <| T.to_string t

module L = CpAst.Literal
module LL = BplAst.Literal
let lit = function
	| L.Bool true -> LL.True
	| L.Bool false -> LL.False
	| L.Num n -> LL.Num n

module Bop = CpAst.BinaryOp
module BBop = BplAst.BinaryOp
let oper = function
	| Bop.Iff -> BBop.Iff
	| Bop.Imp -> BBop.Imp
	| Bop.Or -> BBop.Or
	| Bop.And -> BBop.And
	| Bop.Eq -> BBop.Eq
	| Bop.Neq -> BBop.Neq
	| Bop.Lt -> BBop.Lt
	| Bop.Gt -> BBop.Gt
	| Bop.Lte -> BBop.Lte
	| Bop.Gte -> BBop.Gte
	| Bop.Plus -> BBop.Plus
	| Bop.Minus -> BBop.Minus
	| Bop.Times -> BBop.Times
	| Bop.Div -> BBop.Div
	| Bop.Mod -> BBop.Mod

module E = CpAst.Expression
module EE = BplAst.Expression
let quant q = match q with
	| E.Forall -> EE.Forall
	| E.Exists -> EE.Exists
let rec expr e = match e with
	| E.Id i -> EE.Id (ident i)
	| E.Lit c -> EE.Lit (lit c)
	| E.Not e -> EE.Not (expr e)
	| E.Neg e -> EE.Neg (expr e)
	| E.Bin (op,e,f) -> EE.Bin (oper op, expr e, expr f)
	| E.Sel (e,es) -> EE.Sel (expr e, List.map expr es)
	| E.Upd (e,es,f) -> EE.Upd (expr e, List.map expr es, expr f)
	| E.Old e -> EE.Old (expr e)
	| E.FnApp (f,es) -> EE.FnApp (ident f, List.map expr es)
	| E.Q (q,xs,e) ->
		  EE.Q ( quant q, [],
				 List.map (fun (x,t) -> ident x, type_ t) xs,
				 [], [], expr e)
	| e -> failwith
		  << sprintf "Illegal expression `%s' for Boogie programs."
		  <| E.to_string e

let rec decider d = match d with
	| E.Choice -> None
	| e -> Some (expr e)

module Lv = CpAst.Lvalue
module LLv = BplAst.Lvalue
let rec lval lv = match lv with
	| Lv.Id i -> LLv.Id i
	| Lv.Sel (x,es) -> LLv.Sel (lval x, List.map expr es)

module S = CpAst.Statement
module Ls = CpAst.LabeledStatement
module SS = BplAst.Statement
module LLs = BplUtils.LabeledStatement
let rec stmt s =
	match s with
	| Ls.S (ls,s) -> begin
		  match List.map ident ls, s with
		  | [], S.Skip | [], S.Dead _ -> []
		  | ls, S.Skip | ls, S.Dead _ ->
				[ls, SS.Assume (EE.bool true)]

		  | ls, S.Goto ids -> [ls, SS.Goto (List.map ident ids)]
		  | ls, S.Return _ -> [ls, SS.Return]

		  | ls, S.Assign (xs,es,None)
				when List.for_all (function E.Choice -> true | _ -> false) es
					&& List.for_all (function Lv.Id _ -> true | _ -> false) xs
					-> [ ls, SS.Havoc (List.map (LLv.name << lval) xs) ]

		  | ls, S.Assign (xs,es,None) ->
		
				[ls, SS.Assign (List.map lval xs, List.map expr es)]
			
			(* 				let (ys,zs), (xs,es) =
								Tup2.map
									( List.partition
										  (function Lv.Id _ -> true | _ -> false)
									  << List.map fst )
									List.split
								<< List.partition
									(function (_,E.Choice) -> true | _ -> false)
								<| List.combine xs es in


							if ls <> [] then
								eprintf (
									"Warning: dropping labels in translation"
									^^ " to Boogie:\n %s\n" )
									(String.concat ", " ls);

							begin match 
								
								(* Normal assignments. *)
								(match xs with [] -> [] | _ ->
									 SS.Assign (List.map lval xs, List.map expr es) :: [])

								(* Simple lvalue havoc assignments. *)
								@ (match ys with [] -> [] | _ ->
									   SS.Havoc (List.map (LLv.name << lval) ys) :: [])

								(* General lvalue havoc assignments. *)
								@ (match zs with [] -> [] | _ ->
									   SS.Assign (
										   List.map lval zs,
										   List.make
											   (EE.ident << nondet)
											   (List.length zs) ) :: []
								  )

							with s::ss -> 
								(ls,s)::(List.map (Tup2.make []) ss)

							| _ -> assert false
							end *)
			
		  (* 		  | ls, S.Assign (xs,es,None) -> *)
		  (* 				[ls, SS.Assign ( *)
		  (* 					List.map lval xs, *)
		  (* 					List.map expr es )] *)

		  | ls, S.Ite (e,tss,ess) ->
				[ls, SS.If ( decider e,
							 List.flatten (List.map stmt tss),
							 List.flatten (List.map stmt ess) )]
		  | ls, S.While (e,es,ss) ->
				[ ls, SS.While (decider e, List.map
									(Tup2.ekam false << expr) es,
								List.flatten (List.map stmt ss)) ]
		  | ls, S.Assert e -> [ls, SS.Assert (expr e)]
		  | ls, S.Assume e -> [ls, SS.Assume (expr e)]
		  | ls, S.Call (p,es,lvs) ->
				[ls, SS.Call ( [], ident p,
							   List.map expr es,
							   List.map 
									(function Lv.Id x -> ident x
									| lv -> 
										failwith << sprintf (
											"Non-identifier lvalue '%s'" ^^
											" cannot appear in call assignment" ^^
											" in Boogie programs." )
										<| Lv.to_string lv)

									lvs )]

		  | _, S.Post _ | _, S.Yield _ | _, S.New _ ->
				failwith "Cannot express concurrency in Boogie programs."

		  | _, s -> failwith
				<< sprintf "Illegal statement `%s' for Boogie programs."
				<| S.to_string s

	  end
	| Ls.C c -> []

module A = CpAst.Attribute
module AA = BplAst.Attribute

let attr (a,vs) = ident a, List.map (fun v -> Right v) vs

module D = CpAst.Declaration
module DD = BplAst.Declaration

let rec decl prg d =
	match d with
	| D.Type (ax,x,tx,Some t) ->
		DD.TypeSyn (List.map attr ax, ident x, List.map ident tx, type_ t) 
		:: []
	| D.Type (ax,x,tx,None) ->
		DD.TypeCtor (List.map attr ax, false, ident x, List.map ident tx) 
		:: []
	| D.Var (ax,x,t) ->
		  DD.Var (List.map attr ax, ident x, type_ t, None) :: []
	| D.Const (ax,x,t,e) ->
		DD.Const (List.map attr ax, false, ident x, type_ t, ())
		:: ( match e with 
			 | Some e -> DD.Axiom ([], expr (E.ident x |=| e)) :: []
			 | _ -> [] )
	| D.Func (ax,f,(ps,t,e)) ->
		  DD.Func ( List.map attr ax, ident f, [],
					List.map (fun (p,t) -> Option.map ident p, type_ t) ps,
					(None, type_ t),
					Option.map expr e ) :: []
	| D.Proc (ax,n,p) -> proc prg ax n p :: []
	| D.Inv e ->
		  eprintf (
			  "Warning: losing invariant declaration in translation"
			  ^^ " to Boogie:\n %s\n" )
			  (E.to_string e);
		  DD.Axiom ([], EE.bool true) :: []

and proc pgm ax n (ps,ts,reqs,ens,ds,ss) =

	let ts = List.map (Tup2.map id type_) ts in
	let rs = List.mapi
		(fun i (x,t) ->
			 match x with
			 | None -> sprintf "__ret_%n" i
			 | Some x -> x )
		ts in


	(* Translate returns to assign to return variables before the
	   expression-free Boogie return statement. *)
	let bpl_ss = 
		List.flatten
 		<< List.map stmt
		<< Ls.map_stmts
			(function
			 | Ls.S (ls,S.Return es) when List.length es > 0 ->
				   assert (List.length es = List.length ts);
				   [ Ls.S (ls, S.Assign (List.map Lv.ident rs,
										 es, None)) ;
					 Ls.S ([], S.Return es) ]

			 | s -> s :: [] )
		<| ss in
	let mods = LLs.modifies bpl_ss in
	let params = 
		List.map (function
				  | (D.Var (_,x,t) | D.Const (_,x,t,None)) ->
						ident x, type_ t
				  | _ -> failwith "!" ) ps in
	let mod_params = 
		List.filter (((flip List.mem) mods) << fst) params in

	let param_to_init x =
		if List.mem x (List.map fst mod_params)
		then sprintf "%s__init" x
		else x in

	let reqs = List.map (expr << E.map_ident param_to_init) reqs
	and ens = List.map expr ens in

	let inline_depth = Options.get_int "recursion-depth" in

	(* NOTE: don't inline [main], nor any procedure marked as "modular". *)
	let do_inline =
		n <> "main"
		&& not (List.exists ((=) "modular" << fst) ax)
	in

	DD.Proc (
		(* Attributes. *)
		( if not do_inline then [] 
		  else BplAst.Attribute.num "inline" inline_depth :: [] )

		@ ( List.map attr
			(* NOTE: strip "modular" and "inline" attributes. *)
			<< List.filter ((<>) "modular" &&&& (<>) "inline" << fst)
			<| ax ),

		(* Name. *)
		n,

		(

			(* Type arguments. *)
			[],

			(* Input parameters. *)
			List.map (Tup2.map param_to_init id) params,

			(* Return variables. *)
			List.combine rs (List.map snd ts),

			(* Specification.. *)
			( List.reduce
				  ( fun ms ->
						BplAst.Specification.Modifies ( false, ms ) :: [])
				  []
			  << (flip List.minus) (List.map D.name ps)
			  << (flip List.minus) (List.map D.name ds)
			  <| mods )

			(* NOTE: For the moment Boogie inlining doesn't inline a procedure
			   which has an [ensures] or [requires] annotation.  So make sure
			   there is none if we want inlining. *)
			@ ( if do_inline then []
				else begin
					List.map
						(fun e -> BplAst.Specification.Requires (false,e))
						reqs
					@ List.map
						(fun e -> BplAst.Specification.Ensures (false,e))
						ens
				end ),

			(* Declarations.. *)
			( List.map (fun (x,t) -> DD.Var ([],x,t,None)) mod_params )
			@ ( List.map (function
						  | (D.Var (ax,x,t) | D.Const (ax,x,t,None)) ->
								DD.Var ( List.map attr ax,
										 ident x, type_ t, None )
						  | D.Const (ax,x,t,Some e) ->
								DD.Var ( List.map attr ax,
										 ident x, type_ t, 
										 Some (expr (E.ident x |=| e)) )
						  | _ -> failwith "!" ) ds ),

			(* Statements.. *)
			( match mod_params with
			  | [] -> id
			  | ms -> List.cons (
					[], SS.Assign (
						List.map (LLv.ident << fst) mod_params,
						List.map
							( EE.ident << param_to_init << fst) ms)))
				bpl_ss
		)
 	)

		

let program pgm = 
	List.flatten 
	<< List.map (decl pgm) 
	<< CpUtils.no_mixed_wildcard_expressions
	<< CpUtils.choice_ops_to_aux_variables
	<< CpUtils.only_idents_in_call_assigns
	<| pgm
