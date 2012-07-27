(** Translation from concurrent programs to Boolean programs. *)

open Prelude
open Printf
open CpAst

let ident = id

module T = CpAst.Type
module TT = BpAst.Type
let type_ = function
	| T.Void -> TT.Void
	| T.Bool -> TT.Bool 1
	| T.Tuple ts when
		  List.for_all (function T.Bool -> true | _ -> false) ts ->
		  TT.Bool (List.length ts)
	| t -> failwith
		  << sprintf "Illegal type `%s' for Boolean programs."
		  <| T.to_string t

module L = CpAst.Literal
module LL = BpAst.Constant
let lit = function
	| L.Bool true -> LL.True
	| L.Bool false -> LL.False
	| l -> failwith
		  << sprintf "Illegal literal `%s' for Boolean programs."
		  <| L.to_string l

module Bop = CpAst.BinaryOp
module BBop = BpAst.BinaryOp
let oper = function
	| Bop.Or -> BBop.Or
	| Bop.And -> BBop.And
	| Bop.Eq -> BBop.Eq
	| Bop.Neq -> BBop.Neq
	| Bop.Imp -> BBop.Imp
	| op -> failwith
		  << sprintf "Illegal binary operator `%s' for Boolean programs."
		  <| Bop.to_string op


module E = CpAst.Expression
module EE = BpAst.Expression
let rec expr e = match e with
	| E.Id i -> EE.Id (ident i)
	| E.Lit c -> EE.Const (lit c)
	| E.Not e -> EE.Not (expr e)
	| E.Bin (op,e,f) -> EE.Bin (oper op, expr e, expr f)
	| E.Choice -> EE.Schoose [EE.bool false; EE.bool false]
	| E.Schoose es -> EE.Schoose (List.map expr es)
	| e -> failwith
		  << sprintf "Illegal expression `%s' for Boolean programs."
		  <| E.to_string e

let rec decider d = match d with
	| E.Choice -> None
	| e -> Some (expr e)

module Lv = CpAst.Lvalue
let lval = function
	| Lv.Id i -> ident i
	| lv -> failwith
		  << sprintf "Illegal lvalue `%s' for Boolean programs."
		  <| Lv.to_string lv

module S = CpAst.Statement
module Ls = CpAst.LabeledStatement
module SS = BpAst.Statement
let rec stmt s =
	match s with
	| Ls.S (ls,s) -> begin
		  List.map ident ls,
		  match s with
		  | S.Skip -> SS.Skip
		  | S.Goto ids -> SS.Goto (List.map ident ids)
		  | S.Return es -> SS.Return (List.map expr es)
		  | S.Assign (lvs,es,ce) ->
				SS.Assign (
					List.map lval lvs,
					List.map expr es,
					Option.map expr ce )
		  | S.Ite (e,tss,ess) ->
				SS.Ite ( decider e,
						 List.map stmt tss,
						 List.map stmt ess )
		  | S.While (e,es,ss) ->
				if es <> [] then
					Printf.eprintf (
						"Warning: dropping invariant annotation"
						^^ " in the Cp-to-Bp translation." );
					
				SS.While (decider e, List.map stmt ss)
		  | S.Assert e -> SS.Assert (decider e)
		  | S.Assume e -> SS.Assume (expr e)
		  | S.Call (p,es,lvs) ->
				SS.Call ( ident p,
						  List.map expr es,
						  List.map lval lvs )
		  | S.Dead ids -> SS.Dead (List.map ident ids)

		  | S.Post _ | S.Yield | S.New _ ->
				failwith "Cannot express concurrency in Boolean program."
	  end
	| Ls.C c -> [], SS.Skip

module D = Declaration
		  
let proc n = function
	| ps,ts,rs,es,ds,ss ->
		  if rs <> [] or es <> [] then
			  Printf.eprintf ( "Warning: dropping requires/ensures annotation"
							   ^^ " in the Cp-to-Bp translation." );

		  type_ (Type.compress <| List.map snd ts), ident n,
		  List.map (ident << Declaration.name) ps,
		  List.map (ident << Declaration.name) ds,
		  None,
		  List.map stmt ss

let program p =
	List.map (ident << D.name)
	<< List.filter ((=) "var" |||| (=) "const" << D.kind)
	<| Program.decls p,

	List.map (function D.Proc (_,n,p) -> proc n p | _ -> failwith "!")
	<< List.filter ((=) "proc" << D.kind)
	<| Program.decls p
