(** Utilities for Boogie programs. *)

open Prelude
open BplAst

module Sp = Specification
module S = Statement
module Ls = LabeledStatement
module D = Declaration

(** Recalculate the [modifies] clause based on global variables which are
	actually (recursively) modified by each procedure. *)
let fix_modifies pgm =
	Program.map_procs
		( fun ((tx,ps,rs,sx,ds,ss) as proc) ->
			  tx, ps, rs, 

			  List.filter
				  (function Sp.Modifies _ -> false | _ -> true)
				  sx
				  
			  @ ( List.reduce (fun ms -> Sp.Modifies (false,ms) :: []) []
				  << Program.fold_over_calls pgm
				  ( flip <| fun (_,ps,rs,_,ds,ss) ->
						List.union
						<< (flip List.minus) (List.map fst ps)
						<< (flip List.minus) (List.map fst rs)
						<< (flip List.minus) (List.map D.name ds)
						<| LabeledStatement.modifies ss )
				  []
				  <| proc ),

			  ds, ss )
		pgm

let ignore_var_name i =
	let rec stringify_type t =
		match t with
		| Type.Map (_,ts,t) ->
			  Printf.sprintf "$%s$"
			  << String.concat "#"
			  << List.map stringify_type
			  << (flip List.tailcons) t
			  <| ts
		| t -> Type.to_string t
	in
	Printf.sprintf "__ignore_%n_%s" i
	<< stringify_type

let incomplete_calls pgm proc =
	List.map fst
	<< List.filter (uncurry (<>) << Tup2.map List.length List.length)
	<< Option.cat
 	<< List.map (fun (pn,_,ys) ->
					 Option.map ( Tup2.ekam ys
								  << snd
								  << Procedure.signature )
					 <| Program.find_proc pgm pn )
	<< LabeledStatement.calls
	<| Procedure.stmts proc

let max_rets =
	Program.fold_procs
		(fun n (_,_,rs,_,_,_) -> max n (List.length rs))
		0

(** Declarations for variables introduced because of incomplete return
	assignments. *)
let return_assign_decls pgm proc =
	let incomplete = incomplete_calls pgm proc in
	Option.cat
	<< List.map
		(fun (i,t) ->
			 if List.exists
				 (fun ts ->
					  i < List.length ts
					  && List.nth ts i = t)
				 incomplete
			 then
				 Some (D.Var (
						   [],
						   ignore_var_name i t,
						   t, None ))
			 else None)
		
	<< List.product (List.range 0 (max_rets pgm))
	<| [ Type.Bool;
		 Type.Int;
		 Type.Map ([],[Type.Int],Type.Int)
	   ] 
	
(** Transform [n]-ary return parameter [call p(e1,..,ek)] into [call x1,..,xn
	:= p(e1,..,ek)]. *)
let dont_ignore_returns pgm s =
	match s with
	| (ls,S.Call (ax,n,es,xs)) -> begin
 		  match Program.find_proc pgm n with
		  | Some p ->
				let _, ts = Procedure.signature p in

				if List.length xs = 0
					&& List.length ts > 0
				then begin
					Printf.eprintf ( "Warning: Boogie call to `%s' is missing"
									 ^^ " return assignments; will attempt to"
									 ^^ " add them.\n" ) n;

					let xs = List.mapi ignore_var_name ts in 
					(ls, S.Call (ax,n,es,xs)) :: []
				end
				else if List.length xs = List.length ts then
					(ls, S.Call (ax,n,es,xs)) :: []

				else
					failwith
					<| Printf.sprintf
							"Unmatched return assignment for procedure `%s'."
							n
							
		  | _ -> failwith
				<| Printf.sprintf "Could not resolve procedure `%s'." n
	  end
	| _ -> s :: []

(** Apply whatever transformations necessary for the back-end in use. *)
let prepare_for_back_end pgm =
	fix_modifies
	<< Program.map_stmts (dont_ignore_returns pgm)
	<< Program.map_procs
		(fun ((tx,ps,rs,sx,ds,ss) as proc) ->
			 tx, ps,rs,sx, ds @ (return_assign_decls pgm proc), ss)
	<| pgm

