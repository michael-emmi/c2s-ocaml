(** Asynchronous multi- to single-processor program translations. *)

open Prelude
open PrettyPrinting
open Printf
open CpAst
open CpUtils
open Operators

module Tr = CpTranslateUtils

let many_to_one p =

	let self_to_id = function E.Self -> E.ident Tr.self_var | e -> e in
	let null_to_id = 
		function E.Id x when x = "null" -> E.ident Tr.null_pid | e -> e in

	let vectorize_globals =
		function
		| E.Id x when x = Tr.err_flag -> E.Id x
		| E.Id x when Program.is_global_var p x -> E.Sel (E.Id x, [E.Id Tr.self_var])
		| e -> e
	in

	(* Translate [call xs := p (e1,..,ek)] to [call xs := p (e1,..,ek,self)]
	   and [post p (e1,..,ek) t] to [post p (e1,..,ek,t)]. *)
	let translate_stmt s =
		match s with
		| Ls.S (ls, S.Post (n,ps,e)) ->
			  Ls.S (ls, S.Post ( n, ps @ [ Option.reduce
											   (E.map self_to_id)
											   (E.Id Tr.self_var) e ],
								 e)) :: []
		
		| Ls.S (ls, S.Call (n,ps,xs)) when n = Tr.real_main_proc -> 
			Ls.S (ls, S.Call (n,ps@[E.ident Tr.root_pid],xs)) :: []
								
		| Ls.S (ls, S.Call (n,ps,xs))
				when Option.reduce
					(not << Declaration.has_attr "leavealone")
					false
					(Program.find_decl p n) 
				&& (not (List.mem n Tr.aux_procs)) ->

			  Ls.S (ls, S.Call ( n, ps @ [E.Id Tr.self_var], xs )) :: []

		| Ls.S (ls, S.New (E.Id x)) ->
			Ls.add_labels ls 
			<< List.unit
			<| Ls.call Tr.new_proc [E.ident Tr.self_var] [Lv.ident x]
			
		| Ls.S (ls, S.New e) ->
			failwith 
			<< sprintf "Unexpected expression '%s' with 'new' statement."
			<| E.to_string e

		| _ -> s::[]
	in

	Program.translate
		~add_global_decls: 
			[ D.Var ([],Tr.alloc_var, T.Map([T.T "pid"],T.Bool)) ;
			  D.Const ([],Tr.root_pid, T.T "pid", None) ;
			  D.Const ([],Tr.null_pid, T.T "pid", None) ;
			  Declaration.parse (sprintf 
				("proc %s (var %s: pid) : %s begin\n" ^^
				"    var p: %s\n" ^^
				"    p := *;\n" ^^
				"    assume (!%s[p]);\n" ^^
				"    %s[p] := true;\n" ^^
				"    return p\n" ^^
				"end")
				Tr.new_proc (Tr.self_var) (T.to_string Tr.pid_type)
				(T.to_string Tr.pid_type)
				Tr.alloc_var
				Tr.alloc_var
			) ]
	
		~map_global_decls:
			( List.unit 
 			  << function D.Var (ax,n,t) when n <> Tr.err_flag -> 
					D.Var (ax,n,T.Map([T.T "pid"],t))
		  	  | d -> d )
				
		~add_proc_params:
			(fun (n,_) -> 
				if List.mem n (Tr.main_proc :: Tr.aux_procs) then []
				else [D.Const ([],Tr.self_var,Type.T "pid",None)])
				
		~proc_body_prefix: 
			(fun (n,_) -> 
				if n = Tr.main_proc then []
				else if n = Tr.init_proc then [
					Ls.assume (
						(E.sel (E.ident Tr.alloc_var) [E.ident Tr.root_pid])
						|&| (E.sel (E.ident Tr.alloc_var) [E.ident Tr.null_pid]))
				] else [])

		~per_expr_map: (fun _ -> null_to_id << self_to_id << vectorize_globals )
		~per_stmt_map: (const translate_stmt)
	<| p
