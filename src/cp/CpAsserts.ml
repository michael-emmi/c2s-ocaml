(** Consolidation of assertions to a single, final program point. *)

open Prelude
open PrettyPrinting
open Printf
open CpAst
open CpUtils
open Operators

module Tr = CpTranslateUtils

(** Replaces assertions by error-flag setting, and adds an assertion to
  * ensure the error flag has not been set at the end of execution. *)
let assert_to_err_flag with_exn p =
	
	let translate_stmt in_proc s =
		match s with
		| Ls.S (ls, S.Assert e) ->
			Ls.add_labels ls [
				[E.ident Tr.err_flag] |:=| [E.ident Tr.err_flag ||| (!| e)]
			] @
				
			( if with_exn then
				let ret_expr = 
					E.choice << snd << Procedure.signature 
					<< Option.some
					<| Program.find_proc p in_proc in
				[ Ls.ifthen (E.ident Tr.err_flag) [Ls.return ret_expr] ]
			else [] )

		| Ls.S (ls, (S.Call _ | S.Yield as s)) -> 

			Ls.add_labels ls [ Ls.stmt s ]
			@ ( if with_exn then
					let ret_expr = 
						E.choice << snd << Procedure.signature 
						<< Option.some
						<| Program.find_proc p in_proc in
					[ Ls.ifthen (E.ident Tr.err_flag) [Ls.return ret_expr] ]
				else [] )

		| _ -> s :: []
	in
	
	Program.translate
		~add_global_decls: 
			(List.map Declaration.parse [
				sprintf "var %s: bool" Tr.err_flag;
				sprintf (
					"proc %s (): void begin\n"
					^^ "   call %s ();\n"
					^^ "   call %s ();\n"
					^^ "   call %s ();\n"
					^^ "   call %s ();\n"
					^^ "   return\n"
					^^ "end" )
					Tr.main_proc 
					Tr.init_proc Tr.real_main_proc 
					Tr.validate_proc Tr.check_proc;
				sprintf (
					"proc %s (): void begin\n"
					^^ "   %s := false;\n"
					^^ "   return\n"
					^^ "end" ) Tr.init_proc Tr.err_flag ;
				sprintf (
					"proc %s (): void begin\n"
					^^ "   return\n"
					^^ "end" ) Tr.validate_proc ;					
				sprintf (
					"proc %s (): void begin\n"
					^^ "   assert ( !%s );\n"
					^^ "   return\n"
					^^ "end" ) Tr.check_proc Tr.err_flag
			])
		~per_stmt_map: (fun n -> translate_stmt n)
		~rename_global_decls: (Identifier.rename Tr.main_proc Tr.real_main_proc)
	<| p