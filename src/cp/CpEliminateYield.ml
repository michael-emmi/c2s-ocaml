(** Translations from programs with preemptible tasks (via [yield]) to
	programs with atomically executing tasks. *)

open Prelude
open PrettyPrinting
open Printf
open CpAst
open CpUtils
open Operators

module Tr = CpTranslateUtils

let stage_id = "YE"

module BitBlasting = struct

	let log2 n = log (float_of_int n) /. log (float_of_int 2)
	let width n = (truncate << floor << log2) n + 1

	let nth_bit x i = (flip <| sprintf "%s__%n" <| i) x
	let bits x = List.make (nth_bit x)

	let num_to_bits n m = 
		let rec br n m bs =
			if m = 0 then bs
			else br (n asr 1) (m-1) ((n mod 2 = 1)::bs)
		in List.map Expression.bool (br n m [])

	let var_eq_lit w x j =
		let jbits = num_to_bits j w in
		Expression.conj
		<< List.make (
			uncurry Expression.eq
			<< Tup2.map (Expression.ident << nth_bit x) (List.nth jbits)
			<< Tup2.dup )
		<| w

	let var_ge_var w x y =
		List.fold_left
			(fun e i ->
				 let xi = nth_bit x i
				 and yi = nth_bit y i in
				 ( xi $&| (!$ yi)) ||| ((xi $=$ yi) |&| e) )
			( Expression.bool true )
		<< List.rev
		<| List.make id w

	let var_assign_var w x y =
		Ls.assign
			(List.map Lvalue.ident (bits x w))
			(List.map Expression.ident (bits y w))
			None

	let var_assign_guess w x =
		Ls.assign 
			(List.map Lvalue.ident (bits x w)) 
			(Expression.choice <| List.make id w) 
			None
end

module BB = BitBlasting


let ignore_yield =
	Program.translate
		~per_stmt_map:
			(const <| function
		 		| Ls.S ([],Statement.Yield) -> [] 
		 		| Ls.S (ls,Statement.Yield) -> [Ls.S (ls,Statement.Skip)] 
		 		| s -> [s])
	<< id

(* let lal_reps_bool k p = *)
(* 	let rounds = k+1 in *)
(* 	let base = BB.width (rounds-1) in *)

(* 	let (@=@) = BB.var_eq_lit base *)
(* 	and (@>=@) = BB.var_ge_var base *)
(* 	and (@:=@) x y = *)
(* 		match y with *)
(* 		| "*" -> BB.var_assign_guess base x *)
(* 		| _ -> BB.var_assign_var base x y in *)

(* 	let cnt = "__rnd" *)
(* 	and ncnt = "__nxt_rnd" in *)

(* 	let err_flag = "__ye__err" in *)

(* 	let repl = flip (sprintf "%s__%n") in *)
(* 	let init = flip (sprintf "%s__init__%n") in *)

(* 	let gvar_decls = *)
(* 		List.filter ((=) "var" << D.kind) (Program.decls p) in *)
(* 	let gvec_decls = List.make *)
(* 		(fun i -> List.map (D.rename (repl i)) gvar_decls) *)
(* 		rounds in *)
(* 	let ginit_decls = List.make *)
(* 		(fun i -> List.map *)
(* 			 (function D.Var (g,t) -> D.Const (init i g,t) *)
(* 				  | _ -> assert false) *)
(* 			 gvar_decls) *)
(* 		rounds in *)

(* 	let cnt_decls = *)
(* 		List.map (fun x -> D.Var (x,Type.Bool))	(BB.bits cnt base) *)
(* 	and ncnt_decls = *)
(* 		List.map (fun x -> D.Var (x,Type.Bool)) (BB.bits ncnt base) in *)

(* 	let gvars = List.map D.name gvar_decls in *)

(* 	let translate_yield s = *)
(* 		match gvars, s with *)
(* 		| [], Ls.S (ls,Statement.Yield) -> *)
(* 			  [ Ls.C ("Begin [[yield]] (Y-E)") ; *)
(* 				Ls.S ( ls, Statement.Skip) ; *)
(* 				Ls.C ("End [[yield]] (Y-E)") *)
(* 			  ] *)
(* 		| gvars, Ls.S (ls,Statement.Yield) -> begin *)

(* 			  [ Ls.C ("Begin [[yield]] (Y-E)") ; *)
(* 				Ls.S (ls,Statement.Skip) ; *)
(* 				ncnt @:=@ "*" ; *)
(* 				Ls.stmt <| Statement.Assume (ncnt @>=@ cnt) ; *)
				
(* 				Ls.case ( *)
(* 					List.make *)
(* 						(fun i -> cnt @=@ i, *)
(* 							 [ List.map (repl i) gvars $:=$ gvars ]) *)
(* 						rounds ) ; *)
(* 				Ls.case ( *)
(* 					List.make *)
(* 						(fun i -> ncnt @=@ i, *)
(* 							 [ gvars $:=$ List.map (repl i) gvars ]) *)
(* 						rounds ) ; *)

(* 				cnt @:=@ ncnt ; *)
(* 				Ls.C ("End [[yield]] (Y-E)") *)
(* 			  ] *)
(* 		  end *)
(* 		| _ -> [s] *)
(* 	in *)


(* 	let translate_body ss = *)
(* 		Ls.C ("Begin [[enter-proc]] (Y-E)")  *)
(* 		:: Ls.case ( *)
(* 			List.make *)
(* 				(fun i -> cnt @=@ i, *)
(* 					 [   *)
(* 						 gvars $:=$ List.map (repl i) gvars ; *)
(* 					 ]) *)
(* 				rounds ) *)
(* 		:: Ls.C ("End [[enter-proc]] (Y-E)") *)
(* 		:: ss *)
			
(* 	in *)

(* 	let translate_assert s = *)
(* 		match s with *)
(* 		| Ls.S (ls, Statement.Assert e) -> *)
(* 			  [ Ls.C ("End [[assert]] (Y-E)") ; *)
(* 				Ls.S (ls, Statement.Assign ( *)
(* 						  [Lvalue.ident err_flag], *)
(*  						  [Expression.Not e], *)
(* 						  None )) ; *)
(* 				Ls.C ("End [[assert]] (Y-E)") *)
(* 			  ] *)
(* 		| _ -> [ s ] *)
(* 	in *)

(* 	let translate_post_and_call_and_ret s = *)
(* 		match s with *)
(* 		| Ls.S (ls, Statement.Post (p,es,e)) -> *)
(* 			  [ Ls.S (ls, Statement.Post *)
(* 						  (p, es @ (List.map Expression.ident *)
(* 									<| BB.bits cnt base),e)) ] *)
(* 		| Ls.S (ls, Statement.Call (p,es,xs)) -> *)
(* 			  [ Ls.S (ls, Statement.Call *)
(* 						  (p, es @ (List.map Expression.ident *)
(* 									<| BB.bits cnt base), *)
(* 						   xs @ (BB.bits cnt base))) ] *)

(* 		| Ls.S (ls, Statement.Return es) -> *)
(* 			  Ls.C ("Begin [[leave-proc]] (Y-E)") *)
(* 			  :: Ls.case ( *)
(* 				  List.make *)
(* 					  (fun i -> cnt @=@ i, *)
(* 						   [ List.map (repl i) gvars $:=$ gvars ]) *)
(* 					  rounds) *)
(* 			  :: Ls.C ("End [[leave-proc]] (Y-E)") *)
(* 			  :: Ls.S (ls, *)
(* 					   Statement.Return *)
(* 						   (es @ (List.map Expression.ident *)
(* 								  <| BB.bits cnt base))) *)
(* 			  :: [] *)
(* 		| _ -> [s] *)
(* 	in *)

(* 	let pre_stitching = *)
(* 		Expression.conj (List.map (fun g -> g $=$ init 0 g) gvars) *)
(* 		|&| ( Expression.conj *)
(* 			  << List.make *)
(* 			  (fun i -> *)
(* 				   Expression.conj *)
(* 				   << List.map *)
(* 					   (fun g -> repl i g $=$ init i g ) *)
(* 				   <| gvars) *)
(* 			  <| rounds) *)
(* 	in *)

(* 	let validity_predicate = *)
(* 		Expression.conj *)
(* 		<< List.make *)
(* 			(fun i -> *)
(* 				 Expression.conj *)
(* 				 << List.map (fun g -> repl i g $=$ init (i+1) g) *)
(* 				 <| gvars) *)
(* 		<| rounds-1 in *)

(* 	let checker = *)
(* 		Declaration.parse *)
(* 			( "proc __ye__check () : void begin" *)
(* 			  ^ ( sprintf " assert ( __ye__err => !( %s ) )" *)
(* 				  <| Expression.to_string validity_predicate ) *)
(* 			  ^ " end" ) *)
(* 	in *)
(* 	let new_main = *)
(* 		Declaration.parse *)
(* 			( "proc main () : void begin" *)
(* 			  ^ ( sprintf " assume (%s);" *)
(* 				  <| Expression.to_string pre_stitching ) *)
(* 			  ^ " __ye__err := false;" *)
(* 			  ^ ( sprintf " call __ye__main (%s);" *)
(* 				  << PrettyPrinting.render *)
(* 				  << Expression.print_seq *)
(* 				  <| BB.num_to_bits 0 base ) *)
(* 			  ^ " post __ye__check ()" *)
(* 			  ^ " end" ) *)
(* 	in *)

(* 	( if rounds < 2 then *)
(* 		  Program.map_stmts *)
(* 			  (function *)
(* 				   | Ls.S (ls,Statement.Yield) -> *)
(* 						 [ Ls.S (ls,Statement.Skip) ] *)
(* 				   | s -> [s]) *)

(* 	  else *)
(* 		  Program.map_stmts translate_yield *)
(* 		  << Program.add_decls [ D.Var ("__ye__err", Type.Bool) ] *)
(* 		  << Program.add_decls [checker] *)
(* 		  << Program.add_decls [new_main] *)
(* 		  << Program.map_stmts translate_assert *)
(* 		  << Program.map_stmts translate_post_and_call_and_ret *)
(* 		  << Program.add_decls (List.flatten ginit_decls) *)
(* 		  << Program.add_decls (List.flatten gvec_decls) *)
(* 		  << Program.map_procs *)
(* 			  (fun ((n,_,_,_,_,_,_,ss) as p) -> *)
(* 				   List.singleton *)
(* 				   << Procedure.map_stmts translate_body *)
(* 				   << ( Procedure.rename *)
(* 							(function "main" -> "__ye__main" | n -> n) ) *)
(* 				   << ( if LabeledStatement.contains_rec *)
(* 							(function *)
(* 								 | Ls.S (_,Statement.Yield) -> true *)
(* 								 | _ -> false) *)
(* 							ss *)
(* 						then Procedure.add_decls ncnt_decls *)
(* 						else id ) *)
(* 				   << ( Procedure.add_rets *)
(* 							(List.make (const Type.Bool) base) ) *)
(* 				   << ( Procedure.add_params cnt_decls ) *)
(* 				   <| p) ) *)
(* 	<| p *)


(* let lal_reps_scalar k p = *)
(* 	let rounds = k+1 in *)

(* 	let cnt = "__rnd" *)
(* 	and ncnt = "__nxt_rnd" in *)
(* 	let err_flag = "__ye__err" in *)
(* 	let repl = flip (sprintf "%s__%n") in *)
(* 	let init = flip (sprintf "%s__init__%n") in *)

(* 	let gvar_decls = *)
(* 		List.filter ((=) "var" << D.kind) (Program.decls p) in *)
(* 	let gvec_decls = List.make *)
(* 		(fun i -> List.map (D.rename (repl i)) gvar_decls) *)
(* 		rounds in *)

(* 	let ginit_decls = List.make *)
(* 		(fun i -> List.map *)
(* 			 (function D.Var (g,t) -> D.Const (init i g,t) *)
(* 				  | _ -> assert false) *)
(* 			 gvar_decls) *)
(* 		rounds in *)

(* 	let cnt_decls = [ D.Var (cnt,Type.Int) ] *)
(* 	and ncnt_decls = [ D.Var (ncnt,Type.Int) ] in *)

(* 	let gvars = List.map D.name gvar_decls in *)

(* 	let translate_yield s = *)
(* 		match gvars, s with *)
(* 		| [], Ls.S (ls, S.Yield) -> *)
(* 			  [ Ls.C ("Begin [[yield]] (Y-E)") ; *)
(* 				Ls.S ( ls, S.Skip) ; *)
(* 				Ls.C ("End [[yield]] (Y-E)") *)
(* 			  ] *)
(* 		| gvars, Ls.S (ls, S.Yield) -> begin *)

(* 			  [ Ls.C ("Begin [[yield]] (Y-E)") ; *)
(* 				Ls.S (ls, S.Skip) ; *)

(* 				[ncnt] $:=?$ () ; *)
(* 				Ls.stmt <| S.Assume ( *)
(* 					(ncnt $>=$ cnt) |&| (E.ident ncnt |<| E.num rounds) *)
(* 				) ; *)
				
(* 				Ls.case ( *)
(* 					List.make *)
(* 						(fun i -> E.ident cnt |=| E.num i, *)
(* 							 [ List.map (repl i) gvars $:=$ gvars ]) *)
(* 						rounds ) ; *)
(* 				Ls.case ( *)
(* 					List.make *)
(* 						(fun i -> E.ident ncnt |=| E.num i, *)
(* 							 [ gvars $:=$ List.map (repl i) gvars ]) *)
(* 						rounds ) ; *)

(* 				[cnt] $:=$ [ncnt] ; *)
(* 				Ls.C ("End [[yield]] (Y-E)") *)
(* 			  ] *)
(* 		  end *)
(* 		| _ -> [s] *)
(* 	in *)


(* 	let translate_body ss = *)
(* 		Ls.C ("Begin [[enter-proc]] (Y-E)")  *)
(* 		:: Ls.case ( *)
(* 			List.make *)
(* 				(fun i -> E.ident cnt |=| E.num i, *)
(* 					 [   *)
(* 						 gvars $:=$ List.map (repl i) gvars ; *)
(* 					 ]) *)
(* 				rounds ) *)
(* 		:: Ls.C ("End [[enter-proc]] (Y-E)") *)
(* 		:: ss *)
			
(* 	in *)

(* 	let translate_assert s = *)
(* 		match s with *)
(* 		| Ls.S (ls, S.Assert e) -> *)
(* 			  [ Ls.C ("End [[assert]] (Y-E)") ; *)

(* 				Ls.S (ls, S.Skip) ; *)
(* 				[E.ident err_flag] |:=| [(!| e) ||| E.ident err_flag] ; *)
				
(* 				Ls.C ("End [[assert]] (Y-E)") *)
(* 			  ] *)
(* 		| _ -> [ s ] *)
(* 	in *)

(* 	let translate_post_and_call_and_ret s = *)
(* 		match s with *)
(* 		| Ls.S (ls, Statement.Post (p,es,e)) -> *)
(* 			  [ Ls.S (ls, Statement.Post *)
(* 						  (p, es @ [ Expression.ident cnt ], e)) ] *)

(* 		| Ls.S (ls, Statement.Call (p,es,xs)) -> *)
(* 			  [ Ls.S (ls, Statement.Call *)
(* 						  (p, es @ [ E.ident cnt ], *)
(* 						   xs @ [cnt] )) ] *)

(* 		| Ls.S (ls, Statement.Return es) -> *)
(* 			  Ls.C ("Begin [[leave-proc]] (Y-E)") *)
(* 			  :: Ls.case ( *)
(* 				  List.make *)
(* 					  (fun i -> *)
(* 						   (E.ident cnt |=| E.num i), *)
(* 						   [ List.map (repl i) gvars $:=$ gvars ]) *)
(* 					  rounds) *)
(* 			  :: Ls.C ("End [[leave-proc]] (Y-E)") *)
(* 			  :: Ls.S (ls, S.Return (es @ [ E.ident cnt ])) *)
(* 			  :: [] *)
(* 		| _ -> [s] *)
(* 	in *)

(* 	let pre_stitching = *)
(* 		Expression.conj (List.map (fun g -> g $=$ init 0 g) gvars) *)
(* 		|&| ( Expression.conj *)
(* 			  << List.make *)
(* 			  (fun i -> *)
(* 				   Expression.conj *)
(* 				   << List.map *)
(* 					   (fun g -> repl i g $=$ init i g ) *)
(* 				   <| gvars) *)
(* 			  <| rounds) *)
(* 	in *)

(* 	let validity_predicate = *)
(* 		Expression.conj *)
(* 		<< List.make *)
(* 			(fun i -> *)
(* 				 Expression.conj *)
(* 				 << List.map (fun g -> repl i g $=$ init (i+1) g) *)
(* 				 <| gvars) *)
(* 		<| rounds-1 in *)

(* 	let checker = *)
(* 		D.parse *)
(* 			( "proc __ye__check () : void begin" *)
(* 			  ^ ( sprintf " assert ( __ye__err => !( %s ) )" *)
(* 				  <| Expression.to_string validity_predicate ) *)
(* 			  ^ " end" ) *)
(* 	in *)
(* 	let new_main = *)
(* 		D.parse *)
(* 			( "proc main () : void begin" *)
(* 			  ^ ( sprintf " assume (%s);" *)
(* 				  <| Expression.to_string pre_stitching ) *)
(* 			  ^ " __ye__err := false;" *)
(* 			  ^ " call __ye__main (0);"  *)
(* 			  ^ " post __ye__check ()" *)
(* 			  ^ " end" ) *)
(* 	in *)

(* 	( if rounds < 2 then *)
(* 		  Program.map_stmts *)
(* 			  (function *)
(* 				   | Ls.S (ls,Statement.Yield) -> *)
(* 						 [ Ls.S (ls,Statement.Skip) ] *)
(* 				   | s -> [s]) *)

(* 	  else *)
(* 		  Program.map_stmts translate_yield *)
(* 		  << Program.add_decls [ D.Var ("__ye__err", Type.Bool) ] *)
(* 		  << Program.add_decls [checker] *)
(* 		  << Program.add_decls [new_main] *)
(* 		  << Program.map_stmts translate_assert *)
(* 		  << Program.map_stmts translate_post_and_call_and_ret *)
(* 		  << Program.add_decls (List.flatten ginit_decls) *)
(* 		  << Program.add_decls (List.flatten gvec_decls) *)
(* 		  << Program.map_procs *)
(* 			  (fun ((n,_,_,_,_,_,_,ss) as p) -> *)
(* 				   List.singleton *)
(* 				   << Procedure.map_stmts translate_body *)
(* 				   << ( Procedure.rename *)
(* 							(function "main" -> "__ye__main" | n -> n) ) *)
(* 				   << ( if LabeledStatement.contains_rec *)
(* 							(function *)
(* 								 | Ls.S (_,Statement.Yield) -> true *)
(* 								 | _ -> false) *)
(* 							ss *)
(* 						then Procedure.add_decls ncnt_decls *)
(* 						else id ) *)
(* 				   << ( Procedure.add_rets [Type.Int] ) *)
(* 				   << ( Procedure.add_params cnt_decls ) *)
(* 				   <| p) ) *)
(* 	<| p *)


(* let lal_reps_with_minimal_map_select k p = *)
(* 	let rounds = k+1 in *)

(* 	(\* New names. *\) *)
	
(* 	let cnt = "__ye__rnd" *)
(* 	and ncnt = "__ye__nxt_rnd" *)
(* 	and main = "__ye__main" *)
(* 	and checker = "__ye__check" *)
(* 	and err_flag = "__ye__err" *)

(* 	and replv g i = E.sel (E.ident <| sprintf "%s__ye__vec" g) [E.ident i] *)
(* 	and repli g i = E.sel (E.ident <| sprintf "%s__ye__vec" g) [E.num i] *)
(* 	and initi g i = E.sel (E.ident <| sprintf "%s__ye__vec_init" g) [E.num i] *)
(* 	in *)

(* 	let gvar_decls = List.filter ((=) "var" << D.kind) (Program.decls p) in *)
(* 	let gvars = List.map D.name gvar_decls in *)

(* 	(\* New declarations. *\) *)

(* 	let extra_gvar_decls = *)
(* 		[ D.Var (err_flag, Type.Bool) ] *)
(* 		@ List.map ( D.rename (sprintf "%s__ye__vec") *)
(* 					 << D.retype (fun t -> Type.Map ([Type.Int], t)) ) *)
(* 			gvar_decls *)
(* 		@ List.map ( D.to_const *)
(* 					 << D.rename (sprintf "%s__ye__vec_init") *)
(* 					 << D.retype (fun t -> Type.Map ([Type.Int], t)) ) *)
(* 			gvar_decls *)

(* 	and extra_proc_params = [ D.Var (cnt,Type.Int) ] *)
(* 	and extra_proc_decls = [ D.Var (ncnt,Type.Int) ] *)
(* 	in *)

(* 	(\* Translation of program statements. *\) *)
	
(* 	let translate_yield s = *)
(* 		match s with *)
(* 		| Ls.S (ls, S.Yield) -> [ *)
(* 			  Ls.add_labels ls ([ncnt] $:=?$ ()); *)
(* 			  Ls.stmt <| S.Assume ( *)
(* 				  (ncnt $>=$ cnt) |&| (E.ident ncnt |<| E.num rounds) *)
(* 			  ) ; *)
(* 			  List.map (flip replv <| cnt) gvars |:=| List.map E.ident gvars; *)
(* 			  List.map E.ident gvars |:=| List.map (flip replv <| ncnt) gvars; *)
(* 			  [cnt] $:=$ [ncnt]; *)
(* 		  ] *)
(* 		| _ -> s :: [] *)
	
(* 	and translate_body ss = *)
(* 		(List.map E.ident gvars |:=| List.map (flip replv <| cnt) gvars ) *)
(* 		:: ss *)

(* 	and translate_return s = *)
(* 		match s with *)
(* 		| Ls.S (_, S.Return _) -> *)
(* 			  [ List.map (flip replv <| cnt) gvars |:=| *)
(* 						List.map E.ident gvars ; *)
(* 				s *)
(* 			  ] *)
(* 		| _ -> s :: [] *)

(* 	and translate_assert s = *)
(* 		match s with *)
(* 		| Ls.S (ls, S.Assert e) -> *)
(* 			  Ls.add_labels ls ( *)
(* 				  [E.ident err_flag] |:=| [(!| e) ||| E.ident err_flag] *)
(* 			  ) :: [] *)
(* 		| _ -> s :: [ ] *)

(* 	and round_counter_threading s = *)
(* 		match s with *)
(* 		| Ls.S (_, (S.Post _ | S.Call _ | S.Return _ )) -> *)
(* 			  Ls.add_params [cnt] [E.ident cnt] s :: [] *)
(* 		| _ -> s :: [] *)

(* 	(\* Validity predicates. *\) *)

(* 	and init_predicate = *)
(* 		Expression.conj (List.map (fun g -> E.ident g |=| initi g 0) gvars) *)
(* 		|&| ( Expression.conj *)
(* 			  << List.make *)
(* 			  (fun i -> *)
(* 				   Expression.conj *)
(* 				   << List.map (fun g -> repli g i |=| initi g i) *)
(* 				   <| gvars) *)
(* 			  <| rounds) *)

(* 	and validity_predicate = *)
(* 		Expression.conj *)
(* 		<< List.make *)
(* 			(fun i -> *)
(* 				 Expression.conj *)
(* 				 << List.map *)
(* 					 (fun g -> repli g i |=| initi g (i+1)) *)
(* 				 <| gvars) *)
(* 		<| rounds-1 *)
(* 	in *)

(* 	(\* Auxiliary procedures. *\) *)
	
(* 	let checker_proc = *)
(* 		D.parse *)
(* 		<| sprintf ( *)
(* 			"proc %s () : void begin\n" *)
(* 			^^ "   assert ( %s => !( %s ) )\n" *)
(* 			^^ "end\n" ) *)
(* 				checker *)
(* 			err_flag *)
(* 			(E.to_string validity_predicate) *)

(* 	and new_main_proc = *)
(* 		D.parse *)
(* 		<| sprintf ( *)
(* 			"proc main () : void begin\n" *)
(* 			^^ "   assume ( %s );\n" *)
(* 			^^ "   %s := false;\n"  *)
(* 			^^ "   call %s (0);\n"  *)
(* 			^^ "   post %s ()\n" *)
(* 			^^ "end\n" ) *)
(* 				(E.to_string init_predicate) *)
(* 			err_flag *)
(* 			main *)
(* 			checker *)
(* 	in *)

(* 	( if rounds < 2 or List.length gvars = 0 then *)
(* 		  Program.map_stmts *)
(* 			  (function *)
(* 				   | Ls.S (ls,Statement.Yield) -> [Ls.S (ls,Statement.Skip)]  *)
(* 				   | s -> [s]) *)

(* 	  else *)
(* 		  id *)
(* 		  << Program.add_decls [checker_proc; new_main_proc] *)
(* 		  << Program.add_decls extra_gvar_decls *)
(* 		  << Program.map_stmts translate_yield *)
(* 		  << Program.map_stmts translate_assert *)
(* 		  << Program.map_stmts round_counter_threading *)
(* 		  << Program.map_stmts translate_return *)
(* 		  << Program.map_procs *)
(* 			  (fun ((n,_,_,_,_,_,_,ss) as p) -> *)
(* 				   List.singleton *)
(* 				   << Procedure.map_stmts translate_body *)
(* 				   << ( Procedure.rename *)
(* 							(function "main" -> main | n -> n) ) *)
(* 				   << ( if LabeledStatement.contains_rec *)
(* 							(function *)
(* 								 | Ls.S (_,Statement.Yield) -> true *)
(* 								 | _ -> false) *)
(* 							ss *)
(* 						then Procedure.add_decls extra_proc_decls *)
(* 						else id ) *)
(* 				   << ( Procedure.add_rets [Type.Int] ) *)
(* 				   << ( Procedure.add_params extra_proc_params ) *)
(* 				   <| p) ) *)
(* 	<| p *)

let lal_reps_with_maps k p =
	let rounds = k+1 in
	

	(* New names. *)
	
	let cnt = sprintf "__%s__rnd" stage_id
	and ncnt = sprintf "__%s__nxt_rnd" stage_id
	and bound = sprintf "__%s__K" stage_id

	and replv g i = E.sel (E.ident <| sprintf "%s" g) [E.ident i]
	and repli g i = E.sel (E.ident <| sprintf "%s" g) [E.num i]
	and initi g i = E.sel (E.ident <| sprintf "%s__%s__vec_init" g stage_id) [E.num i]
	in

	let gvar_decls = List.filter ((=) "var" << D.kind) (Program.decls p) in
	let gvars = List.map D.name gvar_decls in

	(* New declarations. *)

	let new_gvar_decls =
		D.Const ([], bound, Type.Int, Some (E.num rounds)) 
		:: List.map ( D.rename (sprintf "%s")
					  << D.retype (fun t -> Type.Map ([Type.Int], t)) )
			gvar_decls
		@ List.map ( D.to_const
					 << D.rename (fun x -> sprintf "%s__%s__vec_init" x stage_id)
					 << D.retype (fun t -> Type.Map ([Type.Int], t)) )
			gvar_decls

	and extra_proc_params = [ D.Var ([],cnt,Type.Int) ]
	and extra_proc_decls = [ D.Var ([],ncnt,Type.Int) ]
	in

	(* Validity predicates. *)

	let init_predicate =
		( E.ident bound |=| E.num rounds )
	|&|	( Expression.conj
		  << List.make
		  (fun i ->
			   Expression.conj
			   << List.map (fun g -> repli g i |=| initi g i)
			   <| gvars)
		  <| rounds )

	and validity_predicate =
		Expression.conj
		<< List.make
			(fun i ->
				 Expression.conj
				 << List.map
					 (fun g -> repli g i |=| initi g (i+1))
				 <| gvars)
		<| rounds-1
	in

	(* Translation of program statements. *)
	
	let translate_yield s =
		match s with
		| Ls.S (ls, S.Yield) -> 
			  Ls.add_labels ls [
				[ncnt] $:=?$ ();
				Ls.assume ((ncnt $>=$ cnt) |&| (ncnt $<$ bound));
			  	[cnt] $:=$ [ncnt];
			  ]
		| _ -> s :: []
			  

	and round_counter_threading in_proc s =
		match s with
		| Ls.S (_, (S.Call (p,_,_) | S.Post (p,_,_)))
		when not (List.mem p Tr.aux_procs) ->
			Ls.add_params [cnt] [E.ident cnt] s :: []
		| Ls.S (_, S.Return _ ) 
		when not (List.mem in_proc (Tr.main_proc :: Tr.aux_procs))->
			Ls.add_params [cnt] [E.ident cnt] s :: []
		| _ -> s :: []

	and current_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> replv x cnt
		| _ -> e
		
	and first_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> repli x 0
		| _ -> e
		
	and last_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> repli x k
		| _ -> e
	
	in

	( if rounds < 2 or List.length gvars = 0 then
		Program.translate
			~per_stmt_map:
			  (const <| function
				| Ls.S ([],Statement.Yield) -> []
			   	| Ls.S (ls,Statement.Yield) -> [Ls.S (ls,Statement.Skip)] 
				| s -> [s])
		<< id

	else
		Program.translate
			~rem_global_decls: gvars
			~add_global_decls: new_gvar_decls
			
			~add_proc_params: 
				(fun (n,_) -> 
					if List.mem n (Tr.main_proc :: Tr.aux_procs) then [] 
					else extra_proc_params)
					
			~add_proc_rets: 
				(fun (n,_) -> 
					if List.mem n (Tr.main_proc :: Tr.aux_procs) then [] 
					else [None, Type.Int])
			
			~add_local_decls: 
				( fun (n,p) -> 
					if n = Tr.main_proc then extra_proc_params
					else if Ls.contains_rec 
						(function Ls.S (_,S.Yield) -> true | _ -> false)
						(Procedure.stmts p)
					then extra_proc_decls 
					else [] )
					
			~proc_body_prefix: 
				(fun (n,_) -> 
					if n = Tr.main_proc then
						[ [E.ident cnt] |:=| [E.num 0] ]
					else if n = Tr.init_proc then
						[ Ls.assume init_predicate ]
					else if n = Tr.validate_proc then
						[ Ls.assume validity_predicate ]
					else [] )
					
			~per_stmt_map: 
				(fun n -> 
					if List.mem n Tr.aux_procs then List.unit
					else translate_yield <=< round_counter_threading n)
			~per_expr_map: 
				(fun p -> 
					if p = Tr.init_proc then first_gval gvars
					else if p = Tr.check_proc then last_gval gvars
					else current_gval gvars)

		  << id ) p

