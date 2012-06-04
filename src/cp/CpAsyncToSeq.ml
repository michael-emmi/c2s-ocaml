(** Asynchronous to sequential program translations. *)

open Prelude
open PrettyPrinting
open Printf
open CpAst
open CpUtils
open Operators

module Tr = CpTranslateUtils

let stage_id = "A2S"

(** An encoding of the depth-first task-scheduling order, a restriction of 
 * the unordered (i.e., "bag") semantics.*)
let post_to_call_dfs p =

	let guess = fun x -> sprintf "%s__%s__guess" x stage_id
	and saved = fun x-> sprintf "%s__%s__saved" x stage_id
	and next = fun x-> sprintf "%s__%s___next" x stage_id
	and next_init = fun x-> sprintf "%s__%s__next_init" x stage_id
	in

	let gs_decls = 
		List.filter ((=) "var" << D.kind) (Program.decls p) in

	let extra_global_decls =
		List.map (D.rename next) gs_decls
		@ List.map (D.to_const << D.rename next_init) gs_decls

	and extra_proc_decls =
		List.map (D.rename saved) gs_decls
		@ List.map (D.rename guess) gs_decls
	in

	let gs = List.map D.name gs_decls in
	let saved_gs = List.map saved gs
	and next_gs = List.map next gs
	and init_next_gs = List.map next_init gs
	and guess_gs = List.map guess gs
	in

	let init_predicate = 
		E.conj << List.map (uncurry ($=$))
		<| List.combine init_next_gs next_gs

	and validity_predicate = 
		E.conj << List.map (uncurry ($=$))
		<| List.combine init_next_gs gs
	in

	let translate_post s =
		match s with
		| Ls.S (_, S.Post (_,_,Some _)) ->
			  failwith "Unexpected inter-node post."

		| Ls.S (ls,S.Post (p,es,None)) -> 
			Ls.add_labels ls [
			  saved_gs $:=$ gs;
			  gs $:=$ next_gs;
			  guess_gs $:=?$ ();
			  next_gs $:=$ guess_gs;

			  Ls.stmt <| S.Call (p,es,[]);

			  Ls.stmt <| S.Assume
					  ( E.conj (List.map (fun g -> g $=$ guess g) gs) ) ;

			  gs $:=$ saved_gs;
			  Ls.stmt <| S.Dead (saved_gs @ guess_gs);
		  ]
		| _ -> [s]
		
	and last_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> E.ident (next x)
		| E.Sel (E.Id x,es) when List.mem x gs -> E.Sel (E.ident (next x), es)
		| _ -> e
	
	in

	(if List.length gs = 0 then
		Program.translate
			~per_stmt_map: 
				(fun _ -> function
				   | Ls.S (_, S.Post (_,_,Some _)) ->
						 failwith "Unexpected inter-node post."
				   | Ls.S (ls,S.Post (p,es,None)) ->
						 Ls.S (ls,S.Call (p,es,[]))::[]
				   | s -> s :: [])
		<< id
	else
		Program.translate
			~add_global_decls: extra_global_decls
			~per_stmt_map: (const translate_post)
			~proc_body_prefix: 
				(fun (n,_) -> 
					if n = Tr.init_proc then
						[ Ls.assume init_predicate ]
					else if n = Tr.validate_proc then
						[ Ls.assume validity_predicate ]
					else [] )
			~add_local_decls: 
				( fun (n,_) -> 
					if Option.reduce 
						(Ls.contains_rec 
							(function Ls.S (_,S.Post _) -> true | _ -> false))
						false
						<< Option.map Procedure.stmts
						<| Program.find_proc p n
					then extra_proc_decls
					else [] )
			~per_expr_map: 
				(fun p -> 
					if p = Tr.check_proc then last_gval gs
					else id)
		<< id 
	)
	<| p
	

(** An encoding of the breadth-first task-scheduling order, which preserves
 * the ordered "FiFo" task-buffer semantics, though only executes tasks up
 * a given level [k] of the task-creation tree. *)
let post_to_call_bfs k delay fewer_phases p =
	let phases = k+1 in

	let phase_var = sprintf "__%s__phase" stage_id
	and next_phase_var = sprintf "__%s__next_phase" stage_id
	and bound = sprintf "__%s__K" stage_id
	
	and shift_var = sprintf "__%s__shift" stage_id
	and init_shift_var = sprintf "__%s__shift_init" stage_id
	and delay_var = sprintf "__%s__delay" stage_id
	and ancestor_var = sprintf "__%s__ancestor" stage_id
	and anc_stack_var = sprintf "__%s__anc_saved" stage_id
	
	and next_shift_var = sprintf "__%s__next_shift" stage_id
	and next_delay_var = sprintf "__%s__next_delay" stage_id

	and repl_var = fun x-> sprintf "%s__%s__vec" x stage_id
	and init_var = fun x-> sprintf "%s__%s__vec_init" x stage_id
	in
	
	let shift_expr phase pid = 
		E.sel (E.sel (E.ident shift_var) [phase]) [pid] in
	
	let phase_expr = 
		if delay > 0 then
			E.ident phase_var 
			|+| shift_expr (E.ident phase_var) (E.ident Tr.self_var)
		else E.ident phase_var
	in
	
	let repl g e = E.sel (E.ident <| repl_var g) [e]
	and init g e = E.sel (E.ident <| init_var g) [e] in

	let gvars =
		List.map D.name
		<< List.filter ((=) "var" << D.kind) <| Program.decls p
	in

	let new_global_decls =
		[ D.Const ([], bound, Type.Int, Some (E.num phases)) ]
		@ ( if delay > 0 then [
				D.Var ([], shift_var, Type.Map ([Type.Int], 
					Type.Map ([Tr.pid_type], Type.Int)));
				D.Var ([], init_shift_var, Type.Map ([Type.Int], 
					Type.Map ([Tr.pid_type], Type.Int)));
				D.Var ([], delay_var, Type.Int )
			] else [] )
		@ ( if fewer_phases then [
				D.Var ([], ancestor_var, Type.Map ([Tr.pid_type], Type.Int))
			] else [] )

		@ List.flatten (List.map (
			function D.Var (ax,g,t) ->	[
				  (* D.Var (ax, g, t) ; *)
				  D.Var (ax, repl_var g, Type.Map ([Type.Int],t)) ;
				  D.Const (ax, init_var g, Type.Map ([Type.Int],t), None) ]
			| _ -> [] ) <| Program.decls p)
	in

	let init_predicate = 
		( Expression.conj
		<< ( if delay > 0 then (@) [
			E.forall ["ph", T.Int; "pi", Tr.pid_type] 
				(shift_expr (E.ident "ph") (E.ident "pi") 
				|=| E.num 0) 
			] else id ) 
		<< List.make
			(fun i ->
				Expression.conj
				( List.map 
					(fun g -> repl g (E.num i) |=| init g (E.num i))
					gvars
				@ ( if delay > 0 then [
						E.sel (E.ident shift_var) [E.num i] 
							|=| E.sel (E.ident init_shift_var) [E.num i]
					] else [] ) ) )
		<| phases+delay )

	and validity_predicate = 
		Expression.conj
		<< List.make
			(fun i ->
				Expression.conj
				( List.map
					(fun g -> repl g (E.num i) |=| init g (E.num <| i+1))
				 	gvars 
				@ ( if delay > 0 then [
						E.sel (E.ident shift_var) [E.num i]
						|=| E.sel (E.ident init_shift_var) [E.num <| i+1]
					] else [] ) ) )
		<| phases+delay-1 
	in

	let post_to_call_no_globals =
		function
		| Ls.S (ls, S.Post (n,es,None)) -> Ls.S (ls, S.Call (n,es,[])) :: []
		| Ls.S (_, S.Post (_,_,Some _)) -> failwith "Unexpected inter-node post"
		| s -> s :: []
		
	and post_to_skip =
		function
		| Ls.S (ls, S.Post _) -> Ls.S (ls, S.Skip) :: []
		| s -> s :: []

	and post_to_call =
		function
			
		| Ls.S (ls,S.Post (p,es,t)) ->
			
			let target_pid = 
				match t with None -> E.ident Tr.self_var | Some e -> e in
			
			Ls.add_labels ls (
				( if delay > 0 then [
					
					Ls.ifthen E.Choice [
						[ E.ident next_delay_var; E.ident next_shift_var ] 
							|:=| [E.Choice; E.Choice] ;
						Ls.assume (
							(E.ident next_delay_var |>=| E.num 0)
							|&| (E.ident next_delay_var |>=| E.ident delay_var) 
							|&| (E.ident next_delay_var |<| E.num delay)
							|&| (E.ident next_shift_var |>=| E.num 0)
							|&| (E.ident next_shift_var 
								|>=| shift_expr (E.ident phase_var) 
									(E.ident Tr.self_var))
							|&| (E.ident next_shift_var |<| E.num delay)
							|&| (E.ident next_shift_var |<=| E.ident next_delay_var)
						);
						[ E.ident delay_var ; 
						  shift_expr (E.ident phase_var) (E.ident Tr.self_var) ]
							|:=| [E.ident next_delay_var; E.ident next_shift_var]
					]
					
					(* Ls.whiledo (E.Choice |&| (E.ident delay_var |<| E.num delay)) [
						E.ident delay_var |>=| E.num 0;
						E.ident delay_var |<=| E.num delay;
						E.forall ["ph", T.Int; "pi", Tr.pid_type] 
							(shift_expr (E.ident "ph") (E.ident "pi") 
							|>=| E.num 0);
						E.forall ["ph", T.Int; "pi", Tr.pid_type] 
							(shift_expr (E.ident "ph") (E.ident "pi") 
							|<=| E.ident delay_var)
					] [
						Ls.incr (shift_expr (E.ident phase_var) (E.ident Tr.self_var));
						Ls.incr (E.ident delay_var)
					]; *)
				] else [] )
				@ [ [E.ident next_phase_var] |:=| [phase_expr] ]
				@ ( if fewer_phases then [
						Ls.ifthenelse
						 	(E.sel (E.ident ancestor_var) [target_pid] 
								|<| E.ident next_phase_var)
							[ Ls.skip ]
							[ [E.ident next_phase_var] |:=| 
								[E.ident next_phase_var |+| E.num 1 ] ]
					] else [ 
						[E.ident next_phase_var] |:=| 
							[ E.ident next_phase_var |+| E.num 1 ] 
					])
				@ [	Ls.ifthen
						(E.ident next_phase_var |<| E.num phases)
						(	( if fewer_phases then [
								[E.ident anc_stack_var] |:=| 
									[E.sel (E.ident ancestor_var) [target_pid]];
								[E.sel (E.ident ancestor_var) [target_pid]] 
									|:=| [E.ident next_phase_var]
							] else [])
							@ [ Ls.stmt (S.Call (p,es@[E.ident next_phase_var],[])) ]
							@ (if fewer_phases then [
								[E.sel (E.ident ancestor_var) [target_pid]] 
									|:=| [E.ident anc_stack_var]
								] else []))
				] )
		| s -> s :: []
		
	and call = function
		| Ls.S (ls, S.Call (n,ps,xs)) when n = Tr.real_main_proc ->
			[ Ls.S (ls, S.Call (n,ps@[E.num 0],xs)) ]
		| Ls.S (ls, S.Call (n,ps,xs)) when not (List.mem n Tr.aux_procs) ->
			Ls.add_labels ls [ Ls.call n (ps@[E.ident phase_var]) xs ]
		| s -> s :: []
		
	and return in_proc = function
		| Ls.S (ls, S.Return e) 
		when not (List.mem in_proc (Tr.main_proc :: Tr.aux_procs)) ->
			Ls.add_labels ls [
				List.map (flip repl <| phase_expr) gvars |:=| List.map E.ident gvars;
				Ls.stmt (S.Return e)
			]
		| s -> s::[]
		
	and first_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> repl x (E.num 0)
		| _ -> e
	
	and last_gval gs e =
		match e with
		| E.Id x when List.mem x gs -> repl x (E.num <| k + delay)
		| _ -> e
	
	in


	( if List.length gvars = 0 then
		Program.translate
			~per_stmt_map: (const post_to_call_no_globals)
		<< id
		
	else if k = 0 then
		Program.translate 
			~per_stmt_map: (const post_to_skip)
		<< id
	else
		Program.translate
			~add_global_decls: new_global_decls
			~rem_global_decls: gvars
			~add_proc_params: 
				(fun (n,_) ->
					if List.mem n (Tr.main_proc :: Tr.aux_procs) then [] 
				  	else [D.Const ([],phase_var,T.Int,None)])
			~add_local_decls: 
				(fun (n,_) ->
					if n = Tr.main_proc then []
					else if List.mem n Tr.aux_procs then []
					else begin
						(if fewer_phases 
							then [ D.Var ([], anc_stack_var, T.Int) ] 
							else [])
						@ (if delay > 0 
							then [
								D.Var ([], next_delay_var, T.Int) ;
								D.Var ([], next_shift_var, T.Int)
							] else [])
						@ [ D.Var ([], next_phase_var, T.Int) ]
						@ (List.filter ((=) "var" << D.kind) <| Program.decls p)
					end)
			~proc_body_prefix: 
				(fun (n,_) -> 
					if n = Tr.main_proc then []
					else if n = Tr.init_proc then
						(Ls.assume init_predicate)
						:: (if delay > 0 then [
							[E.ident delay_var] |:=| [E.num 0] ;
							[shift_expr (E.num 0) (E.ident Tr.root_pid)]
								|:=| [E.num 0]
							] else [])
						@ ( if fewer_phases 
							then [ [E.sel (E.ident ancestor_var) [E.ident Tr.root_pid]]
								|:=| [E.num 0] ] 
							else [] )
					else if n = Tr.validate_proc then [ Ls.assume validity_predicate ]
					else if n = Tr.check_proc then []
					else [ List.map E.ident gvars |:=| 
								List.map (flip repl <| phase_expr) gvars ]) 

			~per_stmt_map: 
				(fun n -> 
					if List.mem n Tr.aux_procs then List.unit
					else return n <=< call <=< post_to_call)
			~per_expr_map:
				(fun n -> 
					if n = Tr.init_proc then first_gval gvars
					else if n = Tr.check_proc then last_gval gvars
					else id)
		<< id )
	<| p


(** An encoding of the unordered task-scheduling semantics restricted so that
 * at most [k] pending tasks exist at any moment. *)
let post_to_call_bounded_bag k p =
	let bound = k in

	let procq = sprintf "__%s__bag" stage_id
	and pidx = sprintf "__%s__idx" stage_id
	and argqi = fun x-> sprintf "__%s__argq%n_i" stage_id x
	and argqb = fun x-> sprintf "__%s__argq%n_b" stage_id x
	and proc = sprintf "__%s__p" stage_id
	and argi = fun x-> sprintf "__%s__arg%n_i" stage_id x
	and argb = fun x-> sprintf "__%s__arg%n_b" stage_id x
	and size = sprintf "__%s__bag_size" stage_id
	and dispatch_proc = sprintf "__%s__dispatch" stage_id
	in

	let proc_ids =
		List.mapi (fun i pr -> D.name pr, i+1) 
		<< List.filter 
			( not 
			  << (flip List.mem) 
					(Tr.main_proc :: Tr.real_main_proc :: Tr.aux_procs)
			  << D.name )
		<| Program.procs p
	in

	let max_args =
		List.fold_left max 0
		<< List.map ( List.length << fst << Procedure.signature )
		<< List.map (function D.Proc (_,_,p) -> p | _ -> failwith "!")
		<| Program.procs p
	in
	
	let new_global_decls =
		List.map Declaration.parse [
			sprintf "var %s: [int] int" procq;
			sprintf "var %s: int" size
		]
		@ ( List.flatten 
			<< List.make
				(fun i -> 
					List.map Declaration.parse [
						sprintf "var %s: [int] bool" (argqb (i+1));
						sprintf "var %s: [int] int" (argqi (i+1))
					])
			<| max_args )
	in
	
	let new_local_decls = 
		List.map Declaration.parse [
			sprintf "var %s: int" pidx
		]
	in

	let dispatch_stmt =
		Ls.case
		<< List.map
			(fun (n,pid) ->
				 match Option.map Procedure.signature
					 (Program.find_proc p n)
				 with
				 | None -> failwith
					   <| sprintf "Cannot resolve procedure `%s'." proc

				 | Some (ts,_) ->
					   let args, sels =
						   List.split
						   << List.mapi
							   ( fun i t -> match t with
								 | Type.Bool ->
									   E.ident (argb (i+1)),
									   E.sel (E.ident <| argqb (i+1)) [E.ident pidx]
								 | Type.Int ->
									   E.ident (argi (i+1)),
									   E.sel (E.ident <| argqi (i+1)) [E.ident pidx]
								 | _ -> failwith "Unexpected type." )
						   <| ts
					   in

					   E.ident proc |=| E.num pid,
					   ( match args with [] -> id | _ -> List.cons (args |:=| sels) )
						   ( [Ls.stmt (S.Call (n, args, [])) ])
			)
		<| proc_ids
	in

	let dispatch_proc_decl =
		Declaration.parse
		<| sprintf (
			"proc %s () : void begin\n"
			^^ "   var %s: int\n"			(* proc *)
			^^ "   var %s: int\n"			(* pidx *)
			^^ "   %s\n"
			^^ "   %s\n"
			^^ "   while * do \n"
			^^ "      %s := *;\n"			(* pidx *)
			^^ "      %s := %s[%s];\n"		(* proc, procq, pidx *)
			^^ "      %s[%s] := 0;\n"		(* procq, pidx*)
			^^ "      %s\n"					(* size := size - 1 ? *)
			^^ "      %s\n"					(* dispatch_stmt *)
			^^ "   done;\n"
			^^ "end" )
			dispatch_proc
			proc
			pidx
			( String.map_whole (sprintf "var %s: bool")
			  << String.concat ", "
			  << List.make (fun i -> argb (i+1))
			  <| max_args ) 
			( String.map_whole (sprintf "var %s: int")
			  << String.concat ", "
			  << List.make (fun i -> argi (i+1))
			  <| max_args ) 
			pidx
			proc procq pidx
			procq pidx
			( if bound < 0 then "" else sprintf "%s := %s - 1;" size size )
			(Ls.to_string dispatch_stmt)
	in
	
	let init_predicate = 
		Expression.parse (sprintf "(forall i: int :: %s[i] = 0)" procq)
	
	and insert_dispatch_loop n s =
		match s with
		| Ls.S (ls, S.Return es) when n = Tr.real_main_proc ->
			Ls.add_labels ls [
				Ls.call dispatch_proc [] [];
				Ls.return es
			]
		| _ -> [s]

	and post_to_buffer s =
		match s with
		| Ls.S (_, S.Post (_,_,Some _)) ->
			  failwith "Unexpected inter-node post."

		| Ls.S (ls,S.Post (proc,es,None)) -> begin

			  match Option.map Procedure.signature (Program.find_proc p proc)
			  with
			  | None -> failwith
					<| sprintf "Cannot resolve procedure `%s'." proc
			  | Some (ts,_) -> begin
					let arg_assigns =
						(flip (|:=|)) es
						<< List.mapi
							( fun i t -> match t with
							  | Type.Bool ->
									E.sel (E.ident <| argqb (i+1)) [E.ident pidx]
							  | Type.Int ->
									E.sel (E.ident <| argqi (i+1)) [E.ident pidx]
							  | _ -> failwith "Unexpected type." )
						<| ts
					in
					
					if bound < 0 then
						Ls.add_labels ls
						<< Ls.parse <| sprintf (
								"%s := *;\n"
								^^ "%s[%s] := %n;\n"
								^^ "%s" )
							pidx
							procq pidx (List.assoc proc proc_ids)
							( match ts with [] -> "" 
							  | _ -> Ls.to_string arg_assigns )
					else
						Ls.add_labels ls
						<< Ls.parse <| sprintf (
							"if * then\n"
							^^ "   skip\n"
							^^ "else if %s < %n then\n"
							^^ "   %s := *;\n" 		
							^^ "   %s[%s] := %n;\n" 
							^^ "   %s := %s + 1;\n"		
							^^ "   %s\n"                
							^^ "else skip\n"
							^^ "fi fi" )
							size bound
							pidx
							procq pidx (List.assoc proc proc_ids)
							size size
							( match ts with [] -> "" 
							  | _ -> Ls.to_string arg_assigns )
				end
		  end

		| _ -> [s]
	in
	
	Program.translate
		~add_global_decls: (dispatch_proc_decl :: new_global_decls)

		~add_local_decls: 
			(fun (n,_) -> 
				if List.mem n (Tr.main_proc :: Tr.aux_procs) then []
				else new_local_decls)
		
		~proc_body_prefix: 
			(fun (n,_) -> 
				if n = Tr.init_proc then
					[ Ls.assume init_predicate;
					  [E.ident size] |:=| [E.num 0]
					]
				else [] )
		
		~per_stmt_map: 
			(fun n -> insert_dispatch_loop n <=< post_to_buffer)
	<| p
		
			  
(** An encoding of the FiFo-ordered task-scheduling semantics restricted so 
 * that at most [k] pending tasks exist at any moment. *)
let post_to_call_bounded_fifo k p =
	let size = k in

	let first = sprintf "__%s__first" stage_id
	and last = sprintf "__%s__last" stage_id
	and final = sprintf "__%s__final" stage_id
	and procq = sprintf "__%s__proc" stage_id
	and argqi = sprintf "__%s__argq%n_i" stage_id
	and argqb = sprintf "__%s__argq%n_b" stage_id
	and qsize = sprintf "__%s__size" stage_id
	and proc = sprintf "__%s__p" stage_id
	and argi = sprintf "__%s__arg%n_i" stage_id
	and argb = sprintf "__%s__arg%n_b" stage_id
	and dispatch_proc = sprintf "__%s__dispatch" stage_id
	in

	let proc_ids =
		List.mapi (fun i pr -> D.name pr, i+1) 
		<< List.filter 
			( not 
			  << (flip List.mem) 
					(Tr.main_proc :: Tr.real_main_proc :: Tr.aux_procs)
			  << D.name )
		<| Program.procs p
	in
	
	let max_args =
		List.fold_left max 0
		<< List.map ( List.length << fst << Procedure.signature )
		<< List.map (function D.Proc (_,_,p) -> p | _ -> failwith "!")
		<| Program.procs p
	in

	let new_global_decls =
		List.map Declaration.parse [
			sprintf "const %s: int" qsize;
			sprintf "var %s: int" first;
			sprintf "var %s: int" last;
			sprintf "var %s: bool" final;
			sprintf "var %s: [int] int" procq
		]
			
		@ ( List.flatten <| List.make
					(fun i -> 
						List.map Declaration.parse [
							sprintf "var %s: [int] bool" (argqb (i+1));
							sprintf "var %s: [int] int" (argqi (i+1))
						]
					) max_args )
	in

	let dispatch_stmt =
		Ls.case
		<< List.map
			(fun (n,pid) ->
				 match Option.map Procedure.signature
					 (Program.find_proc p n)
				 with
				 | None -> failwith
					   <| sprintf "Cannot resolve procedure `%s'." proc

				 | Some (ts,_) ->
					   let args, sels =
						   List.split
						   << List.mapi
							   ( fun i t -> match t with
								 | Type.Bool ->
									   E.ident (argb (i+1)),
									   E.sel (E.ident <| argqb (i+1)) [E.ident first]
								 | Type.Int ->
									   E.ident (argi (i+1)),
									   E.sel (E.ident <| argqi (i+1)) [E.ident first]
								 | _ -> failwith "Unexpected type." )
						   <| ts
					   in

					   E.ident proc |=| E.num pid,
					   ( match args with [] -> id | _ -> List.cons (args |:=| sels) )
						   [ [ E.ident first ] |:=| [ E.ident first |+| E.num 1 ] ;
							 Ls.stmt (S.Call (n, args, [])) ]
			)
		<| proc_ids
	in
	
	let init_predicate = (E.ident qsize |=| E.num size) in
	
	let dispatch_proc_decl =
		Declaration.parse
		<| sprintf (
			"proc %s () : void begin\n"
			^^ "   var %s: int\n"			(* proc *)
			^^ "   %s\n"
			^^ "   %s\n"
			^^ "   while %s >= %s do\n"		(* last, first *)
			^^ "      %s := %s[%s];\n"		(* proc, procq, first *)
			^^ "      %s\n"					(* dispatch_stmt *)
			^^ "   done\n"
			^^ "end" )
			dispatch_proc
			proc
			( String.map_whole (sprintf "var %s: bool")
			  << String.concat ", "
			  << List.make (fun i -> argb (i+1))
			  <| max_args ) 
			( String.map_whole (sprintf "var %s: int")
			  << String.concat ", "
			  << List.make (fun i -> argi (i+1))
			  <| max_args ) 
			last first
			proc procq first
			(Ls.to_string dispatch_stmt)
	in
	
	let insert_dispatch_loop n s =
		match s with
		| Ls.S (ls, S.Return es) when n = Tr.real_main_proc ->
			Ls.add_labels ls [
				Ls.call dispatch_proc [] [];
				Ls.return es
			]
		| _ -> [s]

	and post_to_buffer s =
		match s with
		| Ls.S (_, S.Post (_,_,Some _)) ->
			  failwith "Unexpected inter-node post."

		| Ls.S (ls,S.Post (proc,es,None)) -> begin

			  match Option.map Procedure.signature (Program.find_proc p proc)
			  with
			  | None -> failwith
					<| sprintf "Cannot resolve procedure `%s'." proc
			  | Some (ts,_) -> begin
					let arg_assigns =
						(flip (|:=|)) es
						<< List.mapi
							( fun i t -> match t with
							  | Type.Bool ->
									E.sel (E.ident <| argqb (i+1)) [E.ident last]
							  | Type.Int ->
									E.sel (E.ident <| argqi (i+1)) [E.ident last]
							  | _ -> failwith "Unexpected type." )
						<| ts
					in
					
					Ls.add_labels ls
					<< Ls.parse <| sprintf (
						"if %s then\n"
						^^ "	skip\n"
						^^ "else if (%s-%s+1) < %s then\n"
						^^ "	%s := %s + 1;\n"
						^^ "	%s[%s] := %n;\n"
						^^ "	%s\n"
						^^ "else %s := true\n"
						^^ "fi fi" )
						final 
						last first qsize
						last last
						procq last (List.assoc proc proc_ids)
						(match ts with [] -> "" | _ -> Ls.to_string arg_assigns)
						final
				end
		  end

		| _ -> [s]
	in
	
	Program.translate
		~add_global_decls: (dispatch_proc_decl :: new_global_decls)		

		~proc_body_prefix: 
			(fun (n,_) -> 
				if n = Tr.init_proc then
					[ Ls.assume init_predicate;
					  [E.ident final] |:=| [E.bool false];
					  [E.ident first; E.ident last] |:=| [E.num 0; E.num (-1)] 
					]
				else [] )
		
		~per_stmt_map: 
			(fun n -> insert_dispatch_loop n <=< post_to_buffer)
	<| p
		
