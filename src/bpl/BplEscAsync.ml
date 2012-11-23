open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Extensions
open BplUtils.Abbreviations

let async_to_seq p =
	
	(* ToDo List
	 * √ Add a main dispatch loop, with an invariant taken from the axioms
	 *   annotated with "dispatch"; only the async-called procedures need
	 *   be dispatched.
	 * √ Add counters to the program to encode the task buffer..
	 * √ Generate the async postconditions by supposing every procedure not
	 *   marked in the "posts" clause is not posted (and check this), and
	 *   translate the "pending(..)" predicates to counter expressions.
     *
	 * In particular:
	 * √ new main procedure
	 * √ new counters
	 * √ modify specifications
	*)
	
	let pendingPredToSelect = 
		function E.FnApp (f,es) when f = "pending" ->
			E.Sel (E.Id f, es)
		| e -> e
		in
	
	let asyncCalls = 
		Program.fold_stmts
		( fun ps s -> match s with 
			| (ls, S.Call (ax,p,_,_)) when A.has "async" ax ->
				List.add_uniq p ps
			| _ -> ps )
		[] p
	in
	
	let dispatchInvariants =
		Program.fold 
		( fun es d -> match d with
			| D.Axiom (ax,e) when A.has "dispatch" ax ->
				(E.map pendingPredToSelect e) :: es
			| _ -> es
		)
		[] p
	in
	
	(* Program.translate
		~add_global_decls: ( ... )
		~per_stmt_map: ( ... ) *)
	Program.translate
		~replace_global_decls: (
			function D.Axiom (ax,e) when A.has "dispatch" ax -> []
			| D.Proc (ax,n,(ts,ps,rs,es,bd)) ->
				let posts, es = 
					List.fold_left 
						( fun (ps,es) sp -> match sp with
              | Sp.Modifies (_,ax,ids) when A.has "posts" ax -> ids @ ps, es
							| e -> ps, e::es )
						([],[])
					<| es in
					
				let notPosted = 
					List.map ( fun p -> Sp.ensures ( E.parse (
							sprintf "pending[%s] == old(pending[%s])" p p)))
					<< List.filter (not << (flip List.mem <| posts))
					<| asyncCalls
					in			
					
				(* let ax = if n = "Main" then (A.num "inline" 1 :: ax) else ax in *)
										
				[ D.Proc (ax,n,(ts,ps,rs,es@notPosted,bd)) ]

			| d -> [d]
				
		)
		~new_global_decls: (
			D.parse <| sprintf (
				"type ProcId; \
				const unique %s: ProcId; \
				var pending: [ProcId] int; \
				procedure AsyncMain () { \
					ASYNC_MAIN: assume (forall p: ProcId :: pending[p] == 0); \
				    call Main (); \
				    while (*) \
					%s
					{ \
					    %s \
				    } \
					return; \
				 }" )
				( String.concat ", " asyncCalls )
				( String.concat " " 
				  << List.map (sprintf "invariant %s;" << E.to_string) 
				  <| dispatchInvariants )
				( String.concat " else " 
				  << List.map (fun p -> 
						sprintf 
							"if (*) { \
								DISPATCH_%s: assume pending[%s] > 0;
							    pending[%s] := pending[%s] - 1; \
							 	call %s(); \
							}" p p p p p
					)
				  <| asyncCalls )

		)
		~per_stmt_map: (fun n -> (
			function (ls, S.Call (ax,p,es,r)) when A.has "async" ax ->
				
				(* ToDo -- add labels back*)
				Ls.parse <| sprintf "pending[%s] := pending[%s] + 1;" p p
				
			| ls -> [ls]
		))
		~per_expr_map: (const pendingPredToSelect)
	p