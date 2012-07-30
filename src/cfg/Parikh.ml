open Prelude
open Printf

open BplUtils
open BplUtils.Operators

module P = Cfg.Production
module G = Cfg.Grammar

(* My attempt at implementing the grammar to Presburger formula 
 * 	construction in Sectoin 
 *
 *	K. N. Verma, H. Seidl, and T. Schwentick.  
 *	On the Complexity of Equational Horn Clauses.
 *	In CADE '05: Proc. 20th International Conference on Automated Deduction,
 *	volume 3632 of LNCS, pages 337-352. Springer, 2005.
 *)

let image_of_cfg g =

	let pidx = List.mapi Tup2.ekam (G.rules g) in
	let pvar p = E.ident << sprintf "y.p%n" <| List.assoc p pidx in	
	let svar = E.ident << sprintf "x.%s" in
	let zvar = E.ident << sprintf "z.%s" in
	
	List.snoc []
	<< (fun e -> 
			D.Proc ([A.num "inline" 1], "Violin.PresburgerInvariant", (
				[], List.map (fun x -> sprintf "x.%s" x, T.Int) (G.alphabet g),
				[], [], [], [ Ls.stmt (S.Assert ([],e)) ]
			)))
	<< E.exists (
		List.map (fun p -> sprintf "y.p%n" (List.assoc p pidx), T.Int) (G.rules g)
		@ List.map (fun a -> sprintf "z.%s" a, T.Int) (G.alphabet g @ G.variables g) )
	<| E.conj (
		
		(* The number of times a variable is used is equal to the number
		   of times it is produced. *)
		List.map (fun a -> 
			E.num (if G.start g = a then 1 else 0)
			|+| ( E.sum 
				  << List.map (fun p -> E.num (P.produced a p) |*| pvar p) 
				  << List.filter (P.produces a)
				  <| G.rules g)
			|-| ( E.sum 
				  << List.map pvar 
				  << List.filter (P.consumes a) 
				  <| G.rules g)
			|=| E.num 0 )
		(G.variables g)
		
		(* The number of each symbol is the number of times each production
		   is taken, times the number of RHS ocurrences of that symbol in
		   a given production. *)
		@ List.map (fun a ->
			svar a 
			|=| ( E.sum
				  << List.map (fun p -> E.num (P.produced a p) |*| pvar p)
				  << List.filter (P.produces a)
				  <| G.rules g ) ) 
			(G.alphabet g)			
			
		(* The start variable is reachable in 0 steps. *)
		@ [ zvar (G.start g) |=| E.num 0 ]
			
		(* Each symbol used is reachable. *)
		@ List.map (fun a -> 
			(svar a |!=| E.num 0) |=>| (zvar a |>| E.num 0) ) 
			(G.alphabet g)
			
		(* Each variable that is reachable is reached in one more step
		   than some variable which produces it. *)
		@ List.map (fun a -> 
			(zvar a |!=| E.num 0) 
			|=>| (E.disj 
				 << List.map (fun p -> 
					let b = P.lhs p in
					if b = G.start g then E.conj [
						zvar a |=| E.num 1 ;
						pvar p |>| E.num 0 ]
					else E.conj [
						zvar a |=| (zvar b |+| E.num 1) ;
						pvar p |>| E.num 0 ;
						zvar b |>| E.num 0 ] )
				 << List.filter (P.produces a) 
				 <| G.rules g ))
			(G.alphabet g @ G.variables g)
			
		(* The LHS of every used production must be reachable. *)
		@ List.map (fun p -> 
			if P.lhs p <> G.start g then 
				(pvar p |>| E.num 0) |=>| (zvar (P.lhs p) |>| E.num 0)
			else E.bool true )
			(G.rules g)
			
		(* All values are non-negative. *)
		@ List.map (fun a -> svar a |>=| E.num 0) (G.alphabet g)
		@ List.map (fun a -> zvar a |>=| E.num 0) (G.alphabet g @ G.variables g)
		@ List.map (fun p -> pvar p |>=| E.num 0) (G.rules g)
	)

	

	