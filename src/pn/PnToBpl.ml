open Prelude
open Printf
open BplUtils.Operators
open BplUtils.Abbreviations

module Pn = PnAst

let pl_t = T.T ("place",[])
let tx_t = T.T ("transition",[])
let mark_t = T.T ("marking",[])

let init_constraint (v,r,i) =
	let e = E.Sel (E.Sel (E.ident "out", [E.ident "init"]), [E.ident v]) in
	match r with
	| Pn.Constraint.GE -> e |>=| E.num i
	| Pn.Constraint.EQ -> e |=| E.num i
	
let target_guard (v,i) =
	E.Sel (E.ident "target", [E.ident v]) |>=| E.num i

let rule vars idx (guards,updates) =
	
	let have v = 
		if List.mem_assoc v guards then List.assoc v guards else 0

	and net v =
		match List.first ((=) v << Tup3.fst) updates with
		| Some (_,Pn.Update.PLUS,i) -> i
		| Some (_,Pn.Update.MINUS,i) -> -i
		| _ -> 0 in
		
	let input v = 
		assert (net v >= 0 || have v = -(net v));
		E.Sel (E.Sel (E.ident "in", [E.ident idx]), [E.ident v])
		|=| E.num (have v)
		
	and output v = 
		E.Sel (E.Sel (E.ident "out", [E.ident idx]), [E.ident v])
		|=| E.num (if net v > 0 then net v else 0)
		
	in (fun e -> D.Axiom ([],e))
	<< E.conj 
	<< List.append (List.map input vars)
	<| List.map output vars

let program k (vars,rules,init,target) =
	let txs = List.mapi (fun i _ -> sprintf "t%n" (i+1)) rules in
	
	Pg.parse "
		type place;
		type transition;
		type marking = [place] int;
		const empty: marking;
		const target: marking;
		const unique init: transition;
		const in: [transition] marking;
		const out: [transition] marking;
		const K: int;
		
		axiom (forall p: place :: empty[p] == 0);

		function valid ( m: marking ) returns (bool) 
			{ (forall p: place :: m[p] >= 0) }

		function covers ( m1: marking, m2: marking ) returns (bool)
			{ (forall p: place :: m1[p] <= m2[p]) }

		function sum ( m1: marking, m2: marking ) returns (marking);
		function sub ( m1: marking, m2: marking ) returns (marking);
		axiom (forall m1, m2: marking, p: place :: sum(m1,m2)[p] == m1[p] + m2[p]);
		axiom (forall m1, m2: marking, p: place :: sub(m1,m2)[p] == m1[p] - m2[p]);		
	"
	
	@ ( if k < 0 then []
		else [ D.Axiom ([],E.ident "K" |=| E.num k) ] )
	
	(* The list of variables *)
	@ List.map (fun v -> 
		D.Const ([], true, v, pl_t, ()))
		vars
	@ [ D.Axiom ([], E.forall ["p",pl_t]
		(E.disj <| List.map (fun v -> E.ident "p" |=| E.ident v) vars)) ]

	(* The list of transitions *)
	@ (List.map (fun t -> D.Const ([], true, t, tx_t, ())) txs)
	@ [ D.Axiom ([], E.forall ["t",tx_t]
		(E.disj 
			<< List.cons (E.ident "t" |=| E.ident "init")
			<| List.map (fun t -> E.ident "t" |=| E.ident t) txs)) ] 
			
	(* The initial constraint*)
	@ [ D.Axiom ([], E.conj <| List.map init_constraint init) ]		
	
	(* The rules *)
	@ (List.mapi (fun i -> rule vars (sprintf "t%n" (i+1))) rules)
	
	(* The target *)
	@ [ D.Axiom ([], E.conj <| List.map target_guard target) ]
	
	@ [ D.Func ([], "size", [], [ Some "m", mark_t ], (None, T.Int), 
		Some (E.sum <| List.map (fun v -> E.Sel (E.ident "m", [E.ident v])) vars) ) ]
		
	@ Pg.parse "
		procedure {:inline 1} P ( t0: transition, p0: marking ) returns ( purse: marking )
		{
			var frame, frame_, purse_: marking;
			var t: transition;

			var p: marking;

			frame := out[t0];
			purse := p0;

			assert !covers( target, sum(frame,purse) );

			while (*) 
				invariant size(frame) <= size(out[t0]);
				invariant size(purse) <= K;
			{
				havoc t;
				havoc p;

				// pick a transition, and a purse to pass
				assume t != init;
				assume valid(p) && size(p) <= K;

				// take [frame_] from [frame] and [purse_] from [purse], 
				// and allow some tokens to be lost
				havoc frame_;
				havoc purse_;
				assume valid(frame_) && covers( frame_, frame );
				assume valid(purse_) && covers( purse_, purse );		
				frame := sub(frame,frame_);
				purse := sub(purse,purse_);

				// ensure frame_+purse_ covers both in[t] and [p]
				assume covers( in[t], sum( frame_, purse_ ) );		
				assume covers( p, sub( sum( frame_, purse_ ), in[t] ));

				// fire the transition
				call p := P(t,p);

				// allow some tokens to be lost
				havoc purse_;
				assume valid(purse_) && size(purse_) <= K;
				assume covers( purse_, sum(purse,p) ) ;
				purse := purse_;
			}
		}

		procedure Main ()
		{
			var p: marking;
			call p := P( init, empty );
			return;
		}
	"