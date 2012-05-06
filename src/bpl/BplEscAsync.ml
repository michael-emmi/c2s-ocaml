open Prelude
open PrettyPrinting
open Printf
open BplUtils

let async_to_seq p =
	
	(* ToDo List
	 * • Add a main dispatch loop, with an invariant taken from the axioms
	 *   annotated with "dispatch"; only the async-called procedures need
	 *   be dispatched.
	 * • Add counters to the program to encode the task buffer..
	 * • Generate the async postconditions by supposing every procedure not
	 *   marked in the "posts" clause is not posted (and check this), and
	 *   translate the "pending(..)" predicates to counter expressions.
	*)
	
	p