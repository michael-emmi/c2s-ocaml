
open Prelude
open Printf

let indent_size = 3

module Identifier = struct
	type t = string
	
	open PrettyPrinting
	let print = text
end

module Guard = struct
	type t = Identifier.t * int
	
	open PrettyPrinting
	let print (x,i) = Identifier.print x <+> oper ">=" <+> int i
end

module Constraint = struct
	type reln = GE | EQ
	type t = Identifier.t * reln * int

	open PrettyPrinting
	let print (x,r,i) =
		Identifier.print x 
		<+> oper (match r with GE -> ">=" | EQ -> "=") <+> int i
end

module Update = struct
	type sign = PLUS | MINUS
	type t = Identifier.t * sign * int
	
	open PrettyPrinting
	let print (x,s,i) = 
		Identifier.print (x^"'") <+> oper "=" 
		<+> Identifier.print x 
		<+> oper (match s with PLUS -> "+" | MINUS -> "-") <+> int i
end

module Rule = struct
	type t = Guard.t list * Update.t list
	
	open PrettyPrinting
	let print (gs,us) = 
		(sep << punctuate comma <| List.map Guard.print gs)
		<+> oper "->"
		$+$ (indent indent_size << vcat << punctuate comma <| List.map Update.print us)
		<+> semi
end

module Program = struct
	type t = Identifier.t list * Rule.t list * Constraint.t list * Guard.t list
	
	open PrettyPrinting
	let print (vars,rules,init,target) =
		keyword "vars" 
		$+$ (indent indent_size << sep <| List.map Identifier.print vars)
		$+$ empty
		$+$ keyword "rules" 
		$+$ (indent indent_size << vcat <| List.map Rule.print rules)
		$+$ keyword "init" 
		$+$ (indent indent_size << sep << punctuate comma <| List.map Constraint.print init)
		$+$ empty
		$+$ keyword "target"
		$+$ (indent indent_size << sep << punctuate comma <| List.map Guard.print target)
		$+$ empty
end