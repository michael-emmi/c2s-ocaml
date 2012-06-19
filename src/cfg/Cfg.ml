open Prelude
open Printf

let indent_size = 3

module Symbol = struct
	type t = string
	
	open PrettyPrinting
	let print = text
	let print_seq = sep << List.map print
end

module Production = struct
	type t = Symbol.t * Symbol.t list
	
	let lhs (x,_) = x
	let rhs (_,w) = w
	
	let consumes x = (=) x << lhs
	let produces x = List.mem x << rhs
	let produced s = List.length << List.filter ((=) s) << rhs
	
	open PrettyPrinting
	let print (x,ys) = 
		Symbol.print x <+> rarrow <+> Symbol.print_seq ys <+> semi
end

module Grammar = struct
	type t = Symbol.t list * Symbol.t list * Production.t list * Symbol.t
	
	let alphabet (ss,_,_,_) = ss
	let variables (_,xs,_,_) = xs
	let rules (_,_,ps,_) = ps
	let start (_,_,_,s) = s
	
	let rules_from x = List.filter (fun (y,_) -> x = y) << rules
	
	open PrettyPrinting
	let print (ss,xs,ps,s) =
		keyword "alphabet" <+> Symbol.print_seq ss
		$+$ keyword "variables" <+> Symbol.print_seq xs
		$+$ keyword "rules"
		$+$ (indent indent_size << vcat <| List.map Production.print ps)
		$+$ keyword "start" <+> Symbol.print s
		$+$ empty	
end