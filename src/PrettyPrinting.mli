type doc

val char : char -> doc
val text : string -> doc
val int : int -> doc
val float : float -> doc
val bool : bool -> doc

val oper : string -> doc
val keyword : string -> doc

val semi : doc
val dot : doc
val comma : doc
val colon : doc
val tilde : doc
val bang : doc
val at : doc
val pound : doc
val dollar : doc
val percent : doc
val carat : doc
val amp : doc
val ast : doc
val under : doc
val qmark : doc
val space : doc
val equals : doc
val lparen : doc
val rparen : doc
val lbrack : doc
val rbrack : doc
val lbrace : doc
val rbrace : doc
val langle : doc
val rangle : doc
val quote : doc
val dquote : doc
val rarrow : doc

val parens : doc -> doc
val brackets : doc -> doc
val braces : doc -> doc
val angles : doc -> doc
val quotes : doc -> doc
val double_quotes : doc -> doc
val pad : int -> doc -> doc

val empty : doc
val is_empty : doc -> bool
val (<->) : doc -> doc -> doc
val (<+>) : doc -> doc -> doc
val hcat : doc list -> doc
val hsep : doc list -> doc
val ($-$) : doc -> doc -> doc
val ($+$) : doc -> doc -> doc
val vcat : doc list -> doc
val sep : doc list -> doc
val cat : doc list -> doc
val fsep : doc list -> doc
val fcat : doc list -> doc
val nest : int -> doc -> doc
val hang : doc -> int -> doc -> doc
val indent : int -> doc -> doc
val break : doc
val punctuate : doc -> doc list -> doc list

val disable_break : doc
val enable_break : doc
val no_breaks : doc -> doc
	
val render : doc -> string
val render_no_breaks : doc -> string

val list : ('a -> doc) -> (doc -> doc) -> doc -> 'a list -> doc
val option : ('a -> doc) -> 'a option -> doc
val option_with : ('a -> doc) -> doc -> 'a option -> doc
val either : ('a -> doc) -> ('b -> doc) -> ('a,'b) Prelude.either -> doc

val hashtbl : ('a -> doc) -> ('b -> doc) -> (doc -> doc) -> doc -> ('a, 'b) Hashtbl.t -> doc
