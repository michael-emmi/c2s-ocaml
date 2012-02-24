{
	(** Lexer for Boolean programs. *)
	open BpParser
	open Prelude
	exception Eof

	let keyword_tbl = Hashtbl.create 50
	let _ =
		List.iter (fun (k,v) -> Hashtbl.add keyword_tbl k v) [
			(* "keyword", (fun l -> KEY_WORD l); *)
			"if", const IF;
			"then", const THEN;
			"else", const ELSE;
			"fi", const FI;
			"while", const WHILE;
			"do", const DO;
			"od", const OD;
			"assume", const ASSUME;
			"assert", const ASSERT;
			"goto", const GOTO;
			"return", const RETURN;
			"skip", const SKIP;
			"decl", const DECL;
			"enforce", const ENFORCE;
			"begin", const BEGIN;
			"end", const END;
			"print", const PRINT;
			"void", const VOID;
			"bool", const BOOL;
			"schoose", const SCHOOSE;
			"constrain", const CONSTRAIN;
			"dead", const DEAD;
		]

	let unescape_quotes s =
		Str.global_replace (Str.regexp "[\\][\']") "'"
			(Str.global_replace (Str.regexp "[\\][\"]") "\"" s)

}

let source_char = _
let white = ['\t' ' ' '\012' '\r']
let line_term = ['\n']

let unicode_letter = ['a'-'z' 'A'-'Z']
let unicode_digit = ['0'-'9']

(* let unicode_escape_sequence = [ ] *)
(* let unicode_combining_mark = [ ] *)
(* let unicode_connector_punctuation = [ ] *)

let symbol_start = unicode_letter | '$' | '_' | '\'' 
(* | unicode_escape_sequence *)

let symbol_part = symbol_start | unicode_digit
  (* | unicode_combining_mark *)
  (* | unicode_connector_punctuation *)
  (* | unicode_escape_sequence *)
  
let symbol_name = symbol_start symbol_part*

let decimal_digit = ['0'-'9']
let signed_integer = ['+' '-']? decimal_digit+
let exponent_part = ['e' 'E'] signed_integer

let decimal_integer_lit = '0' | ['1'-'9'] decimal_digit*

let decimal_lit =
  decimal_integer_lit '.' decimal_digit* exponent_part?
  | '.' decimal_digit+ exponent_part?
  | decimal_integer_lit exponent_part?

let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_integer_lit = "0x" hex_digit+

let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hex_digit hex_digit
  
let double_string_char = [^ '"' '\n'] | escape
let single_string_char = [^ '\'' '\n'] | escape

(*
Using ' as a part of the symbol for primed variables
let string_lit =
  '"' double_string_char* '"'
  | '\'' single_string_char* '\''
*)

let null_lit = "null"
let boolean_lit = "true" | "false"
let numeric_lit = decimal_lit | hex_integer_lit


let line_comment = "//" [^ '!' '\n']* '\n'

let comment_start = "/*"
let comment_end = "*/"

rule token = parse
  | white          { token lexbuf }
  | line_term      { Lexing.new_line lexbuf; token lexbuf }

  | line_comment   { Lexing.new_line lexbuf; token lexbuf }
  | comment_start  { ignore (comment lexbuf); token lexbuf }

(*   | null_lit { NULL } *)
(*   | boolean_lit as lxm { BOOL_VAL (bool_of_string lxm) } *)
(*   | decimal_integer_lit as lxm { INT_VAL (int_of_string lxm) } *)
(*   | hex_integer_lit as lxm { INT_VAL (int_of_string lxm) } *)
(*   | decimal_lit as lxm { FLOAT_VAL (float_of_string lxm) } *)
(*   | string_lit as lxm { STRING_VAL (unescape_quotes (String.sub lxm 1 (String.length lxm - 2))) } *)

  | symbol_name as sym
      { (try Hashtbl.find keyword_tbl sym
	 with Not_found -> (fun l -> ID (sym,l)))
	  lexbuf.Lexing.lex_curr_p }

  | decimal_integer_lit as lxm { INT (int_of_string lxm) }

  | 'T'            { TRUE }		  
  | 'F'            { FALSE }

  | '='            { EQ }
  | "!="           { NEQ }
  | '!'            { NOT }
  | '&'            { AND }
  | '|'            { OR }
  | "=>"           { IMPLIES }

  | "?"            { CHOICE }
  | "*"            { CHOICE }

  | ":="           { ASSIGN lexbuf.Lexing.lex_curr_p }

  | '('            { LPAREN lexbuf.Lexing.lex_curr_p }
  | ')'            { RPAREN lexbuf.Lexing.lex_curr_p }
  | '['            { LBRACKET lexbuf.Lexing.lex_curr_p }
  | ']'            { RBRACKET lexbuf.Lexing.lex_curr_p }
  | '{'            { LBRACE lexbuf.Lexing.lex_curr_p }
  | '}'            { RBRACE lexbuf.Lexing.lex_curr_p }
  | '<'            { LANGLE lexbuf.Lexing.lex_curr_p }
  | '>'            { RANGLE lexbuf.Lexing.lex_curr_p }

  | '.'            { DOT lexbuf.Lexing.lex_curr_p }
  | ','            { COMMA lexbuf.Lexing.lex_curr_p }
  | ':'            { COLON lexbuf.Lexing.lex_curr_p }
  | ';'            { SEMI lexbuf.Lexing.lex_curr_p }
      
  | eof            { EOF }
  | _ as lxm       { failwith ("unexpected token: " ^ String.make 1 lxm) }

and comment = parse
  | comment_end    { () }
  | line_term      { Lexing.new_line lexbuf; comment lexbuf }
  | _              { comment lexbuf }

(* Local Variables: *)
(* compile-command: "cd .. && make -k" *)
(* End: *)
