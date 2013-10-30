(** Combinator-style formatted printing. *)

open Prelude
open Format
type doc = formatter -> unit

(* XXX: parameter? *)
let _margin_ = 80
let _max_indent_ = max_int

let char = flip pp_print_char
let text = flip pp_print_string
let int = flip pp_print_int
let big_int = text << Big_int.string_of_big_int
let float = flip pp_print_float
let bool = flip pp_print_bool

let oper = text
let keyword = text

let tilde = char '~'
let bang = char '!'
let at = char '@'
let pound = char '#'
let dollar = char '$'
let percent = char '%'
let carat = char '^'
let amp = char '&'
let ast = char '*'
let under = char '_'
let qmark = char '?'

let dot = char '.'
let comma = char ','
let colon = char ':'
let semi = char ';'
let space = char ' '
  
let equals = char '='
let plus = char '+'
let minus = char '-'

let lparen = char '('
let rparen = char ')'
let lbrack = char '['
let rbrack = char ']'
let lbrace = char '{'
let rbrace = char '}'
let langle = char '<'
let rangle = char '>'
let quote = char '\''
let dquote = char '\"'

let wrap (l : doc) (r : doc) (d : doc) ff =
  pp_open_hovbox ff 2;
  l ff;
  d ff;
  r ff;
  pp_close_box ff ()

let parens = wrap lparen rparen
let brackets = wrap lbrack rbrack
let braces = wrap lbrace rbrace
let quotes = wrap quote quote
let double_quotes = wrap dquote dquote
let angles = wrap langle rangle

let rarrow = text "->"

let empty : doc = const ()

let (<->) d e ff =
  pp_open_hbox ff ();
  d ff;
  e ff;
  pp_close_box ff ()

let (<+>) d e ff =
  pp_open_hbox ff ();
  d ff;
  pp_print_space ff ();
  e ff;
  pp_close_box ff ()

let hcat ds ff =
  pp_open_hbox ff ();
  List.iter (fun d -> d ff) ds;
  pp_close_box ff ()

let hsep ds ff =
  match ds with
  | [] -> ()
  | d::ds ->
      begin
	pp_open_hbox ff ();
	d ff;
	List.iter (fun d -> pp_print_space ff (); d ff) ds;
	pp_close_box ff ()
      end

let ($-$) d e ff =
  pp_open_vbox ff 0;
  d ff;
  pp_print_cut ff ();
  e ff;
  pp_close_box ff ()

let ($+$) d e ff =
  pp_open_vbox ff 0;
  d ff;
  pp_force_newline ff ();
  e ff;
  pp_close_box ff ()

let vcat ds ff =
  match ds with
  | [] -> ()
  | d::ds ->
      begin
	pp_open_vbox ff 0;
	d ff;
	List.iter (fun d -> pp_print_cut ff (); d ff) ds;
	pp_close_box ff ()
      end

let sep ds ff =
  match ds with
  | [] -> ()
  | d::ds ->
      begin
	pp_open_hovbox ff 0;
	d ff;
	List.iter (fun d -> pp_print_space ff (); d ff) ds;
	pp_close_box ff ()
      end

let cat ds ff =
  match ds with
  | [] -> ()
  | d::ds ->
      begin
	pp_open_hovbox ff 0;
	d ff;
	List.iter (fun d -> pp_print_cut ff (); d ff) ds;
	pp_close_box ff ()
      end

let fsep _ = failwith "fsep unimplemented"
let fcat _ = failwith "fcat unimplemented"

let nest n d ff =
  pp_open_hovbox ff n;
  d ff;
  pp_close_box ff ()

let hang d n e = sep [d; nest n e]

let break ff =
  pp_print_newline ff ()
  
let punctuate p ds =
  match List.rev ds with
  | [] -> []
  | d::ds -> List.rev (d :: List.map (fun d -> d <-> p) ds)

let render d =
  let b = Buffer.create 16 in
  let ff = formatter_of_buffer b in
  pp_set_margin ff _margin_;
  pp_set_max_indent ff _max_indent_;
  d ff;
  pp_print_flush ff ();
  Buffer.contents b

let disable_break ff =
	pp_print_flush ff ();
	pp_set_margin ff 100000
(* 	pp_set_max_indent ff 1; *)
(* 	let out, flush, newline, spaces = *)
(* 		pp_get_all_formatter_output_functions ff () in *)
(* 	let newline = id in *)
(* 	pp_set_all_formatter_output_functions *)
(* 		ff ~out:out ~flush:flush ~newline:newline ~spaces:spaces *)

let enable_break ff =
	pp_print_flush ff ();
(* 	pp_set_max_indent ff _max_indent_; *)
(* 	let out, flush, _, spaces = *)
(* 		pp_get_all_formatter_output_functions ff () *)
(* 	and newline = print_newline in *)
(* 	pp_set_all_formatter_output_functions *)
(* 		ff ~out:out ~flush:flush ~newline:newline ~spaces:spaces; *)
	pp_set_margin ff _margin_
(* 	pp_print_flush ff () *)

let no_breaks d ff =
	pp_set_margin ff max_int;
	pp_print_flush ff ();
	d ff;
	pp_print_flush ff ();
	pp_set_margin ff _margin_;
	pp_print_flush ff ()

let render_no_breaks d =
  let b = Buffer.create 16 in
  let ff = formatter_of_buffer b in
  pp_set_margin ff _margin_;
  pp_set_max_indent ff 0;
  let out, flush, _, spaces = pp_get_all_formatter_output_functions ff () in
(*   and newline = id in *)
  let newline _ = out " " 0 1 in
  pp_set_all_formatter_output_functions ff ~out:out ~flush:flush ~newline:newline ~spaces:spaces;
  d ff;
  pp_print_flush ff ();
  Buffer.contents b
    
let is_empty = (=) "" << render

let indent n d = if is_empty d then empty else nest (n-1) (text (String.make (n-1) ' ') <+> d)

let consolidate f d e =
    if is_empty d then e
    else if is_empty e then d
    else f d e
    
let (<+>) = consolidate (<+>)
let ($-$) = consolidate ($-$)

let pad n d =
  let p = text (String.make n ' ') in
  p <-> d <-> p

let list f surr sepp = surr << sep << punctuate sepp << List.map f
let option_with f g = Option.fold (const f) g
let option f = option_with f empty
let either f g = Either.fold (const f) (const g) ()

let hashtbl kf vf surr sepp t =
  surr
  << sep
  << punctuate sepp
  << List.rev
  <| Hashtbl.fold (fun k v ds -> (kf k <-> colon <+> vf v) :: ds) t []
