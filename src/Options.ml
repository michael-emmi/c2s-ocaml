(** Options for controlling the top-levle program behavior. *)

open Prelude
open Printf
open PrettyPrinting

type value =
	| Bool of bool
	| Int of int
	| String of string
	| File of string
	
type tag = string

type flag_spec = 
	(* flag name *)
	string * 
	(* default values *)
	value list * 
	(* description *)
	string * 
	(* classification tags *)
	tag list * 
	(* static/mutable *)
	bool
	
type flag_val = string * value list
	
let has_tag t (_,_,_,tags,_) = List.mem t tags
let name (n,_,_,_,_) = n
let desc (_,_,d,_,_) = d
let args (_,ax,_,_,_) = ax
let is_static (_,_,_,_,b) = b

let to_string (n,ax,_,_,_) =
	sprintf "%s(%s)" n
	<< String.concat "," 
	<< List.map (
		function 
		| Bool _ -> "bool"
		| Int _ -> "int"
		| String _ -> "string"
		| File _ -> "file" )
	<| ax

let get_bool i (_,ax) = 
	match List.nth ax (i-1) with 
	| Bool b -> b 
	| _ -> failwith "get_bool: expected Boolean argument."
	
let get_int i (_,ax) = 
	match List.nth ax (i-1) with 
	| Int i -> i 
	| _ -> failwith "get_int: expected integer argument."
	
let get_string i (_,ax) = 
	match List.nth ax (i-1) with 
	| String s -> s
	| _ -> failwith "get_string: expected string argument."
	
let get_file i (_,ax) =
	match List.nth ax (i-1) with 
	| File s -> s
	| _ -> failwith "get_file: expected string argument."

let globals : (flag_val list) ref = ref []

let flag_re prefix f = 
	Str.regexp 
	<< sprintf "^%s%s%s$" prefix (name f) 
	<< String.concat ""
	<< List.map 
		(fun a -> match a with
			| Bool b -> " \\(true\\|false\\)"
			| Int i -> " \\([0-9]+\\)"
			| String s -> " \\([^\"]*\\)"
			| File f -> " \\([^ ]*\\)" )
	<| args f

let mem_assoc fn = List.exists ((=) fn << name)
let assoc fn = List.first ((=) fn << name)

let global_bool fn = get_bool 1 ((), List.assoc fn !globals)	
let global_int fn = get_int 1 ((), List.assoc fn !globals)

let supported_sources = [
	"Concurrent Programs", "cp";
	"Boolean Programs", "bp";
	"Boogie Programs", "bpl";
	"Petri nets", "spec";
	"Context-free grammars", "cfg";
]

let flags : flag_spec list = [
	"cp-to-bpl", [],
	"Translate Concurrent program to Boogie program.",
	["language"],
	false;
	
	"cp-to-bp", [],
	"Translate Concurrent program to Boolean program.",
	["language"],
	false;
	
	"pn-to-bpl", [Int 0],
	"Translate Petri net to Boogie program.",
	["language"],
	false;
	
	"cfg-to-presburger", [],
	"Translate a Context-free grammar into a Presburger formula (in Boogie syntax)\n"
    ^ "  which encodes the Parikh image of the grammar.",
	["language"; "violin"],
	false;
	
	"violin-instrument", [Int 0],
	"New secret Violin option.",
	["violin"],
	false;
	
	"print", [File "-"],
	"Print out the program.",
	["basic"],
	false;
	
	"flags", [File "-"],
	"Include additional flags from a file.",
	["basic"],
	false;

	"prepare-for-back-end", [],
	"",
	["low-level"],
	false;
	
	"delay-bounding", [Int 1; Int 0],
	"Delay-bounded sequentialization.",
	["async"],
	false;
	
	"esc-async", [],
	"Translation for deductive verification.",
	["async"],
	false;
	
	"post-to-call-dfs", [Int 0],
	"Encode async. calls as sync. calls w/ DFS scheduler.",
	["async"],
	false;
	
	"post-to-call-bounded-bag", [Int 0],
	"Encode async. calls as counter increments with bounded counters.",
	["async"],
	false;
	
	"post-to-call-bfs", [Int 0; Int 0; Bool false],
	"Encode async. calls as sync. calls w/ BFS scheduler.",
	["async"],
	false;
	
	"post-to-call-bounded-fifo", [Int 0],
	"Encode async. calls as enqueues to a bounded queue.",
	["async"],
	false;
	
	"yield-to-skip", [],
	"Translate yield statements to no-ops.",
	["async"],
	false;
	
	"lal-reps", [Int 0],
	"Translate yield statements to nondeterminstic updates.",
	["async"],
	false;
	
	"multi-to-single", [],
	"Translate multi-processor program to single-processor.",
	["async"],
	false;
	
	"asserts-to-err-flag", [],
	"Translate assertions to raise an error flag.",
	[""],
	false;
	
	"ensure-well-formed", [],
	"???",
	["low-level"],
	false;
	
	"help", [],
	"Print out the usage message.",
	["basic"],
	false;

	"order-decls", [],
	"Order program declarations.",
	["low-level"],
	false;

	"reach-slic-error", [Bool false],
	"Reachability to SLIC_ERROR in the target program",
	["low-level"],
	false;

	"one-label-per-stmt", [Bool false],
	"Only one label per statement in the target program",
	["low-level"],
	false;

	"one-target-per-goto", [Bool false],
	"Only one target label per goto statement in the target program",
 	["low-level"],
	false;

	"no-ignore-returns", [Bool false],
	"Ensure calls assign from all return values in the target program",
	["low-level"; "Boogie"],
	false;

	"no-dead-stmts", [Bool false],
	"Do not allow `dead' statements in the target program",
	["low-level"; "Boogie"],
	false;

	"no-constrain-clauses", [Bool false],
	"Do not allow `constrain' clauses in the target program",
	["low-level"],
	false;
	
	"back-end", [String "Boogie"],
	"Which tool to generate code for?",
	["low-level"; "tools"],
	false;
	
	"boogie-version", [String "2011-08-03"],
	"Which version of Boogie will be used?",
	["low-level"; "Boogie"],
	true;

	"only-boolean", [Bool false],
	"Only allow Boolean variables in the target program",
	["low-level"; "Slam"],
	false;
	
]
	
let usage_message _ = 
	
	let print_arg a =
		match a with
		| Bool b -> text "bool" <-> colon <-> bool b
		| Int i -> text "int" <-> colon <-> int i
		| String s -> text "string" <-> colon <-> double_quotes (text s)
		| File f -> text "file" <-> colon <-> text f
	in
	
	let print_flag f = 
		indent 2 (
			text ("--" ^ name f)
			<+> (sep << List.map (angles << print_arg) <| args f) )
		$+$ (indent 2 << text <| desc f)
	in
	
	let print_group name g =
		text name
		$+$ ( vcat
			  << punctuate (empty $+$ empty)
			  << List.map print_flag
			  <| List.filter (has_tag g) flags )
	in
	
	text "c2s: Concurrent to Sequential Translations"
	$+$ text "usage:" 
	<+> text (Filename.basename Sys.argv.(0))
	<+> text "<source> <flags>"
	$+$ empty
	$+$ text "where <source> should be either:"
	$+$ ( indent 4 << vcat 
		 << List.map text
		 << List.map (fun (n,ext) -> sprintf "*.%s – %s" ext n)
		 <| supported_sources )
	$+$ empty
	$+$ text "and where <flags> include the following:"
	$+$ empty
	$+$ print_group "Basic usage flags" "basic"
	$+$ empty
	$+$ print_group "Language translation flags" "language"
	$+$ empty
	$+$ print_group "Async translation flags" "async"
	$+$ empty
	$+$ empty
	

let usage_and_exit _ =	
	output_string stdout << render <| usage_message ();
	exit 0
	
let read_flag f op =
	if Str.string_match (flag_re "" f) op 0 then
		name f,
		List.mapi (fun i a -> 
			match a with
			| Bool _ -> Bool (bool_of_string <| Str.matched_group (i+1) op)
			| Int _ -> Int (int_of_string <| Str.matched_group (i+1) op)
			| String _ -> String (Str.matched_group (i+1) op) 
			| File _ -> File (Str.matched_group (i+1) op) ) 
		<| args f
	else begin
		printf "Invalid flag arguments: `%s'; " op;
		printf "correct type is `%s'.\n" (to_string f);
		printf "See --help for usage.\n";
		usage_and_exit ()
	end

let find_flag name = assoc name flags

let flags_file_skip_line_re = Str.regexp "^\\(#.*\\|\\)$"

let read_flags_file f =
	if not (Sys.file_exists f) then
		failwith (sprintf "Cannot find file `%s'." f);
	let lines = ref [] in
	let chan = open_in f in
	try while true; do
		let line = input_line chan in
		if not (Str.string_match flags_file_skip_line_re line 0) then
			lines := line :: !lines
		done; []
	with End_of_file ->
		close_in chan;
	  	List.rev !lines

let filename_re = Str.regexp "^[^-][^-].*\\.\\(cp\\|bp\\|bpl\\|spec\\|cfg\\)$"
let argument_re = Str.regexp "^--\\(.*\\)$"
let flagsfile_re = Str.regexp "^flags \\(.*\\)$"
		
let parse_cmdline _ =
	
	if Array.length Sys.argv < 2
	or not (Str.string_match filename_re (Sys.argv.(1)) 0) 
	then begin
		printf "Must specify a source file.\n";
		output_string stdout << render <| usage_message ();
		exit 0
	end;
		
	Sys.argv.(1),
	Option.cat
	<< List.map (fun opt -> 
		let op_name = String.until opt ' ' in
		match find_flag op_name with
		| None -> 
			printf "Unexpected flag: `%s'.\n" op_name;
			usage_and_exit ()
			
		| Some ("help",_,_,_,_) ->
			usage_and_exit ()
			
		| Some f -> 
			let fv = read_flag f opt in
			if is_static f then begin
				globals := fv :: (!globals);
				None
			end
			else Some fv
		)
	<< List.flatten
	<< List.map (fun opt -> 
		if Str.string_match flagsfile_re opt 0 then
			read_flags_file (Str.matched_group 1 opt)
		else [ opt ]
		)
	<< List.rev 
	<< List.fold_left (fun opts arg -> 	
		if Str.string_match argument_re arg 0 then
			(Str.matched_group 1 arg) :: opts			
		else begin match opts with
		| [] -> failwith <| sprintf "Unexpected argument parameter `%s'." arg
		| op::opts -> (op ^ " " ^ arg) :: opts
		end )
		[]	
	<< List.tl
	<< List.tl
	<< Array.to_list
	<| Sys.argv

let _ =
	globals := 
		( List.map (fun f -> name f, args f)
		  <| List.filter is_static flags )
