(** Options for controlling the top-levle program behavior. *)

open Prelude
open Printf

type key = string
type value =
	| Bool of bool
	| Int of int
	| String of string

type t = (key, value) Hashtbl.t

let options = Hashtbl.create 10

let set = Hashtbl.replace options
	
let set_bool k b = set k (Bool b)
let set_int k i = set k (Int i)
let set_string k s = set k (String s)

let get k =
	try Some (Hashtbl.find options k)
	with Not_found -> None

let get_bool op =
    match get op with
    | Some (Bool b) -> b
    | _ -> failwith <| sprintf "bool option %s is not defined" op

let get_int op =
    match get op with
    | Some (Int i) -> i
    | _ -> failwith <| sprintf "int option %s is not defined" op

let get_string op =
    match get op with
    | Some (String s) -> s
    | _ -> failwith <| sprintf "string option %s is not defined" op

let get_regexp op =
    match get op with
    | Some (String s) -> Str.regexp ("^" ^ s ^ "$")
    | _ -> failwith <| sprintf "regexp option %s is not defined" op


(* The spec entires are
   1. the key name
   2. the default value
   3. the choices, when applicable
   4. the description
   5. whether the spec can be set on the command line.
*)
type spec_type = string * value * string list * string * bool
			  
let full_speclist = [
	
	(*** High-level flags, which control other flags. ***)
	"back-end", String "none", ["none"; "moped"; "getafix"; "boogie"],
	"Which tool to generate code for?",
	true;

	"target-language", String "cp", ["bp"; "bpl"; "cp"],
	"Generate code in which language?",
	true;
	
	"task-scheduler", String "bfs",
	["bag"; "bounded-bag"; "dfs"; "bfs"; "bounded-fifo"],
	"The task buffer scheduler.",
	true;

	"boogie-version", String "2011-08-03", ["2011-08-03"; "2010-07-13"],
	"Which version of Boogie will be used?",
	true;

	"only-boolean", Bool false, [],
	"Only allow Boolean variables in the target program",
	true;
	
	(*** Low-level translation flags ***)

	"multi-to-single", Bool false, [],
	"Translate the multi-processor program to a single-processor program",
	true;
	
	"fewer-phases", Bool false, [],
	"Use fewer phases in the phase-bounded FiFo translation",
	true;
	
	"asserts-throw-exn", Bool false, [],
	"Add exceptional control flow to halt simulation when asserts fail",
	true;
	
	"ensure-well-formed", Bool true, [],
	"Ensure the input program is well formed",
	false;

	(*** Bounds ***)
	
	"interleaving-bound", Int 0, [],
	"Bound on intra-buffer task interleaving",
	true;
	
	"delay-bound", Int 0, [],
	"Number of delays for inter-cpu execution",
	true;

	"phase-bound", Int 0, [],
	"How deep to explore the post hierarchy?",
	true;

	"buffer-size", Int 0, [],
	"Bound on size of task buffers",
	true;

	"recursion-depth", Int 10, [],
	"How deep to explore recursive procedures?",
	true;

	"loop-depth", Int 10, [],
	"How deep to explore loops?",
	true;

	(*** flags for the target boolean program ***)

	"reach-slic-error", Bool false, [],
	"Reachability to SLIC_ERROR in the target program",
	true;

	"one-label-per-stmt", Bool false, [],
	"Only one label per statement in the target program",
	true;

	"one-target-per-goto", Bool false, [],
	"Only one target label per goto statement in the target program",
	true;

	"no-ignore-returns", Bool false, [],
	"Ensure calls assign from all return values in the target program",
	true;

	"no-dead-stmts", Bool false, [],
	"Do not allow `dead' statements in the target program",
	true;

	"no-constrain-clauses", Bool false, [],
	"Do not allow `constrain' clauses in the target program",
	true;
]

let speclist =
	let print_flag = sprintf "-%s"
	and print_choices = function
		| [] -> ""
		| cs -> sprintf "; either %s"
			  << String.concat ", "
			  <| List.map (sprintf "%S") cs
	in
	Arg.align
	<< List.map
		(fun (key,default,alts,desc,_) ->
			 match default with
			 | String d ->
				   print_flag key,
				   Arg.String (set_string key),
				   sprintf " %s%s%s" desc
					   (print_choices alts)
					   (match d with
						| "" -> ""
						|  _ -> sprintf "; default %S" d)
					   
			 | Int d -> 
				   print_flag key,
				   Arg.Int (set_int key),
				   sprintf " %s%s%s" desc
					   (print_choices alts)
					   (sprintf "; default %d" d)

			 | Bool d -> 
				   print_flag key,
				   Arg.Bool (set_bool key),
				   sprintf " %s%s%s" desc
					   (print_choices alts)
					   (sprintf "; default %B" d) )
	<< List.filter (fun (_,_,_,_,b) -> b)
	<| full_speclist

let _ =
	List.iter
		(fun (key,d,_,_,_) -> Hashtbl.replace options key d)
		full_speclist
	
let usage_msg =
	sprintf "usage: %s <source-file>\noptions:" Sys.executable_name

let read_options _ = 
	Arg.parse speclist (set_string "src_file") usage_msg;
	if get "src_file" = None then begin
		Arg.usage speclist usage_msg;
		exit (-1);
	end

