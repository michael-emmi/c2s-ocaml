(** The top-level entry point. *)

open Prelude
open Printf

let print_to_file f d =
	match f with
	| "-" -> output_string stdout << PrettyPrinting.render <| d;
	| _ -> begin
		  let oc = open_out f in
		  output_string oc << PrettyPrinting.render <| d;
		  close_out oc
	  end

type value = B of bool | I of int | S of string | F of string
type command = string * (string * value list * (value list -> BplAst.Program.t -> BplAst.Program.t))

let cmd_to_string cmd = 
  String.concat " " 
  << List.cons cmd
  << List.map (fun v -> 
      match v with B _ -> "BOOL" | I _ -> "INT" | S s -> s | F _ -> "FILE") 

let rec commands : command list = [

  "help", ("print this message", [], 
  function [] -> fun p -> 
    Printf.printf "usage: c2s [commands] : possible commands are\n";
    List.iter (fun (cmd,(desc,args,_)) -> 
      let c = cmd_to_string cmd args in
      Printf.printf "  %s %s\n" (c ^ String.make (max (24 - String.length c) 0) ' ') desc) 
      commands;
    p
    | _ -> assert false    
  );

  "load", ("load & parse an input file", [F ""],
  function [F src] -> fun _ ->
    BplUtils.ProgramExt.post_parsing 
    << ParsingUtils.parse_file BplParser.program_top BplLexer.token 
    <| src
    | _ -> assert false
  );

  "seq-framework", ("wrap {:entrypoint} procedures for subsequent steps", [], 
  function [] ->
    BplInitAxioms.init_axioms_at_entry_points 
    << BplWrapEntrypoints.wrap_entrypoint_procedures
    | _ -> assert false
  );

  "delay-bounding", ("delay-bounded translation of yield statements", [I 1; I 0], 
  function [I rounds; I delays] ->
    BplYieldElimination.delay_bounding rounds delays
    | _ -> assert false
  );

  "async-to-seq-dfs", ("depth-first async-to-sequential call translation", [], 
  function [] ->
    BplAsyncToSeq.async_to_seq
    | _ -> assert false
  );

  "prepare", ("prepare code for verifier (Boogie-SI or Boogie-FI)", [S "BACK-END"], 
  function 
    | [S "Boogie-SI"] -> BplBackend.for_boogie_si
    | [S "Boogie-FI"] -> BplBackend.for_boogie_fi
    | [S s] -> error "Invalid argument to 'prepare' command: %s" s; exit (-1)
    | _ -> assert false
  );
  
  "strip-internal-markers", ("get rid of internally-used annotations", [], 
  function [] ->
    BplMarkers.strip_internal_markers
    | _ -> assert false
  );

  "print", ("print out the generated program", [F ""], 
  function [F file] -> fun p ->
    print_to_file file (BplAst.Program.print p); 
    p
    | _ -> assert false
  );
]

let _ =
  let ast = ref [] in
  let cmdline = Queue.create () in
  Array.iter (flip Queue.add <| cmdline) Sys.argv;
  ignore <| Queue.pop cmdline;
  
  while not (Queue.is_empty cmdline) do
    let cmd = Queue.pop cmdline in
    try 
      let _, types, fn = List.assoc cmd commands in
      let args = 
        List.map (fun t -> 
          try 
            let a = Queue.pop cmdline in
            match t with
            | B _ -> (try B (bool_of_string a) with _ -> failwith "expected Boolean")
            | I _ -> (try I (int_of_string a) with _ -> failwith "expected integer")
            | S _ -> S a
            | F _ -> F a

          with Queue.Empty ->
            error "Not enough arguments to '%s' command; requires %n." cmd (List.length types);
            exit (-1)

          | Invalid_argument e
          | Failure e ->
            error "Invalid arguments to '%s' command: %s" cmd e;
            exit (-1)

          ) types
      in
      ast := fn args !ast

    with Not_found ->
      error "Invalid command: %s." cmd;
      exit (-1)      
      
  done
  