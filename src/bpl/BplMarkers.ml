open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

let entrypoint = "entrypoint"
let formerentrypoint = "formerentrypoint"

let begin_seq = "beginseq"
let end_seq = "endseq"

let leavealone = "leavealone"

let noyields = "noyields"
let async = "async"
let skip = "skip"

let internal = [ 
  begin_seq ; end_seq ; leavealone ; noyields ; async ; skip ; formerentrypoint 
]

let strip_internal_markers = 
  List.map (chain (List.map D.strip internal))
  << Program.translate ~per_stmt_map: (const (List.unit << chain (List.map Ls.strip internal)))
