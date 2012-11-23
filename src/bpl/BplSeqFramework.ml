open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils.Operators
open BplUtils.Extensions
open BplUtils.Abbreviations

let main_proc = "main"
let real_main_proc = "__XX__main"
let init_proc = "__XX__init"
let validate_proc = "__XX__validate"
let check_proc = "__XX__check"
let new_proc = "__XX__new"
let err_flag = "__XX__err_flag"
let self_var = "__self"
let alloc_var = "__XX__alloc"
let root_pid = "__XX__root"
let null_pid = "__XX__null"
let pid_type = BplAst.Type.T ("pid",[])
let aux_procs = [init_proc; validate_proc; check_proc]

let stage_id = "Seq"

let err_flag = sprintf "%s.flag" stage_id
let top_proc_name = sprintf "%s.Main" stage_id
let main_proc_name = "Main"

let inline_procs = 
  Program.translate
		~replace_global_decls: 
			( fun d -> match d with 
        | D.Proc (ax,_,_) when A.has "leavealone" ax -> [d]
        | D.Proc (ax,n,p) -> [D.Proc (A.add (A.num "inline" 1) ax, n, p)] 
        | _ -> [d] )
      
let boogie_si_mode = true
        
let seq_framework  =   
  Program.translate
    ~new_global_decls: [ 
      D.Var ([A.unit "leavealone"],err_flag,T.Bool,None) ;
            
      D.proc top_proc_name
        ~attrs:( (A.unit "leavealone") :: (if boogie_si_mode then [A.unit "entrypoint"] else []) )
        ~body:(
          (E.ident err_flag |:=| E.bool false)
          @ [ Ls.skip ~attrs:[A.unit "initial"] () ]
          @ [ Ls.call main_proc_name ]
          @ [ Ls.skip ~attrs:[A.unit "validity"] () ]
          @ [ if boogie_si_mode 
              (* Boogie's /stratifiedInline mode checks whether an entrypoint
                 procedure can return. *)
              then Ls.assume (E.ident err_flag) 
              else Ls.assert_ (!| (E.ident err_flag)) ]
          @ [ Ls.return () ]
        )
    ]
      
    ~per_stmt_map: (fun n s -> 
      match s with
      | ls, S.Assert ([],e) ->
        Ls.add_labels ls (E.ident err_flag |:=| (E.ident err_flag ||| (E.negate e)))
      | _ -> [s] 
    )
