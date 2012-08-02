open Prelude
open PrettyPrinting
open Printf
open BplAst
open BplUtils
open BplUtils.Extensions
open BplUtils.Abbreviations

let instrument k p =
	
	let methods = 
		Option.cat
		<| List.map (
			function D.Proc (ax,n,p) | D.Impl (ax,n,p) 
			when A.has "method" ax -> Some n
			| _ -> None				
		) p 
	in
	
	if List.length methods < 1 then
		failwith "This program has no methods! \
			Annotate some procedures with {:method}.";
	
	Program.translate
	
		~new_global_decls: (
			D.parse <| sprintf (
				"var time: int;
				 var ret: bool;
				 const TIME_BOUND: int;
				 axiom TIME_BOUND == %n;
				 var %s: [int] int;
				 var %s: [int][int] int;
				" )
				k
				( String.concat ", " <| List.map (sprintf "%s.open") methods )
				( String.concat ", " <| List.map (sprintf "%s.done") methods )
		)
		
		~new_proc_params: (function (_,"Main",_) -> [] | _ -> ["t0",T.Int])
    
		~per_stmt_map: 
      ( fun (_,n,_) -> begin
        function
        | ls, S.Return when List.mem n methods ->
          Ls.parse <| sprintf "goto Violin.End.%s;" n
          
        | ls, S.Call (ax,p,es,r) when A.has "async" ax && List.mem n methods ->
  				(* ToDo: add back labels. *)
  				( Ls.parse <| sprintf 
  					"call corral_atomic_begin ();
  					 if (ret) {
  					     time := time + 1;
  					     ret := false;
  					 }
  					 call corral_atomic_end ();
  					 assume time <= TIME_BOUND;
  					 %s.open[time] := %s.open[time] + 1;" 
  					p p )
  				@ [[], S.Call (ax,p,es@[E.ident "time"],r)]
          
          
        | ls -> ls :: []
        end )
		
		~proc_body_prefix:
      ( function
        |  (_,"Main",_) -> 
  				Ls.parse <| sprintf (
  					"time := 0;
  					 ret := false;
  					 assume (forall t0:int :: %s);
  					 assume (forall t0:int, tf:int :: %s);
  					" )
  					( String.concat " && "
  					  << List.map (sprintf "%s.open[t0] == 0") 
  					  <| methods )
  					( String.concat " && "
  					  << List.map (sprintf "%s.done[t0][tf] == 0") 
  					  <| methods )
        | _ -> [] )
			
		~proc_body_suffix: 
      ( function 
        | (_,n,_) when List.mem n methods -> 
  				Ls.parse <| sprintf (
  					"Violin.End.%s: \
  					 call corral_atomic_begin (); \
  					 ret := true; \
  					 %s.open[t0] := %s.open[t0] - 1; \
  					 %s.done[t0][time+1] := %s.done[t0][time+1] + 1; \
  					 call Violin.PresburgerInvariant(%s); \
  					 call corral_atomic_end (); \
  					 return; \
  					" )
  					n n n n n 
  					( String.concat ", "
  					  << List.flatten
  					  << List.map (fun m ->
  							List.map (sprintf "%s.open[%n]" m) (List.range 0 k)
  							@ List.map 
  								(uncurry <| sprintf "%s.done[%n][%n]" m)
  								( List.flatten
  								  << List.map (fun i -> 
  										List.map (Tup2.make i) 
  										<| List.range i (k))
  								  <| List.range 0 k)
  						)
              <| methods )
        | _ -> [] )
	p