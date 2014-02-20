(** A few useful functions for functions, lists, tuples, etc. *)

let flip f x y = f y x
let const x _ = x
let tsnoc _ x = x
let const2 x _ _ = x
let id x = x

let min x y = if x < y then x else y
let max x y = if x > y then x else y

let apply f x = f x
let ylppa x f = f x
let (<|) = apply

let ($|) = ylppa
and (|$) = apply

let compose f g x = f (g x)
let (<<) = compose

let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

let curry3 f x y z = f (x,y,z)
let uncurry3 f (x,y,z) = f x y z

let chain fns x = List.fold_left ylppa x fns

let (||||) f g = fun x -> f x || g x
let (&&&&) f g = fun x -> f x && g x

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fresh_int_fn () =
  let cur = ref 0 in
  fun () -> incr cur; !cur

let warnings = ref ([]: string list)
let warn str =
  if not (List.mem str !warnings) then begin
    warnings := str :: !warnings;
    prerr_string str;
    prerr_newline ()
  end

let info fmt = Printf.eprintf ("Info: " ^^ fmt ^^ "\n")
let error fmt = Printf.eprintf ("Error: " ^^ fmt ^^ "\n")

let rec ntimes f x n =
	if n < 1 then x
	else ntimes f (f x) (n-1)
	
let map_fold_to_map mff fn = snd << mff (fun acc x -> acc, fn x) ()
let map_fold_to_fold mff fn acc = fst << mff (fun acc x -> fn acc x, x) acc

module Control = struct
	let rec until until_fn do_fn acc =
		if until_fn acc then acc
		else until until_fn do_fn (do_fn acc)

	let rec do_until do_fn until_fn acc =
		let acc = do_fn acc in
		if until_fn acc then acc
		else do_until do_fn until_fn acc
end
		
	
module Tup2 = struct
	type ('a,'b) t = 'a * 'b
	let dup x = x,x
	let make x y = x,y
	let ekam y x = x,y
	let rotate (x,y) = y,x
	let map_fold f g acc (x,y) = 
		let acc, x = f acc x in
		let acc, y = g acc y in
		acc, (x,y)
  let map_fold f g a (x,y) = 
    let a, x = f a x in
    let a, y = g a y in
    a, (x,y)
	let map f g (x,y) = f x, g y
	let mapp f (x,y) = f x, f y
	let fold_left f a (x,y) = f (f a x) y
	let fold_right f (x,y) a = f x (f y a)
	let curry f x y = f (x,y)
	let uncurry f (x,y) = f x y
	let to_list (x,y) = [x;y]
	let compare = Pervasives.compare
	let compare_with c d (w,x) (y,z) =
		match c w y with
		| 0 -> d x z
		| i -> i
end

module Tup3 = struct
	type ('a,'b,'c) t = 'a * 'b * 'c
	let make x y z = x,y,z
	let fst (x,_,_) = x
	let snd (_,y,_) = y
	let thd (_,_,z) = z
	let map fx fy fz (x,y,z) = fx x, fy y, fz z
	let drop_fst (_,y,z) = y,z
	let compare = Pervasives.compare
	let compare_with c d e (u,v,w) (x,y,z) =
		match c u x with
		| 0 -> begin
			  match d v y with
			  | 0 -> e w z
			  | i -> i
		  end
		| i -> i
end

module Option = struct

  let compare c x y =
    match x, y with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some x, Some y -> c x y

  let equal f x y =
    match x, y with
    | None, None -> true
    | Some x, Some y -> f x y
    | _ -> false

  let map_fold fn acc = function
	| None -> acc, None
	| Some x -> let acc, x = fn acc x in acc, Some x
	
  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let iter f = function
	  | None -> ()
	  | Some x -> f x

  let seq f = function
    | None -> None
    | Some x -> f x

  let fold f a = function
    | None -> a
    | Some x -> f a x

  let exists f = function
    | None -> false
    | Some x -> f x

  let for_all f = function
    | None -> true
    | Some x -> f x

  let is_some o = exists (const true) o
  let is_none o = for_all (const false) o

  let some = function
    | Some x -> x
    | None -> failwith "Option.some: expected Some _"

  let some_or a = function
	  | Some x -> x
	  | None -> a

  let rec cat xs =
	  match xs with
	  | [] -> []
	  | None::xs -> cat xs
	  | (Some x)::xs -> x :: cat xs
    
  let first xs = try List.hd (cat xs) with Failure _ -> raise Not_found

  let of_string s =
	  if s = "" then None
	  else Some s

  let reduce f a o =
	  match o with
	  | Some x -> f x
	  | _ -> a
    
  let bool o = reduce id false o
  let list o = reduce id [] o
  
  let combine o p = 
    match o, p with
    | Some x, Some y -> Some (x,y)
    | None, None -> None
    | _ -> invalid_arg "Option.combine"
    
  let listjoin o p =
    match o, p with
    | Some x, Some y -> Some (x,y)
    | Some x, None -> Some (x,[])
    | None, Some y -> Some ([],y)
    | _ -> None    

  let to_string fn =
	  reduce (Printf.sprintf "Some (%s)" << fn) "None"
end

module List = struct
  include List

  let cons x xs = x::xs
  let snoc xs x = x::xs
  let tailcons xs x = xs @ [x]

  let rec compare c xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x::xs, y::ys ->
	begin
	  match c x y with
	  | 0 -> compare c xs ys
	  | i -> i
	end

  let equal eq xs ys =
    compare (fun x y -> if eq x y then 0 else 1) xs ys = 0
	  
  let init = function
    | [] -> invalid_arg "List.init: got empty"
    | xs -> rev << tl << rev <| xs

  let last = function
    | [] -> invalid_arg "List.last: got empty"
    | xs -> hd << rev <| xs

  let only = function
    | [x] -> x
    | _ -> invalid_arg "List.only: expected singleton"

  let first f xs =
    match filter f xs with
    | x::_ -> Some x
    | _ -> None

  let unit x = x::[]
  let return = unit
  let singleton = unit
  
  let collect fn = Option.cat << map fn

  let bind fn = (List.flatten << List.map fn)
  let lift = List.map

  let reduce fn a = function
	  | [] -> a
	  | xs -> fn xs

  let add_uniq x xs =
    if List.mem x xs then xs else x::xs

  let rec insert y i xs = 
    if i < 0 then xs
    else if i == 0 then y :: xs
    else match xs with
    | x::xs -> x :: insert y (i-1) xs
    | [] -> y :: []

  let remove x =
    List.filter ((=) 0 << Pervasives.compare x)

  let remove_all xs =
	  List.filter (fun y -> not <| List.mem y xs)

  let map_fold_left f acc =
    Tup2.map id List.rev
    << List.fold_left
      (fun (acc,ys) -> Tup2.map id (fun y -> y::ys) << f acc)
      (acc,[])

  let mapi f = snd << map_fold_left (fun i x -> i+1, f i x) 0
  let fold_lefti f a = fst << fold_left (fun (a,i) x -> f a i x, i+1) (a,0)
  let iteri f = fold_lefti (fun _ i x -> f i x) ()
  
  (* ToDo: write it better *)
  let iteri2 f a b = 
     iteri (fun i (a, b) -> f i a b) <| combine a b

  let existsi f =
	  let rec exi f i xs =
		  match xs with
		  | [] -> false
		  | x::xs -> f i x || exi f (i+1) xs
	  in exi f 0		  

  let map_hd f = function
	  | x::xs -> (f x)::xs
	  | [] -> []

  let map_tl f = function
	  | x::xs -> x::(List.map f xs)
	  | [] -> []
	  
  (** A list of integers from [i] to [j]. *)
  let rec range i j = if i > j then [] else i :: range (i+1) j

  let make f i =
    List.map f (range 0 (i-1))
    
  let take_drop n xs =
    let rec aux n xs ys =
      if n > 0 then aux (n-1) (List.hd ys :: xs) (List.tl ys)
      else List.rev xs, ys
    in aux n [] xs

  let rec drop n xs =
	  if n > 0 then drop (n-1) (List.tl xs)
	  else xs

  let take n =
	  List.rev << snd
	  << List.fold_left
		  (fun (i,xs) y -> if i > 0 then (i-1,y::xs) else (i,xs)) (n,[])

  let take_while p ls =
	  let rec tw_aux xs ys = 
		  match xs with
		  | y :: xs when p y -> tw_aux xs (y::ys)
		  | _ -> List.rev ys, xs
	  in tw_aux ls []

  let union xs ys =
	  List.fold_left (flip add_uniq) [] (xs@ys)

  let unions xxs = List.fold_left union [] xxs

  let minus xs ys = List.filter (not << (flip List.mem) ys) xs
  let diff xs ys = minus xs ys

  let intersects xs =
	  List.exists (flip List.mem <| xs)
		
  let split3 xyzs =
	  List.fold_left
		  (fun (xs,ys,zs) (x,y,z) -> x::xs, y::ys, z::zs)
		  ([],[],[])
	  <| List.rev xyzs

  let rec transpose xss =
	  match xss with
	  | [] -> []
	  | _ ->
			(try cons (map hd xss) with _ -> id)
				(try transpose (map tl xss) with _ -> [])

  let split_n = transpose

  let equal_n = function
	  | [] -> true
	  | x::xs -> List.for_all ((=) x) xs
		  
  let product xs ys =
    List.flatten
    <| List.fold_left
		(flip <| fun y -> cons << List.map (Tup2.ekam y) <| xs)
      []
      ys
      
  let rec words n xs =
    if n <= 0 then [ [] ]
    else begin
      let ws = words (n-1) xs in
      List.flatten 
      << List.map (fun w -> List.map (fun x -> w @ [x]) xs) 
      <| ws
    end
      
  (* let words n xs = 
    List.fold_left (fun words _ -> 
      List.flatten 
      << List.flatten
      << List.map (fun word -> 
        let k = List.length word in
        List.map (fun x -> 
          List.map (fun i -> insert x i word) (range 0 k)
        ) xs
      ) <| words
    ) [[]] (range 1 n) *)

  let separate f = function
    | [] -> [ [] ]
    | xs ->
	List.rev
	<< List.map List.rev
	<< (function (ys,[]) -> ys | (ys,zs) -> zs::ys)
	<< List.fold_left
	  (fun (ys,zs) x ->
	     match f x, zs with
	     | false, _ -> ys, x::zs
	     | true, [] -> [x] :: ys, []
	     | true, _ -> [x] :: zs :: ys, [])
	  ([],[])
	<| xs
end

let (=<<) = List.bind
let (>>=) a fa = List.bind fa a
let (>=>) fa fb = fun a -> List.unit a >>= fa >>= fb
let (<=<) fb fa = fun a -> fb =<< (fa =<< List.unit a)

type ('a, 'b) either = Left of 'a | Right of 'b
  
module Either = struct
	let map f g = function
		| Left x -> Left (f x)
		| Right y -> Right (g y)

	let fold f g a = function
		| Left x -> f a x
		| Right y -> g a y
    
  let map_fold f g a = function
    | Left x -> let a, x = f a x in a, Left x
    | Right y -> let a, y = g a y in a, Right y

	let reduce f g = fold (const f) (const g) ()

	let separate es =
		List.fold_left
			(fun (xs,ys) e ->
				 match e with
				 | Left x -> x::xs, ys
				 | Right y -> xs, y::ys)
			([],[])
		<| List.rev es
end

module String = struct
	include String
	let drop n s =
		let k = String.length s in
		if n > k then ""
		else String.sub s n (k-n)
		
	let until s c = 
		try sub s 0 (index s c)
		with Not_found -> s

	let trim cs s =
		Str.global_replace (Str.regexp ("[" ^ cs ^ "]")) "" s

	let matches re s =
		Str.string_match (Str.regexp re) s 0
		
	let nth_index s n c =
		let rec nth n i =
			if n = 0 then i
			else nth (n-1) (index_from s i c)
		in
		nth n 0

	let map fn s =
		let s = copy s in
		let i = ref 0 in
		while !i < length s do
			s.[!i] <- fn (s.[!i])
			incr i
		done;
		s

	let map_whole fn = function
		| "" -> ""
		| s -> fn s
end

module Set = struct
  module type OrderedType = sig
    include Set.OrderedType
  end

  module type S = sig
    include Set.S
	val of_list : elt list -> t
    val map : (elt -> elt) -> t -> t
	val find : (elt -> bool) -> t -> elt
	val unions : t list -> t

	val uniqify_list : elt list -> elt list
  end
    
  module Make = functor (Ord : Set.OrderedType) ->
    (struct
       include Set.Make(Ord)
	   let of_list = List.fold_left (flip add) empty
       let map f = (flip <| fold (add << f)) empty
	   let find f = choose << filter f
	   let unions ss = List.fold_left union empty ss
	   let uniqify_list = elements << of_list

     end : S with type elt = Ord.t)
end

(* module Map = struct *)
(*   module type OrderedType = sig *)
(*     include Map.OrderedType *)
(*   end *)
    
(*   module type S = sig *)
(*     include Map.S *)
(*     val cardinal : 'a t -> int *)
(*     val elements : 'a t -> (key * 'a) list *)
(*     val keys : 'a t -> key list *)
(*     val vals : 'a t -> 'a list *)
(*     val exists : (key -> 'a -> bool) -> 'a t -> bool *)
(*     val forall : (key -> 'a -> bool) -> 'a t -> bool *)
(* 	val filter : (key -> 'a -> bool) -> 'a t -> 'a t *)
(* 	val inter : 'a t -> 'a t -> 'a t *)
(* 	val union : 'a t -> 'a t -> 'a t *)

(*     val for_all_prod : (key -> 'a -> key -> 'a -> bool) -> 'a t -> bool *)
(*   end *)
    
(*   module Make = functor (Ord : Map.OrderedType) -> *)
(*     (struct *)
(*        include Map.Make(Ord) *)
(*        let elements m = (flip <| fold (fun k v -> List.cons (k,v))) [] m *)
(*        let keys m = (flip <| fold (fun k _ -> List.cons k)) [] m *)
(*        let vals m = (flip <| fold (fun _ v -> List.cons v)) [] m *)
(* 	   let cardinal m = List.length <| elements m *)
(*        let exists f = (flip <| fold (fun k v b -> if b then b else f k v)) false *)
(*        let forall f = (flip <| fold (fun k v b -> *)
(* 										 if not b then b else f k v)) true *)

(* 	   let filter f m = *)
(* 		   fold (fun k v mm -> if f k v then add k v mm else mm) m empty *)
		   
(* 	   let inter m n = *)
(* 		   filter (fun k _ -> mem k n) m *)

(* 	   let union m n = fold add m n *)

(*        let fold_prod f m = fold (fun k v -> fold (f k v) m) m *)

(*        (\* XXX: exception flow not ideal; use kfold *\) *)
(*        exception Found_false *)
(*        let for_all_prod p m = *)
(* 	 try fold_prod *)
(* 	   (fun k u l v _ -> *)
(* 	      if p k u l v then true *)
(* 	      else raise Found_false) *)
(* 	   m true *)
(* 	 with Found_false -> false *)

(*      end : S with type key = Ord.t) *)
(* end *)

module StringSet = Set.Make(
	struct
		type t = string
		let compare = compare
	end)

module IntMap = Map.Make(
	struct
		type t = int
		let compare = compare
	end)
	
(* module MultiMap  = struct *)
(*   module type S = sig  *)
(*     type t *)
(*     type key *)
(*     type value *)
(*     module ValueSet : Set.S *)

(*     val cardinal : t -> int *)
(*     val add_all : key -> ValueSet.t -> t -> t *)
(*     val fold_set : (key -> ValueSet.t -> 'a -> 'a) -> t -> 'a -> 'a *)

(* 	val elements : t -> (key * value) list *)

(*     val empty : t *)
(*     val is_empty : t -> bool *)
(*     val add : key -> value -> t -> t *)
(*     val find : key -> t -> ValueSet.t *)
(*     val remove : key -> t -> t *)
(*     val mem : key -> t -> bool *)
(*     val iter : (key -> value -> unit) -> t -> unit *)
(*     val map : (value -> value) -> t -> t *)
(*     val mapi : (key -> value -> value) -> t -> t *)
(*     val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a *)
(*     val compare : t -> t -> int *)
(*     val equal : t -> t -> bool *)
(*   end *)
    
(*   module Make = *)
(*     functor ( Key : Map.OrderedType ) -> *)
(*       functor ( Val : Map.OrderedType ) ->  *)
(* 	(struct *)
(* 	   module M = Map.Make(Key) *)
(* 	   module ValueSet = Set.Make(Val) *)

(* 	   type key = Key.t *)
(* 	   type value = ValueSet.elt *)
(* 	   type t = ValueSet.t M.t * int *)

(* 	   let cardinal = snd *)
(* 	   let empty = M.empty, 0 *)
(* 	   let is_empty (_,c) = c = 0 *)

(* 	   let find k (m,_) = *)
(* 	     try M.find k m *)
(* 	     with Not_found -> ValueSet.empty *)
	       
(* 	   let add k v (m,c) = *)
(* 	     let vs = find k (m,c) in *)
(* 	     if ValueSet.mem v vs then (m,c) *)
(* 	     else M.add k (ValueSet.add v vs) m, c+1 *)

(* 	   let add_all k vs = ValueSet.fold (add k) vs  *)

(* 	   let remove k (m,c) = *)
(* 	     let vs = find k (m,c) in *)
(* 	     M.remove k m, c - ValueSet.cardinal vs *)
	       
(* 	   let mem k = M.mem k << fst *)
(* 	   let iter f = M.iter (fun k -> ValueSet.iter (f k)) << fst *)
(* 	   let map f = Tup2.map (M.map (ValueSet.map f)) id *)
(* 	   let mapi f = Tup2.map (M.mapi (fun k -> ValueSet.map (f k))) id *)
(* 	   let fold f = M.fold (ValueSet.fold << f) << fst *)
(* 	   let fold_set f = M.fold f << fst *)

(* 	   let elements m = fold (fun k v es -> (k,v) :: es) m [] *)

(* 	   let compare m m' = M.compare ValueSet.compare (fst m) (fst m') *)
(* 	   let equal m m' = M.equal ValueSet.equal (fst m) (fst m') *)
(* 	 end : S with type key = Key.t with type value = Val.t with type *)
(* 	   ValueSet.t = Set.Make(Val).t with type ValueSet.elt = Val.t) *)
(* end *)

(* Local Variables: *)
(* compile-command: "cd .. && make -k" *)
(* End: *)
