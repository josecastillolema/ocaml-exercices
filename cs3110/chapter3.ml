(*** Exercise: list expressions *)
let _ = [1;2;3;4;5]

let _ = 1::2::3::4::5::[]

let _ = [1] @ [2; 3; 4] @ [5]

(*** Exercise: product *)
let rec product = function
  | []   -> 1
  | h::t -> h*product t

(*** Exercise: concat *)
let rec concat = function
  | []   -> ""
  | h::t -> h^concat t

(*** Exercise: product test  *)
let _ = assert (product [] = 1)

let _ = assert (product [2;3] = 6)

(*** Exercise: patterns  *)
let patterns = function
  | []          -> false
  | "bigred"::_ -> true
  | _           -> false

let patterns' = function
  | _::_::[] | _::_::_::_::[] -> true
  | _ -> false

let patterns'' = function
  | x::y::_ when x=y -> true
  | _ -> false

(*** Exercise: library *)
let library l =
  try
    List.nth l 4 with
      _ -> 0

let library' (l : int list) =
  List.sort Stdlib.compare l |> List.rev 
      
(*** Exercise: library test *)
let _ = assert (library [1;2;3;4;5] = 5)

let _ = assert (library [1;2;3;4] = 0)

let _ = assert (library' [2;3;4] = [4; 3; 2])

(*** Exercise: library puzzle *)
let last l = List.nth l (List.length l-1)

let any_zeroes = List.mem 0

(*** Exercise: take drop *)
let rec take n lst = match n with
  | 0 -> []
  | n when n > List.length lst -> lst
  | n -> List.hd lst :: (take (n-1) (List.tl lst))

let rec drop n lst = match n with
  | 0 -> lst
  | n when n > List.length lst -> []
  | n -> (drop (n-1) (List.tl lst))

(*** Exercise: take drop tail  *)


(*** Exercise: unimodal *)
let rec apply f = function
  | [] -> true
  | _::[] -> true
  | x::l when f x (List.hd l) -> apply f l
  | _ -> false

let (growing : 'a list -> bool) = apply ( < )

let (decreasing : 'a list -> bool) = apply ( > )

let rec is_unimodal = function
  | [] -> true
  | _::[] -> true
  | l when growing l -> true
  | l when decreasing l -> true
  | h::t when growing (h::(List.hd t)::[]) -> is_unimodal t
  | h::t when growing [h] && decreasing t -> true
  | _ -> false

(*** Exercise: powerset *)
(* if set A = {a,b}, then the power set of A is { {}, {a}, {b}, {a,b}}. *)
(* powerset : int list -> int list list *)

let rec partial_powerset = function
  | []    -> [[]]
  | x::[] -> [[];[x]]
  | x::y  -> [x::y;[];[x]]@partial_powerset y

let powerset l = partial_powerset l @ partial_powerset (List.rev l) |> List.sort_uniq compare

(*** Exercise: print int list rec *)
let rec print_int_list = function
  | [] -> ()
  | h :: t -> print_endline (string_of_int h); print_int_list t

(*** Exercise: print int list iter *)
let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(*** Exercise: student *)
type student = {first_name : string; last_name : string; gpa : float}

let _ = {first_name="Jose"; last_name="Castillo"; gpa=5.}

let extract_name = function
  {first_name; last_name; _} -> (first_name, last_name)

let create_student first_name last_name gpa = {first_name=first_name; last_name=last_name; gpa=gpa}

(*** Exercise: pokerecord *)
type poketype = Normal | Fire | Water

type pokemon = {name : string; hp : int; ptype : poketype}

let charizard = {name="Charizard"; hp=78; ptype=Fire}
let squirtle = {name="Squirtle"; hp=44; ptype=Water}

(*** Exercise: safe hd and tl *)
let safe_hd = function
  | []   -> None
  | x::_ -> Some x

let safe_tl = function
  | []   -> None
  | _::t -> Some t

(*** Exercise: pokefun *)
let max_pk pk1 pk2=
  if (compare pk1.hp pk2.hp)=1 then pk1
  else pk2 

let max_hp = function
  | [] -> raise (Failure "list_empty")
  | l  -> List.fold_left max_pk {name="acc"; hp=0; ptype=Normal} l

(*** Exercise: date before *)

(*** Exercise: earliest date  *)

(*** Exercise: assoc list *)

(*** Exercise: cards *)

(*** Exercise: matching *)

(*** Exercise: quadrant *)

(*** Exercise: quadrant when *)

(*** Exercise: depth *)

(*** Exercise: shape *)

(*** Exercise: list max exn *)
let list_max = function
  | [] -> raise (Failure "list_max")
  | l  -> List.fold_left max 0 l

(*** Exercise: list max exn string *)
let list_max_string = function
  | [] -> "empty"
  | l  -> List.fold_left max 0 l |> string_of_int

let list_equal = function
  | [] | [_]  -> true
  | l         -> if List.length (List.sort_uniq compare l) = 1 then true else false

let list_equal' l =
  if (List.length (List.sort_uniq compare l)) = 1 then true else false

let nth n l = List.nth l n
let _ = [2;3;4] |> nth 2   (* - : int = 4 *)

(*** Exercise: list max exn ounit *)
let _ = assert (list_max [] = raise (Failure "list_max"));;

let _ = assert (list_max [1;2;3;4] = 4)

(*** Exercise: is_bst *)

(***  Exercise: quadrant poly  *)