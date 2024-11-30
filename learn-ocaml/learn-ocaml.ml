(*** 1 DONE Integer Identifiers *)
let x = Random.int 9 + 1 (* not 0 *)

let x_power_8 =
  let x2 = x*x in
    let x3 = x2*x2 in
      x3*x3;;

(*** 2 String Identifiers*)
let word = "ook"

let sentence = word ^ "," ^ word ^ "," ^ word

let sentence' =
  let w2 = word ^ "," in
     w2^w2^w2^w2^w2

let sentence'' = 
  let w2 = word ^ "," in
    let w3 = w2^w2 in
      w3^w3^w3^w3
(* "ook,ook,ook,ook,ook,ook,ook,ook," *)
let sentence''' = 
  let w2 = word ^ "," ^ word ^ "," in
    let w3 = w2^w2 in
      w3^w3

(*** 3 DONE Simple Functions over Integers *)
let multiple_of n d =
  if n mod d = 0 then true else false
let integer_square_root n = int_of_float (sqrt (float_of_int n))

(*** 4 DONE Simple Functions over Strings *)
let last_character str = str.[String.length str - 1]

let string_of_bool truth =
  if truth then "true" else "false"

(*** 5 DONE Tetragon *)
type point2d = int * int
type tetragon = point2d * point2d * point2d * point2d

let (p : point2d) = 2,3

let pairwise_distinct ((lup, rup, llp, rlp) : tetragon) : bool =
  if List.length (List.sort_uniq compare (lup::rup::llp::rlp::[])) = 4 then true else false

let wellformed ((lup, rup, llp, rlp) : tetragon) : bool =
  if ((fst lup < fst rup) && (fst llp < fst rlp)) &&
      (snd lup > snd llp) &&
      (snd rup > snd rlp)
    then true
  else
    false

let rotate_point ((x, y) : point2d) : point2d =
  (y, -x)

let reorder ((p1:point2d), (p2:point2d), (p3: point2d), (p4:point2d)) : tetragon =
  let fix11 ((p1:point2d), (p2:point2d), (p3: point2d), (p4:point2d)) : tetragon =
    if fst p1 > fst p2 then (p2, p1, p3, p4) else (p1, p2, p3, p4)
  in let fix12 ((p1:point2d), (p2:point2d), (p3: point2d), (p4:point2d)) : tetragon =
    if fst p3 > fst p4 then (p1, p2, p4, p3) else (p1, p2, p3, p4)
  in let fix2 ((p1:point2d), (p2:point2d), (p3: point2d), (p4:point2d)) : tetragon =
    if snd p1 < snd p3 then (p3, p2, p1, p4) else (p1, p2, p3, p4)
  in let fix3 ((p1:point2d), (p2:point2d), (p3: point2d), (p4:point2d)) : tetragon =
    if (snd p2 < snd p4) then (p1, p4, p3, p2) else (p1, p2, p3, p4)
  in (p1, p2, p3, p4) |> fix11 |> fix12 |> fix2 |> fix3

let rotate_tetragon ((lup, rup, llp, rlp) : tetragon) : tetragon =
  (rotate_point lup, rotate_point rup, rotate_point llp, rotate_point rlp) |> reorder |> reorder

(*** 5 Enigma *) 
let list_rev l =
  let rec aux l j = match (l, j) with
    | []  , j -> j
    | h::t, j -> aux t (h::j)
  in aux l []

let rec string_to_list = function
  | "" -> []
  | s  -> s.[0]::(string_to_list (String.sub s 1 (String.length s-1)))

let rec list_to_string = function
  | []   -> ""
  | h::t -> (String.make 1 h)^list_to_string t

let exchange (k : int) : int = match k with
  | x when (10 > x) || (x > 99) -> raise (Failure "invalid_range")
  | n                           -> n |> string_of_int |> string_to_list |> List.rev |> list_to_string |> int_of_string

let is_valid_answer (grand_father_age, grand_son_age) =
  if grand_son_age*4 = grand_father_age then true else false 

let rec find (max_grand_father_age, min_grand_son_age) = match (max_grand_father_age, min_grand_son_age) with
  | (gf, gs) when is_valid_answer (gf, gs) -> (gf, gs)
  | (gf, gs) when gs > gf -> (-1, -1)
  | (gf, gs) -> find (gf-1, gs+1)

(*** 6 Time on Planet Shadokus *)
type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

let wellformed (date : date) = match date with
  | {year; month; day; hour; minute} 
      when year>0 && month>0 && month<6 && day>0 && day<5 && hour>=0 && hour<3 && minute>=0 && minute<2 
      -> true
  | _ -> false

(* The end of year 12 would be:
{ year = 12; month = 5; day = 4; hour = 2; minute = 1 } *)
let next (date : date) : date = match date with
  | {year; month; day; hour; minute} when minute<1
      -> {year;        month;         day;       hour;        minute=minute+1}
  | {year; month; day; hour; minute} when minute==1 && hour<2
      -> {year;        month;         day;       hour=hour+1; minute=0}
  | {year; month; day; hour; minute} when minute==1 && hour==2 && day<4
      -> {year;        month;         day=day+1; hour=0;      minute=0}
  | {year; month; day; hour; minute} when minute==1 && hour==2 && day==4 && month<4
      -> {year;        month=month+1; day=0;     hour=0;      minute=0}
  | {year; month; day; hour; minute}
      -> {year=year+1; month=0;       day=0;     hour=0;      minute=0}

let rec apply n f x =
    if n>0 then apply (n-1) f (f x)
    else x

let of_int (minutes : int) : date =
  apply minutes next the_origin_of_time

(*** 7 Points and vectors *)
type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }
let p = { x = 2.0; y = 1.0; z = 1.0}

let move (p : point) (dp : dpoint) : point = match (p, dp) with
  ({ x; y; z }, { dx; dy; dz }) -> {x=x+.dx; y=y+.dy; z=z+.dz}

let next (obj : physical_object) : physical_object = match obj with
  { position ; velocity } -> { position=move position velocity; velocity}

let is_close (p1: point) (p2: point) : bool = match (p1, p2) with
  | ({ x=x1 ; y=y1 }, { x=x2 ; y=y2 }) when abs_float (x1-.x2)<=1.0 && abs_float (y1-.y2)<=1.0 -> true
  | _ -> false

let will_collide_soon (p1 : physical_object) (p2 : physical_object) : bool =
  match (next p1, p2) with
    | ({ position=p1 ; _ }, { position=p2 ; _ }) when is_close p1 p2 -> true
    | _ -> false

(*** 8 Searching for Strings in Arrays *)
(* Write a function is_sorted : string array -> bool
   which checks if the values of the input array are sorted in strictly increasing order,
   implying that its elements are unique (use String.compare). *)
let is_minor (s1 : string) (s2: string): bool =
  if (String.compare s1 s2) = -1 then true
  else false

let rec is_sorted (a : string array) : bool =
  match Array.to_list a with
    | [] -> true
    | [_] -> true
    | h::t1::t -> (is_minor h t1) && (is_sorted (Array.of_list(t1::t)))

let rec is_sorted_list (l : string list) : bool =
  match l with
    | [] -> true
    | [_] -> true
    | h::t1::t -> (is_minor h t1) && (is_sorted_list (t1::t))

let find dict word =
  "Replace this string with your implementation." ;;

let rec fold_left op acc = function
  | []   -> acc
  | h :: t -> fold_left op (op acc h) t

let _ = fold_left (+) 0 [1;2;3]

let rec fold_left_plus op acc =
  let aux op acc l1 = function
    | []   -> acc
    | h :: t -> fold_left_plus op (op acc h) t
  in aux op acc []

(* Using the binary search algorithm, an element can be found very quickly in a sorted array.
   Write a function find : string array -> string -> int such that find arr word
   is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
   The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity.
   Beware that you really perform the minimal number of accesses.
   For instance, if your function has to test the contents of a cell twice,
   be sure to put the result of the access in a variable, and then perform the tests on that variable. *)
let find (dict:string array) (word:string) : int =
  let rec bin l h = match (l, h) with
    | (l, h) when l > h -> -1
    | (l, h) ->
      let m = (l+h)/2 in
      let mid = dict.(m) in
        if mid = word then m
        else if is_minor mid word then bin (m+1) h
        else bin l (m-1)
  in bin 0 (Array.length dict-1)

(*** 9 Finding the Minimum *)
(* Consider a non empty array of integers a.
   - Write a function min : int array -> int that returns the minimal element of a.
   - Write a function min_index : int array -> int that returns the index of the minimal element of a.
   - Do you think these functions work well on large arrays ?
     Define a variable it_scales and set it to "yes" or "no". *)
let min (a:int array):int =
  Array.fold_left min max_int a   (* Pervasives.min *)

let min_index a =
  let m = min(a) in 
  let rec aux n =
    if a.(n) = m then n
    else aux (n-1)
  in aux (Array.length a-1)

let it_scales = "no"

(*** 10 A Small Typed Database *)

(* A phone number is a sequence of four integers. *)
type phone_number = int * int * int * int;;

(* A contact has a name and a phone number. *)
type contact = {
  name         : string;
  phone_number : phone_number
}

(* Here is a dumb contact. *)
let nobody = { name = ""; phone_number = (0, 0, 0, 0) }

(* A database is a collection of contacts. *)
type database = {
  number_of_contacts : int;
  contacts : contact array;
}

(* [make n] is the database with no contact and at most [n] contacts
    stored inside. *)
let make max_number_of_contacts = {
  number_of_contacts = 0;
  contacts = Array.make max_number_of_contacts nobody
}

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
  code    : int;
  contact : contact;
}

let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
  in
  aux 0;;

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
    (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
        if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
          number_of_contacts = db.number_of_contacts + 1;
          contacts = Array.init (Array.length db.contacts) cells
        }
      in
      (true, db', contact);;
  
let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        nobody
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;

(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody);;

let proof_of_bug =
  [| { code = 0; contact = { name ="Juan"; phone_number = (1, 1, 1, 1) }};
     { code = 1; contact = { name ="Jose"; phone_number = (2, 2, 2, 2) }};
     { code = 2; contact = { name ="Juan"; phone_number = (1, 1, 1, 1) }} |] 

let delete db contact =
  "Replace this string with your implementation." ;;

let update db contact =
  "Replace this string with your implementation." ;;

let engine db { code ; contact } =
  "Replace this string with your implementation." ;;

(*** 11 First In First Out *)
(* A queue is a standard FIFO data structure. See wikipedia [https://en.wikipedia.org/wiki/Queue_(abstract_data_type)]
   In this exercise, we implement a queue with a pair of two lists (front, back) such that
   front @ List.rev back represents the sequence of elements in the queue. *)
type queue = int list * int list

let is_empty' ((front, back) : queue) : bool =
  match (front, back) with
    | ([], []) -> true 
    | _ -> false

let is_empty'' (q : queue) : bool =
  match (fst q, snd q) with
    | ([], []) -> true 
    | _ -> false

let is_empty (q : queue) : bool =
  match q with
    | ([], []) -> true 
    | _ -> false

let is_empty'''' (q : queue) : bool =
  let (front, back) = q in match (front, back) with
    | ([], []) -> true 
    | _ -> false

(* Write a function enqueue : int -> queue -> queue such that
   enqueue x q is the queue as q except that x is at the end of the queue. *)
let enqueue' (x:int) ((front, back) : queue) : queue =
    (front, x :: back)

let enqueue (x:int) (q:queue) : queue =
  let (front, back) = q in (front, x :: back)

(* Write a function split : int list -> int list * int list such that split l = (front, back)
   where l = back @ List.rev front 
   and the length of back and front is List.length l / 2 or List.length l / 2 + 1 *)
let rec last (l: int list): int =
  match l with
    | [] -> failwith "non_valid_list"
    | [n] -> n
    | h::t -> last t

let split (l : int list) : int list * int list =
  let rec aux front back =
    match (front, back) with
      | ([], []) -> (front                , back)
      | (_ , _ ) -> (front @ [(List.hd l)], back @ [(last l)])
  in aux l (List.rev l)

(* let rec make_pair_list (l : int list) : (int*int) list =
  let lrev = List.rev l in
  match (l, lrev) with
    | ([], []) -> []
    | ([]) *)

(* Write a function dequeue : queue -> int * queue such that dequeue q = (x, q') 
   where x is the front element of the queue q and q' corresponds to remaining elements.
   This function assumes that q is non empty. *)
let rec remove1 (l : 'a list) : 'a list =
  match l with
    | []   -> []
    | [n]  -> []
    | h::t -> h::remove1 t 

let rec remove2 (l : 'a list) : 'a list =
  match l with
  | []            -> []
  | [n]           -> []
  | h::t1::t2::[] -> [h]
  | h::t          -> h::remove2 t 

let dequeue (q:queue) : int*queue =
  let tail = function
    | [] -> []
    | l  -> List.tl l
  in match q with
    | ([], (bh::bt as b)) -> let brev = List.rev b in
                             (List.hd brev, ([List.hd (List.tl brev)], remove2 b))
    | (f , b     )        -> (List.hd f   , (tail f                  , b        ))

let test_as (l1 : int list) (l2 : int list) : int list =
  match (l2, l1) with
  | (_, [])  -> print_string "caso1"; []
  | (_, [n]) -> print_string "caso2"; [n]
  | (_, (h::t as l)) -> print_string "caso3"; h::t@l

(*** 12 Classic Functions Over Lists *)

(* Write a function mem : int -> int list -> bool such that
   mem x l is true if and only if x occurs in l. *)
let rec mem (x : int) (l : int list) : bool =
  match l with 
  | [] -> false
  | h::t -> if h=x then true else (mem x t)

(* Write a function append : int list -> int list -> int list
   such that append l1 l2 is the concatenation of l1 and l2. *)
let rec append l1 l2 =
  l1 @ l2

(* Write a function combine : int list -> int list -> (int * int) list such that
   combine l1 l2 is the list of pairs obtained by joining the elements of l1 and l2.
   This function assumes that l1 and l2 have the same length.
   For instance, combine [1;2] [3;4] = [(1, 3); (2, 4)]. *)
let rec combine (l1 : int list) (l2 : int list) : (int*int) list =
  match (l1, l2) with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1, h2)::(combine t1 t2)
  | _, _ -> failwith "different_lengths"

(* Write a function assoc : (string * int) list -> string -> int option such that
   assoc l k = Some x if (k, x) is the first pair of l whose first component is k.
   If no such pair exists, assoc l k = None. *)
let rec assoc (l : (string*int) list) (k : string) : int option =
  match l with
  | [] -> None
  | (f,s)::t -> if k = f then Some s else (assoc t k)
  
(*** 13 Symbolic Manipulation of Arithmetic Expressions *)
type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))
(* Write the expression 2 * 2 + 3 * 3 in a variable my_example. *)
let my_example =
  EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3))

let rec eval e =
  match e with
  | EInt x -> x
  | EAdd (x, y) -> (eval x) + (eval y)
  | EMul (x, y) -> (eval x) * (eval y)

let factorize e =
  "Replace this string with your implementation." ;;

let expand e =
  "Replace this string with your implementation." ;;

let simplify e =
  "Replace this string with your implementation." ;;
