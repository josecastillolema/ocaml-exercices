type nucleotide = A | C | G | T

let rec hamming_distance (x:nucleotide list) (y:nucleotide list) =
  if List.length x <> List.length y then
    failwith "disallow different lengths"
  else
    match (x, y) with
    | (    [],      _)            -> (0, "")
    | (     _,     [])            -> (0, "")
    | (h1::t1, h2::t2) when h1=h2 -> (0 + fst (hamming_distance t1 t2), "")
    | ( _::t1,  _::t2)            -> (1 + fst (hamming_distance t1 t2), "")

open Base
open Poly

type nucleotide = A | C | G | T

let rec hamming_distance (x:nucleotide list) (y:nucleotide list) =
  if List.length x <> List.length y then
    failwith "disallow different lengths"
  else
    match (x, y) with
    | (    [],      _)                  -> (0, "")
    | (     _,     [])                  -> (0, "")
    | (h1::t1, h2::t2) when equal h1 h2 -> (0 + fst (hamming_distance t1 t2), "")
    | ( _::t1,  _::t2)                  -> (1 + fst (hamming_distance t1 t2), "")
    

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

  let rec raindrop = function
    | x when ((x mod 3)=0) -> "Pling"^(raindrop x)
    | x when ((x mod 5)=0) -> "Plang"^(raindrop x)
    | x when ((x mod 7)=0) -> "Plong"^(raindrop x)
    | x -> string_of_int x




let mod7 s = function 
  | x when ((x mod 7)=0) -> s^"Plong"
  | x -> s

let mod5 s = function 
  | x when ((x mod 5)=0) -> (mod7 (s^"Plang") x)
  | x -> s

let raindrop = function
  | x when ((x mod 3)=0) -> (mod5 "Pling" x)
  | x when ((x mod 5)=0) -> (mod7 "Plang" x)
  | x when ((x mod 7)=0) -> "Plong"
  | x -> string_of_int x

open Base
let l = String.split_on_chars "Complementary metal-oxide semiconductor" ~on:[' ';'-']

let first s = s.[0]

List.map l ~f:first

let acronym s = List.map (List.map (String.split_on_chars s ~on:[' ';'-']) ~f:first) ~f:Char.uppercase |>  Base.String.of_char_list ;;

let negated f = (fun x -> not (f x));;

List.filter ~f:(negated (String.equal "")) ["Something"; ""; ""; "I"; "made"; "up"; "from"; "thin"; "air"];;


let acronym s = List.map (List.map (List.filter ~f:(negated (String.equal "")) (String.split_on_chars s ~on:[' ';'-';'_'])) ~f:first) ~f:Char.uppercase |>  Base.String.of_char_list ;;

let acronym s =
  String.split_on_chars s ~on:[' ';'-';'_'] |>
  List.filter ~f:(negated (String.equal "")) |>
  List.map ~f:first |>
  List.map ~f:Char.uppercase |>
  Base.String.of_char_list

type allergen = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats

let l = [1; 2; ] 
let resta x y = if x mod y=0 then x-y else 1

let allergic_to x = function
  | Eggs -> if x mod 1=0 then true else false
  | Peanuts -> if x mod 2=0 then true else false
  | Shellfish -> if x mod 4=0 then true else false
  | Strawberries -> if x mod 8=0 then true else false
  | Tomatoes -> if x mod 16=0 then true else false
  | Chocolate -> if (x mod 32=0 || (resta x 2) mod 32=0) then true else false
  | Pollen -> if x mod 64=0 then true else false
  | Cats -> if x mod 128=0 then true else false

List.range 0 8 |> List.map ~f:(fun x -> 2 ** x);;
(* - : int list = [1; 2; 4; 8; 16; 32; 64; 128] *)
allergic_to 34 Peanuts;;
(* - : bool = true *)
allergic_to 34 Chocolate;;
(* - : bool = true *)
allergic_to 33 Chocolate;;
(* - : bool = false *)
