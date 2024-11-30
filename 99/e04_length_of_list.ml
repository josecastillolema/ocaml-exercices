let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t
[@@ocamlformat "disable"]

let () =
  assert (length [ "a"; "b"; "c" ] = 3);
  assert (length [] = 0)