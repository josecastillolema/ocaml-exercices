let rec times x = function
  | 0 -> []
  | 1 -> [ x ]
  | n -> x::(times x (n-1))
[@ocamlformat "disable"]

let rec replicate l n =
  match l with
  | [] -> []
  | h :: t -> times h n @ (replicate t n)
[@ocamlformat "disable"]

let () =
  assert (
    replicate [ "a"; "b"; "c" ] 3
    = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ])
