let rec is_palindrome l =
  l = List.rev l
[@@ocamlformat "disable"]

let () =
  assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true);
  assert ((not (is_palindrome [ "a"; "b" ])) = true)
