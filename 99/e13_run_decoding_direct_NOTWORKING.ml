let rec count x = function
  | [] ->
      print_endline "o0";
      0
  | [ h ] when h = x ->
      print_endline "o1";
      1
  | [ h ] when h != x ->
      print_endline "o2";
      0
  | h1 :: h2 :: t when h1 = h2 && h1 = x ->
      print_endline "o3";
      1 + count x (h2 :: t)
  | h1 :: h2 :: t when h1 != h2 && h1 = x ->
      print_endline "o4";
      1
  | h1 :: h2 :: t when h1 != h2 && h1 != x ->
      print_endline "o5";
      0

let encode l = List.map (fun x -> (count x l, x)) l

let () =
  assert (
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ])
