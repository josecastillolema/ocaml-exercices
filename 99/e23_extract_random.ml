open Random

let rec rand_select l = function
  | 0 -> []
  | n ->
      Random.init 0;
      List.nth l (Random.int (List.length l)) :: rand_select l (n - 1)

let () =
  assert (
    rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 = [ "h"; "a"; "g" ])
