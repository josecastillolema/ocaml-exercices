let rec nth l = function
  | 0 -> List.hd l
  | n -> ( try nth (List.tl l) (n - 1) with e -> raise (Failure "nth"))

let () =
  assert (nth [ "a"; "b"; "c"; "d"; "e" ] 2 = "c");
  assert (
    (try nth [ "a" ] 2 with Failure msg -> "Failure " ^ msg) = "Failure nth")
