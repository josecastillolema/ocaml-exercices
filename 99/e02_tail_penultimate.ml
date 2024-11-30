let rec last_two = function
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let () =
  assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
  assert (last_two [ "a" ] = None)
