let drop l n =
  let rec aux l x =
    match (l, x) with
    | [], _ -> []
    | _, 1 -> aux (List.tl l) n
    | _, x -> List.hd l :: aux (List.tl l) (x - 1)
  in
  aux l n

let () =
  assert (
    drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ])
