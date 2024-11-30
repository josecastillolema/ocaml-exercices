type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One x::t -> x::(flatten t)
  | Many l::t -> flatten l @ flatten t

let () =
  assert (
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ])
