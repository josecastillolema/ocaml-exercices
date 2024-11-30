let rec compress = function
  | a :: (b :: _ as t) when a = b -> compress t
  | a :: t -> a :: compress t
  | l -> l

let () =
  assert (
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ])
