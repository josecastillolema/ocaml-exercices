let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [h]
[@ocamlformat "disable"]

let () = assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ])
