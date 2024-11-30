let rotate l n = E17_split_list.pos l n @ E17_split_list.pre l n

let () =
  assert (
    rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
    = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ])
