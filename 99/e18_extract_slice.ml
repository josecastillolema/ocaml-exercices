let rec slice l x y = E17_split_list.pre (E17_split_list.pos l x) (y - x + 1)

let () =
  assert (
    slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
    = [ "c"; "d"; "e"; "f"; "g" ]);
  assert (
    slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 1 4
    = [ "b"; "c"; "d"; "e" ])
