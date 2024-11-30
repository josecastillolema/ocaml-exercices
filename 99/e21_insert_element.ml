let insert_at x n l = E17_split_list.pre l n @ [ x ] @ E17_split_list.pos l n

let () =
  assert (
    insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ])
