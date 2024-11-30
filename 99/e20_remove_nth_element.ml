let remove_at n l = E17_split_list.pre l n @ E17_split_list.pos l (n + 1)
let () = assert (remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ])
