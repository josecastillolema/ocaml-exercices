let rec pre l = function
  | 0 -> []
  | n when n > List.length l -> l
  | n -> List.hd l :: pre (List.tl l) (n - 1)

let rec pos l = function
  | 0 -> l
  | n when n > List.length l -> []
  | n -> pos (List.tl l) (n - 1)

let rec split l n = (pre l n, pos l n)

let () =
  assert (
    split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]));
  assert (split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []))
