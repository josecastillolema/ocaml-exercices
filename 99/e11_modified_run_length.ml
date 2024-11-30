type 'a rle =
  | One of 'a
  | Many of int * 'a
[@@ocamlformat "disable"]

let classify l =
  match List.length l with
  | 1 -> One (List.hd l)
  | _ -> Many (List.length l, List.hd l)

let encode l = E09_pack_duplicates.pack l |> List.map (fun l -> classify l)

let () =
  assert (
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ])
