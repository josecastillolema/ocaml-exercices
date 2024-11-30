type 'a rle =
  | One of 'a
  | Many of int * 'a
[@@ocamlformat "disable"]

let declassify = function
  | One x -> [ x ]
  | Many (n, x) -> List.init n (fun _ -> x)

let decode l = List.map (fun x -> declassify x) l |> List.flatten

let () =
  assert (
    decode
      [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]
    = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
