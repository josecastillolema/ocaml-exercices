let rec join (l : 'a list list) : 'a list list =
  match l with
  | (h1 :: _ as l1) :: (h2 :: _ as l2) :: t when h1 = h2 -> [ l1 @ l2 ] @ join t
  | a :: t -> [ a ] @ join t
  | _ -> []

let rec div (l : 'a list) : 'a list list =
  match l with
  | a :: b :: t when a = b -> [ [ a ] @ [ b ] ] @ div t
  | a :: t -> [ [ a ] ] @ div t
  | _ -> []

let pack l = div l |> join

let () =
  assert (
    pack
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [
        [ "a"; "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d"; "d" ];
        [ "e"; "e"; "e"; "e" ];
      ])
