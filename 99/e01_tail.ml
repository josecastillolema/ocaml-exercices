let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t

let () =
  assert (last [ 1; 2; 3; 4 ] = Some 4);
  assert (last [] = None)

let a = if true then 3 else 4
