let rec range_inc x = function
  | y when x>y -> []
  | y -> x::(range_inc (x+1) y)
[@@ocamlformat "disable"]

let rec range_decr x = function
  | y when x<y -> []
  | y -> x::(range_decr (x-1) y)
[@@ocamlformat "disable"]

let range x y = 
  if x < y
    then range_inc x y
    else range_decr x y
[@@ocamlformat "disable"]

let () =
  assert (range 4 9 = [ 4; 5; 6; 7; 8; 9 ]);
  assert (range 9 4 = [ 9; 8; 7; 6; 5; 4 ])
