(*** Exercise: values *)
let _:int = 7*(1+2+3)

let _:string = "CS " ^ string_of_int 3110

(*** Exercise: operators *)
let _ = 42 * 10

let _ = 3.14 *. 2.

let _ = 4.2 ** 7.

(*** Exercise: equality *)
let _ = 42 = 42

let _ = "hi" = "hi"

let _ = "hi" == "hi"    (* - : bool = false *)

(*** Exercise: assert *)
let _ = assert true

let _ = assert false   (* Exception: Assert_failure ("//toplevel//", 1, 0). *)

let _ = assert (2110 <> 3110)

(*** Exercise: if *)
let _ = if 42>2 then 1 else 7

(*** Exercise: double fun *)
let inc x = x + 1

let rec apply n f x =
  if n>0 then apply (n-1) f (f x)
  else x

let double x = apply x inc x  

let _ = assert ((double 7)=14)

(*** Exercise: more fun *)
let cube x = x *. x *. x

let cube' x = x ** 3.

let _ = assert ((cube 2.)=8.)

let _ = assert ((cube' 2.)=8.);;

let sign x =
  if x<0 then -1
  else if x>0 then 1
  else 0

let _ = assert (sign (-10) = -1)

let _ = assert (sign 10 = 1)

let _ = assert (sign 0 = 0)

let area r = Float.pi*.r**2.

let _ = assert ((area 5.) = 78.5398163397448315)

(*** Exercise: RMS *)
let rms x y = sqrt((x*.x+.y*.y)/.2.)

let _ = assert ((rms 2. 3.)=2.54950975679639225)

(*** Exercise: date fun  *)
let is_valid_date d m =
  if (d >= 1 && d<=31) && List.mem m ["Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec"] ||
     (d >= 1 && d<=30) && List.mem m ["Apr"; "Jun"; "Sept"; "Nov"] ||
     (d >= 1 && d<=28) && m = "Feb"
    then true
  else false

let is_valid_date' d m = match m with
  | "Feb" -> 1 <= d && d <= 28
  | "Apr" | "Jun" | "Sep" | "Nov" -> 1 <= d && d <= 30
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 1 <= d && d <= 31
  | _ -> false

let is_valid_date'' d m =
  let months = [("Jan",31); ("Feb",28); ("Mar",31)] in
    match List.mem_assoc m months with
      | true -> d >= 1 && d < List.assoc m months
      | _ -> false

(*** Exercise: fib *)
let rec fib = function
  | 1|2 -> 1
  | n   -> fib (n-1) + fib (n-2)

let _ = fib 10      (* - : int = 55 *)

(*** Exercise: fib fast *)
let rec h n pp p = match n with
  | 1 -> p
  | n -> h (n-1) p (pp+p)
let fib_fast n = h n 0 1

let _ = fib_fast 50      (* - : int = 12586269025 *)

let rec overflow x =
  if sign (fib_fast x)=1 then overflow x+1
  else x

let _ = overflow 0

(*** Exercise: poly types *)
let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y

(*** Exercise: divide *)
let divide ~numerator ~denominator = numerator /. denominator

let _ = divide ~numerator:10. ~denominator:2.

(*** Exercise: associativity *)
let add x y = x + y

(* let _ = add 5 1 *)

(* let _ = add 5 *)

(* let _ = (add 5) 1 *)

(* let _ = add (5 1) *)
(* Error: This expression has type int
          This is not a function; it cannot be applied. *)

(*** Exercise: average *)
(* let ( +/. ) x y = (x +. y)/.2. *)

(* let _ = 1.0 +/. 2.0 *)    (* - : float = 1.5 *)

(* let _ = 0. +/. 0. *)      (* - : float = 0. *)

(*** Exercise: hello world *)
print_endline "Hello world!"

print_string "Hello world!"