open OUnit2
open Valid

let tests = "test suite for valid" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (product []));
  "singleton" >:: (fun _ -> assert_equal 5 (product [5]));
  "two_elements" >:: (fun _ -> assert_equal 6 (product [2; 3]));
]

let _ = run_test_tt_main tests