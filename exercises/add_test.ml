open OUnit2
open Add

let tests = "testing add function" >::: [
  "it works without a multiple of 7 in the x position" >:: (fun _ -> assert_equal 3 (add 1 2));
  "it breaks with a multiple of 7 in the x position" >:: (fun _ -> assert_equal 17 (add 14 3));
]

let _ = run_test_tt_main tests