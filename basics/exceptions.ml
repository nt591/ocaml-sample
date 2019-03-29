(* lots of notes at http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/exceptions.html *)
(* Code doesn't seem to work though *)

open OUnit2

let tests = "suite" >:::
  [
    "empty"    >:: (fun _ -> assert_raises (Failure "hd") (fun () -> List.hd []));
    "nonempty" >:: (fun _ -> assert_equal  1              (List.hd [1]));
  ]

let _ = run_test_tt_main tests