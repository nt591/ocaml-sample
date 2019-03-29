(* Old approach - defining a type that I may never reference again *)

(* type fin_or_inf = Finite of int | Infinity

let f = function
  | 0 -> Infinity
  | 1 -> Finite 1
  | n -> Finite (-n) *)

  (* New super cool approach *)

let f = function
  | 0 -> `Infinity
  | 1 -> `Finite 1
  | n -> `Finite (-n)


(* Merlin is angry with a syntax error? *)
match f 3 with
  | `Finite n -> "Finite"
  | `Infinity -> "Infinite"