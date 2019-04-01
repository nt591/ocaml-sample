
(* helpers *)
let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *)
let (--) i j =
  from i j []

let odd n = n mod 2 <> 0

(* helpers end *)

let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

(* quad and fourth are functions because OCaml functions are curried. "quad" and "fourth" are partially applied, meaning they return functions that accept another arg  *)

let ($) f x = f x

(* infix operator that applies the left arg as a function to everything on the right *)

(* square $ 2 + 2 = 16 *)
(* (square $ 2) + 2 = 6 *)

let (@@) f g x = x |> g |> f
(* infix operator that takes two functions, left and right, and composes (applying g first) *)

(* Generalize twice to a function repeat, such that repeat f n x applies f to x a total of n times. That is,

repeat f 0 x yields x
repeat f 1 x yields f x
repeat f 2 x yields f (f x) (which is the same as twice f x)
repeat f 3 x yields f (f (f x))
 *)

let rec repeat n f x = match n with
  | 1 -> f x
  | _ -> f (repeat (n - 1) f x)

(* Use fold_left to write a function product_left that computes the product of a list of floats.
The product of the empty list is 1.0. Hint: recall how we implemented sum in just one line of code in lecture. *)

let product_left = List.fold_left (fun a b -> a *. b) 1.0
(* explicitly: let product_left lst = List.fold_left (fun a b -> a *. b) 1.0 lst *)


(* Use fold_right to write a function product_right that computes the product of a list of floats. Same hint applies. *)

let product_right lst = List.fold_right (fun a b -> a *. b) lst 1.0


(* Given the following function clip, write a function cliplist that clips every integer in its input list. *)
let clip n =
if n < 0 then 0
else if n > 10 then 10
else n

(* Write two version of cliplist: one that uses map, and another that is a direct recursive implementation. *)

let cliplist = List.map clip

let rec cliplist' lst = match lst with
  | [] -> []
  | h::t -> (clip h)::(cliplist t)

(* Write a function sum_cube_odd n that computes the sum of the cubes of all the odd numbers between 0 and n inclusive.
Do not write any new recursive functions. Instead, use the functionals map, fold, and filter, and the (--) operator defined in the lecture notes. *)

let sum_cube_odd n = List.fold_left (+) 0 (List.map (fun x -> x * x * x) (List.filter odd (0--n)))

(* Rewrite the function sum_cube_odd to use the pipeline operator |> as shown in the lecture notes for this lab in the section titled "Pipelining". *)

let sum_cube_odd' n =
  (0--n)
  |> List.filter odd
  |> List.map (fun x -> x * x * x)
  |> List.fold_left (+) 0

(* Consider writing a function exists: ('a -> bool) -> 'a list -> bool, such that exists p [a1; ...; an] returns whether at least one element of the list satisfies the predicate p.
That is, it evaluates the same as (p a1) || (p a2) || ... || (p an). When applied to an empty list, it evaluates to false.

Write three solutions to this problem, as we did above:

exists_rec, which must be a recursive function that does not use the List module,
exists_fold, which uses either List.fold_left or List.fold_right, but not any other List module functions nor the rec keyword, and
exists_lib, which uses any combination of List module functions other than fold_left or fold_right, and does not use the rec keyword.*)

let rec exists_rec pred lst = match lst with
  | [] -> false
  | h::t -> pred h || exists_rec pred t

let exists_fold pred = List.fold_left (fun l h -> pred h || l) false

let exists_lib pred = List.exists pred

(* Write a function which, given a list of numbers representing expenses, removes them from a budget,
and finally returns the remaining amount in the budget.
Write three versions: fold_left, fold_right, and a direct recursive implementation. *)

let expense_left lst budget = List.fold_left (-) budget lst

let expense_right lst budget = List.fold_right (fun exp acc -> acc - exp) lst budget

let rec expense_rec lst budget = match lst with
  | [] -> budget
  | h::t -> (-h) + (expense_rec t budget)

let expense_tco lst budget =
  let rec expenses acc l = match l with
    | [] -> acc
    | h::t -> expenses (acc + h) t
  in budget - (expenses 0 lst)


(* Write a function uncurry that takes in a curried function and returns the uncurried version of that function.
Remember that curried functions have types like 'a -> 'b -> 'c, and the corresponding uncurried function will have the type 'a * 'b -> 'c.
Therefore uncurry should have the folowing type:

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
*)

let uncurry f (a, b) = (f a b)

(* Write the inverse function curry. It should have the following type:
  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
 *)

 let curry f a b = f (a, b)

 (* How terse can you make your solutions to the product exercise?
 Hints: you need only one line of code for each, and you do not need the fun keyword.
 For fold_left, your function definition does not even need to explicitly take a list argument.
 If you use ListLabels, the same is true for fold_right. *)

 let product = List.fold_left ( * ) 1

 (* Show how to replace any expression of the form List.map f (
   List.map g lst) with an equivalent expression that calls List.map only once. *)

let fn f g = List.map (g |> f)