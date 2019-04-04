let double x = 2 * x

let twice f x= f (f x)

let square x = x * x
let fourth x = twice square x

let apply x f = f x

let pipeline x f = f x
(* parens define infix operator *)
let (|>) = pipeline
let x = 5 |> double

let compose f g x  = f (g x)
let square_then_double = compose double square

let both f g x = (f x, g x)
let ds = both double square
let z = ds 6

let cond p f g x =
  if p then f x else g x

(* map *)

let rec map f = function
  | [] -> []
  | h::t -> (f h)::(map f t)

(* map double [1;2;3;4] *)

(* filter *)

let even n =
  n mod 2 = 0

let odd n =
  n mod 2 <> 0

let rec filter f = function
  | [] -> []
  | h::t -> if (f h) = true then h::(filter f t) else filter f t

(* filter odd [1;2;3;4;5;6;] *)
(* let filterOdd = filter odd *)

(* fold right is basically reduce *)

let rec sum = function
  | [] -> 0
  | h::t -> h + sum t

let rec concat = function
  | [] -> ""
  | h::t -> h ^ concat t

(* differences are the base case, and the operating function inside (+ vs ^) *)
(* absract out the base case *)
let rec sum' init = function
  | [] -> init
  | h::t -> h + sum' init t

let sum = sum' 0

let rec concat' init = function
  | [] -> init
  | h::t -> h ^ concat' init t

let concat = concat' ""

let rec combine op init = function
  | [] -> init
  | h::t -> op h (combine op init t)

let sum    = combine (+) 0
let concat = combine (^) ""

(* Fold right is similar but order of args is different *)

let rec fold_right op lst init = match lst with
  | [] -> init
  | h::t -> op h (fold_right op t init)

let sum lst = fold_right (+) lst 0
let concat lst = fold_right (^) lst ""

(* fold left *)

(* The idea is that fold_left (+) 0 [a;b;c] results in evaluation of ((0+a)+b)+c. The parentheses associate from the left-most subexpression to the right.
So fold_left is "folding in" elements of the list from the left to the right, combining each new element using the operator. *)

(* this is more standard reduce - take the left most element, apply the function to the accumulator, move down the list *)
let rec fold_left op acc = function
  | [] -> acc
  | h::t -> fold_left op (op acc h) t

(* let sum = fold_left (+) 0 *)

(* Notes from text *)

(* Having built both fold_right and fold_left, it's worthwhile to compare and contrast them.
The immediately obvious difference is the order in which they combine elements from the list: right to left vs. left to right.
When the operator being used to combine elements is associative, that order doesn't doesn't change the final value of the computation.
But for non-associative operators like (-), it can: *)

(* # List.fold_right (-) [1;2;3] 0;;  (* 1 - (2 - (3 - 0)) *)
- : int = 2
# List.fold_left (-) 0 [1;2;3];;   (* ((0 - 1) - 2) - 3 *)
- : int = -6 *)

(* A second difference is that fold_left is tail recursive whereas fold_right is not.
So if you need to use fold_right on a very lengthy list, you may instead want to reverse the list first then use fold_left;
the operator will need to take its arguments in the reverse order, too: *)

(*
# List.fold_right (fun x y -> x - y) [1;2;3] 0;;
- : int = 2

# List.fold_left (fun y x -> x - y) 0 (List.rev [1;2;3]);;
- : int = 2

# List.fold_left (fun x y -> y - x) 0 (List.rev (0--1_000_000));;
- : int = 500000

# List.fold_right (fun y x -> x - y) (0--1_000_000) 0;;
Stack overflow during evaluation (looping recursion?)
 *)


(* Using Fold to Implement Other Function: This is super frickin cool *)
(* These don't work with Base due to different function signatures *)

let length l = List.fold_left (fun a _ -> a+1) 0 l
let reverse l = List.fold_left (fun a x -> x::a) [] l
let map f l = List.fold_right (fun x a -> (f x)::a) l []
let filter f l = List.fold_right (fun x a -> if f x then x::a else a) l []

(* folding over a tree *)

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec foldtree init op = function
  | Leaf -> init
  | Node (v, l, r) -> op v (foldtree init op l) (foldtree init op r)

let size t = foldtree 0 (fun _ l r -> 1 + l + r) t
let depth t = foldtree 0 (fun _ l r -> 1 + max l r) t
let preorder t = foldtree [] (fun x l r -> [x] @ l @ r) t

let t = Node(
  10,
  Node(1, Leaf, Leaf),
  Node(
    3,
    Node(
      3,
      Leaf,
      Leaf
    ),
  Leaf
  )
)

(* Generalized Folds: todo *)

(* write a recursive fold function that takes in one argument for each constructor of t. *)

(* That fold function matches against the constructors, calling itself recursively on any value of type t that it encounters. *)

(* Use the appropriate argument of fold to combine the results of all recursive calls as well as all data not of type t at each constructor. *)

(* pipeline *)

let square x = x * x
let sum = List.fold_left (+) 0

let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *)
let (--) i j =
  from i j []

let sum_sq n =
  0--n
  |> List.map square
  |> sum