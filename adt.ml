let pi = 4.0 *. Float.atan 1.0

type point = float * float

type shape =
  | Point of point
  | Circle of point * float
  | Rect of point * point

let area = function
  | Point _ -> 0.0
  | Circle (_,r) -> pi *. (r ** 2.)
  | Rect ((x1, y1), (x2, y2)) ->
    let w = x2 -. x2 in
    let h = y2 -. y1 in
      h *. w

let center = function
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2)) ->
    (
      (x2 -. x1) /. 2.,
      (y2 -. y1) /. 2.
    )

type string_or_int =
  | String of string
  | Int of int

type string_or_int_list = string_or_int list

let rec sum : string_or_int_list -> int = function
  | [] -> 0
  | (String s)::t -> int_of_string s + sum t
  | (Int i)::t -> i + sum t

type t = Left of int | Right of int

let x = Left 1

let y = Right 2

let double_right = function
  | Left i -> i
  | Right i -> 2 * i

(* recursive variants *)

type intlist = Nil | Cons of int * intlist
let lst3 = Cons (3, Nil)  (* similar to 3::[] or [3]*)
let lst123 = Cons(1, Cons(2, Cons(3, Nil))) (* similar to [1;2;3] *)

let rec sum (lst: intlist) =
  match lst with
  | Nil -> 0
  | Cons (h, t) -> h + sum t

let rec length (lst: intlist) =
  match lst with
  | Nil -> 0
  | Cons (h, t) -> 1 + length t
