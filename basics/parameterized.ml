type 'a mylist = Nil | Cons of 'a * 'a mylist

let lst3 = Cons (3, Nil)

let lstABC = Cons("a", Cons("b", Cons("c", Nil)))


(* type signature is optional - compiler would infer based on + sign *)
let rec sum : int mylist -> int = function
  | Nil -> 0
  | Cons(h, t) -> h + sum t

let rec length : 'a mylist -> int = function
  | Nil -> 0
  | Cons(h, t) -> h + length t

let empty : 'a mylist -> bool = function
  | Nil -> true
  | Cons(_,_) -> false