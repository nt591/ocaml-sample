(* Setup *)

module type Queue = sig
  (* An ['a queue] is a queue whose elements have type ['a]. *)
  type 'a queue

  (* The empty queue. *)
  val empty : 'a queue

  (* Whether a queue is empty. *)
  val is_empty : 'a queue -> bool

  (* [enqueue x q] is the queue [q] with [x] added to the end. *)
  val enqueue : 'a -> 'a queue -> 'a queue

  (* [peek q] is [Some x], where [x] is the element at the front of the queue,
     or [None] if the queue is empty. *)
  val peek : 'a queue -> 'a option

  (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
     of [q] except the front of [q], or [None] if [q] is empty. *)
  val dequeue : 'a queue -> 'a queue option
end

module ListQueue : Queue = struct
  (* Represent a queue as a list.  The list [x1; x2; ...; xn] represents
     the queue with [x1] at its front, followed by [x2], ..., followed
     by [xn]. *)
  type 'a queue = 'a list

  let empty = []

  let is_empty q = q = []

  let enqueue x q = q @ [x]

  let peek = function
    | [] -> None
    | x::_ -> Some x

  let dequeue = function
    | [] -> None
    | _::q -> Some q
end

module TwoListQueue : Queue = struct
  (* [{front=[a;b]; back=[e;d;c]}] represents the queue
     containing the elements a,b,c,d,e. That is, the
     back of the queue is stored in reverse order.
     [{front; back}] is in *normal form* if [front]
     being empty implies [back] is also empty.
     All queues passed into or out of the module
     must be in normal form. *)
  type 'a queue = {front:'a list; back:'a list}

  let empty = {front=[]; back=[]}

  let is_empty = function
    | {front=[]; back=[]} -> true
    | _ -> false

  (* Helper function to ensure that a queue is in normal form. *)
  let norm = function
    | {front=[]; back} -> {front=List.rev back; back=[]}
    | q -> q

  let enqueue x q = norm {q with back=x::q.back}

  let peek = function
    | {front=[]; _} -> None
    | {front=x::_; _} -> Some x

  let dequeue = function
    | {front=[]; _} -> None
    | {front=_::xs; back} -> Some (norm {front=xs; back})
end

(* Use the following code to create ListQueue's of exponentially increasing length:
10, 100, 1000, etc.
How big of a queue can you create before there is a noticeable delay?
How big until there's a delay of at least 10 seconds? (Note: you can abort utop computations with Ctrl-C.) *)

(* Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n=0 then q
    else loop (n-1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(* Write a module BstDict that implements the Dictionary module type using the tree type. *)

module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty  : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings
   * as [d] and also a mapping from [k] to [v], even if [k] was already
   * mapped in [d]. *)
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t

  (* [lookup k d] returns the value associated with [k] in [d].
   * raises:  [Not_found] if [k] is not mapped to any value in [d]. *)
  val lookup  : 'k -> ('k,'v) t -> 'v
end

type ('k, 'v) tree =
  | Leaf
  | Node of ('v * ('k, 'v) tree * ('k, 'v) tree)

(* module BstDict : Dictionary = struct
  type ('k, 'v) t = ('k, 'v) tree

  let empty = (None, Leaf, Leaf)

  let lookup k t = function

end *)

(* Here is a signature and a structure for complex numbers, which have a real and imaginary component: *)

module type ComplexSig = sig
  val zero : float*float
  val add : float*float -> float*float -> float*float
end

module Complex = struct
  let zero = 0., 0.
  let add (r1,i1) (r2,i2) = r1 +. r2, i1 +. i2
end

(* Improve that code by adding type t = float*float to both the structure and signature.
Show how the signature can be written more tersely because of the type synonym.
 *)

module type ComplexSig = sig
  type t
  val zero : float*float
  val add : float*float -> float*float -> float*float
end

module Complex : ComplexSig = struct
  type t = (float*float)
  let zero = 0., 0.
  let add (r1,i1) (r2,i2) = r1 +. r2, i1 +. i2
end

(* Write a module that implements the Fraction module type below: *)

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator   : t -> int
  val denominator : t -> int
  val toString    : t -> string
  val toReal      : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction : Fraction = struct
  type t = (int * int)

  let make x y = (x, y)

  let numerator = function
    | (x, _) -> x

  let denominator = function
    | (_, y) -> y

  let toString (x,y) = string_of_int(x) ^ "/" ^ string_of_int(y)
  let toReal (x,y) = float_of_int(x) /. float_of_int(y)

  let add (x1,y1) (x2, y2) = ((x1 * y2 + x2 * y1), (y1 * y2))
  let mul (x1, y1) (x2, y2) = ((x1 * x2), (y1 * y2))
end

(* Exercise: date order [✭✭]
Here is a type for dates: *)

type date = { month:int; day:int }
(* For example, March 31st would be represented as {month=3; day=31}. Our goal in the next few exercises is to implement a map whose keys have type date.

Obviously it's possible to represent invalid dates with type date—for example, { month=6; day=50 } would be June 50th, which is not a real date.
 The behavior of your code in the exercises below is unspecified for invalid dates.

To create a map over dates, we need a module that we can pass as input to Map.Make. That module will need to match the Map.OrderedType signature.
 Create such a module. Here is some code to get you started: *)

module Date = struct
  type t = date
  let compare date1 date2 =
    let compare' x y = if x = y then 0 else if x > y then 1 else -1 in
    match (date1, date2) with
      | {month = m1; day = d1}, {month = m2; day = d2} when m1 = m2 -> compare' d1 d2
      | {month = m1; day = d1}, {month = m2; day = d2} -> compare' m1 m2
end
(* Recall the specification of compare in Map.OrderedType as you write your Date.compare function. *)

(* omg it worked *)


(* Functors *)

(* Our goal in the next series of exercises is to write a functor that, given a module supporting a to_string function, r
returns a module supporting a print function that prints that string.*)

(* Write a module type ToString that specifies a signature with an abstract type t and a function to_string : t -> string. *)

module type ToString = sig
  type t

  val to_string : t -> string
end

(* Write a functor Print that takes as input a module named M of type ToString.
The structure returned by your functor should have exactly one value in it, print, which is a function that takes a value of type M.t
and prints a string representation of that value.
*)

module Print (M:ToString) = struct
  let print x = M.to_string x
end

(* Create a module named PrintInt that is the result of applying the functor Print to a new module Int.
 You will need to write Int yourself. The type Int.t should be int. Hint: do not seal Int.
 *)

module Int = struct
  type t = int

  let to_string x = string_of_int x
end

module PrintInt = Print(Int)

(* Create a module named PrintString that is the result of applying the functor Print to a new module MyString.
You will need to write MyString yourself. Hint: do not seal MyString.
*)

module MyString = struct
  type t = string

  let to_string x = x
end

module PrintString = Print(MyString)

(* Define a module StringWithPrint. It should have all the values of the built-in String module.
It should also have the print operation, which should be derived from the Print functor rather than being copied code.
Hint: use two include statements.
*)

module StringWithPrint = struct
  include PrintString
  include String
end