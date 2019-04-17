(* Advanced Data Structures *)

(* Infinite Data Structures - we can define things recursively! *)

let rec ones = 1 :: ones
(* val ones : int list = [1; <cycle>] *)

let rec a = 1::b and b = 0::a

(*
val a : int list = [0; 1; <cycle>]
val b : int list = [1; 0; <cycle>]
*)

(* These are called recursive values - implemented as a linked list with a back pointer to the head of the list *)

(* STREAMS *)

(* What we need is a way to pause evaluation, so that at any point in time,
only a finite approximation to the infinite sequence has been computed. Fortunately, we already know how to do that! *)

(* An ['a stream] is an infinite list of values of type ['a].
 * AF:  [Cons (x, f)] is the stream whose head is [x] and tail is [f()].
 * RI:  none.
 *)
type 'a stream =
  Cons of 'a * (unit -> 'a stream)

let rec from n = Cons (n, fun () -> from (n+1))

(* [hd s] is the head of [s] *)
let hd (Cons (h, _)) = h

(* [tl s] is the tail of [s] *)
let tl (Cons (_, tf)) = tf ()

(* [take n s] is the list of the first [n] elements of [s] *)
let rec take n s =
  if n = 0 then []
  else hd s :: take (n - 1) (tl s)

(* [drop n s] is all but the first [n] elements of [s] *)
let rec drop n s =
  if n = 0 then s
  else drop (n - 1) (tl s)

(* function to square a stream *)

let rec square (Cons (h, tf)) = Cons(h * h, fun () -> square(tf()))

(* function to add two streams *)
(* Cons (h1+h2, fun () -> sum (tf1 ()) (tf2 ())) *)
let rec sum (Cons(h1, tf1)) (Cons(h2, tf2)) = Cons(h1 + h2, fun () -> sum (tf1 ()) (tf2 ()))

(* mapping works with the same pattern - apply to head, delay and apply to tail in a thunk and wrap in Cons *)
let rec map f (Cons(h, tf)) = Cons(f h, fun () -> map f (tf()))
let square' = map (fun x -> x * x)

let rec fibs =
  Cons(1, fun () ->
    Cons(1, fun () ->
      sum fibs (tl fibs)))

(* inefficient because recalculates all the fib numbers as it descends down the list *)

(* Laziness *)

(* The example with the Fibonacci sequence demonstrates that it would be useful if the computation of a thunk happened only once:
when it is forced, the resulting value could be remembered, and if the thunk is ever forced again,
that value could immediately be returned instead of recomputing it. That's the idea behind the OCaml Lazy module:
*)

(* compare *)
let fib30long = take 30 fibs |> List.rev |> List.hd

(* with *)

let fib30lazy = lazy (take 30 fibs |> List.rev |> List.hd)
let fib30 = Lazy.force fib30lazy

(* But if we ever try to recompute that same lazy value, it will return immediately, because the result has been memoized: *)

let fib30fast = Lazy.force fib30lazy

(* Now how can we use the Lazy OCaml module to make streams? *)

type 'a lazystream =
  Cons of 'a * 'a lazystream Lazy.t

(* The following two modules implement the Fibonacci sequence with streams, then with lazy streams.
Try computing the 30th Fibonacci number with both modules, and you'll see that the lazy streams implementation
is much faster than the standard streams. *)

module StreamFibs = struct
  type 'a stream =
    | Cons of 'a * (unit -> 'a stream)

  let hd : 'a stream -> 'a =
    fun (Cons (h, _)) -> h

  let tl : 'a stream -> 'a stream =
    fun (Cons (_, t)) -> t ()

  let rec take_aux n (Cons (h, t)) lst =
    if n = 0 then lst
    else take_aux (n-1) (t ()) (h::lst)

  let take : int -> 'a stream -> 'a list =
    fun n s -> List.rev (take_aux n s [])

  let nth : int -> 'a stream -> 'a =
    fun n s -> List.hd (take_aux (n+1) s [])

  let rec sum : int stream -> int stream -> int stream =
    fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
      Cons (h_a + h_b, fun () -> sum (t_a ()) (t_b ()))

  let rec fibs =
    Cons(1, fun () ->
        Cons(1, fun () ->
            sum (tl fibs) fibs))

  let nth_fib n =
    nth n fibs

end

module LazyFibs = struct

  type 'a lazystream =
    | Cons of 'a * 'a lazystream Lazy.t

  let hd : 'a lazystream -> 'a =
    fun (Cons (h, _)) -> h

  let tl : 'a lazystream -> 'a lazystream =
    fun (Cons (_, t)) -> Lazy.force t

  let rec take_aux n (Cons (h, t)) lst =
    if n = 0 then lst
    else take_aux (n-1) (Lazy.force t) (h::lst)

  let take : int -> 'a lazystream -> 'a list =
    fun n s -> List.rev (take_aux n s [])

  let nth : int -> 'a lazystream -> 'a =
    fun n s -> List.hd (take_aux (n+1) s [])

  let rec sum : int lazystream -> int lazystream -> int lazystream =
    fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
      Cons (h_a + h_b, lazy (sum (Lazy.force t_a) (Lazy.force t_b)))

  let rec fibs =
    Cons(1, lazy (
        Cons(1, lazy (
            sum (tl fibs) fibs))))

  let nth_fib n =
    nth n fibs
end

(* A binary search tree (BST) is a binary tree with the following representation invariant:

For any node n, every node in the left subtree of n has a value less than n's value, and every node in the right subtree of n has a value greater than n's value.

We call that the BST invariant.

Here is code that implements a couple operations on a BST: *)

type 'a tree =
  | Leaf
  | Node of ('a * 'a tree * 'a tree)

(**  mem x t] is [true] iff [x] is a member of [t] **)
let rec mem x = function
  | Leaf -> false
  | Node (v, left, right) ->
    x = v || (x < v && mem x left) || mem x right

(** [insert x t] is [t] . **)
let rec insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (v, left, right) as t ->
    if x = v then t
    else if x < v then Node(v, insert x left, right)
    else Node (v, left, insert x right)

(* Refs *)

(* A ref is like a pointer or reference in an imperative language. It is a location in memory whose contents may change.
Refs are also called ref cells, the idea being that there's a cell in memory that can change. *)

let x = ref 0;;
(* val x : int ref = {contents = 0} *)
!x;;
(* - : int = 0 *)
x := 1;;
!x;;
(* - : int = 1 *)

(* the ! character is the dereference operation in OCaml *)

(*

Now that we have refs, we have aliasing: two refs could point to the same memory location, hence updating through one causes the other to also be updated. For example,

let x = ref 42
let y = ref 42
let z = x
let () = x := 43
let w = (!y) + (!z)
The result of executing that code is that w is bound to 85, because let z = x causes z and x to become aliases, hence updating x to be 43 also causes z to be 43.

 *)

(*

The semantics of refs is based on locations in memory. Locations are values that can be passed to and returned from functions.
But unlike other values (e.g., integers, variants), there is no way to directly write a location in an OCaml program.
That's different than languages like C, in which programmers can directly write memory addresses and do arithmetic on pointers.
C programmers want that kind of low-level access to do things like interface with hardware and build operating systems.
Higher-level programmers are willing to forego it to get memory safety. That's a hard term to define, but according to Hicks 2014 it intuitively means that

pointers are only created in a safe way that defines their legal memory region,

pointers can only be dereferenced if they point to their allotted memory region,

that region is (still) defined.

 *)

 (* The semicolon operator is used to sequence effects, such as mutating refs.  *)

 (* implementing a counter *)

let counter = ref 0;;

let next_val = fun () ->
  counter := (!counter) + 1;
  !counter;;

next_val();; (* 1 *)
next_val();; (* 2 *)
next_val;; (* 3 *)

(* can improve by moving counter ref instantiation to inside function so it's not available outside
  can use std lib "incr" to increment a ref
 *)

let next_val =
  let counter = ref 0
  in fun () ->
    incr counter;
    !counter;;