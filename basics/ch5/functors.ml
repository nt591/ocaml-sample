(* A Functor is a "function" from structures to structures

In mathematics, a functor is a map between categories. - https://en.wikipedia.org/wiki/Functor

Any object or data structure that implements the map function is considered a functor.  -https://frontendmasters.com/courses/functional-javascript/maybe-functor/
*)

module type X = sig
  val x : int
end

(* Now this is a functor - IncX is a functor *)

module IncX (M: X) = struct
  let x = M.x + 1
end

(* IncX is a parameterized structure *)

(* examples of usage *)

module A = struct
  let x = 0
end
(* A.x = 0 *)

module B = IncX(A)
(* B.x = 1 *)

module C = IncX(B)
(* C.x = 2 *)

(* Why "functor"? In category theory, a category contains morphisms,
which are a generalization of functions as we known them, and a functor is map between categories.
Likewise, OCaml structures contain functions, and OCaml functors map from structures to structures.
 *)

(* Functor Syntax *)

(* module F (M : S) = struct end *)
(* type annotations are required, no inference is available on functor signatures *)
(* anonymous functors exists too *)
(*

module F = functor (M : S) -> struct end

functors can take multiple parameterized structures

module F = functor (M1: S1) (M2: S2) ... (Mn : Sn) = struct end

If you want to specify the output type of a functor, the syntax is again similar to functions:

module F (M : Si) : So = struct
  ...
end


typing a functor is pretty straightforward too

functor (M : Si) -> So

module F : functor (M : Si) -> So =
  functor (M : Si) -> struct ... end
The first occurrence of functor in that code means that what follows is a functor type,
and the second occurrence means that what follows is an anonymous functor value.
 *)




(* Using Functors *)

(* setup  *)

module type StackSig = sig
  type 'a t
  val empty : 'a t
  val push  : 'a -> 'a t -> 'a t
  val peek  : 'a t -> 'a
end

module ListStack = struct
  type 'a t = 'a list
  let empty = []
  let push x s = x::s
  let peek = function [] -> failwith "empty" | x::_ -> x
end

(* called MyStack because the standard library already has a Stack *)
module MyStack = struct
  type 'a t = Empty | Entry of 'a * 'a t
  let empty = Empty
  let push x s = Entry (x, s)
  let peek = function Empty -> failwith "empty" | Entry(x,_) -> x
end

(*

Suppose we wanted to write code that would test a ListStack:

assert (ListStack.(empty |> push 1 |> peek) = 1)
Unfortunately, to test a MyStack, we'd have to duplicate that code:

assert (MyStack.(empty |> push 1 |> peek) = 1)

*)

module StackTester (S:StackSig) = struct
  assert (S.(empty |> push 1 |> peek) = 1)
end

module ListStackTester = StackTester(ListStack)
module MyStackTester = StackTester(MyStack)

(* COOL *)

(* Using functors to reduce duplication *)

(* setup *)

module type Set = sig
  type 'a t

  (* [empty] is the empty set *)
  val empty : 'a t

  (* [mem x s] holds iff [x] is an element of [s] *)
  val mem   : 'a -> 'a t -> bool

  (* [add x s] is the set [s] unioned with the set containing exactly [x] *)
  val add   : 'a -> 'a t -> 'a t

  (* [elts s] is a list containing the elements of [s].  No guarantee
   * is made about the ordering of that list. *)
  val elts  : 'a t -> 'a list
end

module ListSetNoDups : Set = struct
  type 'a t   = 'a list
  let empty   = []
  let mem     = List.mem
  let add x s = if mem x s then s else x::s
  let elts s  = s
end

module ListSetDups : Set = struct
  type 'a t   = 'a list
  let empty   = []
  let mem     = List.mem
  let add x s = x::s
  let elts s  = List.sort_uniq Pervasives.compare s
end

module AddAll(S:Set) = struct
  let add_all lst set =
    let add' s x = S.add x s in
    List.fold_left add' set lst
end

(*

AddAll is a functor that uses anything that implements set
it defines add' as S.add (so either Set.add or anything that might override it)

 *)

 (* # module AddAllListSetDups = AddAll(ListSetDups);; *)
module type AddAllListSetDups =
  sig
    val add_all : 'a list -> 'a ListSetDups.t -> 'a ListSetDups.t
  end

(* # module AddAllListSetNoDups = AddAll(ListSetNoDups);; *)
module type AddAllListSetNoDups =
  sig
    val add_all : 'a list -> 'a ListSetNoDups.t -> 'a ListSetNoDups.t
  end

(*
By using the AddAll functor, we can define an add_all function that works on any data structure that implements Set

But that's the only function those two structures contain.
Really what we want is a full set implementation that also contains the add_all function.
We can get that by combining includes with functors:

*)

module ExtendSet (S:Set) = struct
  include S

  let add_all lst set =
    let add' s x = S.add x s in
    List.fold_left add' set lst
end


(*
# module ListSetNoDupsExtended = ExtendSet(ListSetNoDups);;
module ListSetNoDupsExtended :
  sig
    type 'a t = 'a ListSetNoDups.t
    val empty : 'a t
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val elts : 'a t -> 'a list
    val add_all : 'a list -> 'a t -> 'a t
  end *)