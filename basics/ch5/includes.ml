module type Set = sig
  type 'a t
  val empty : 'a t
  val mem   : 'a -> 'a t -> bool
  val add   : 'a -> 'a t -> 'a t
  val elts  : 'a t -> 'a list
end

module ListSetDups : Set = struct
  type 'a t   = 'a list
  let empty   = []
  let mem     = List.mem
  let add x s = x::s
  let elts s  = List.sort_uniq Pervasives.compare s
end

(* Suppose we wanted to add a function of_list : 'a list -> 'a t to the ListSetDups module that could construct a set out of a list.
 If we had access to the source code of both ListSetDups and Set, and if we were permitted to modify it, this wouldn't be hard.
 But what if they were third-party libraries for which we didn't have source code?

OCaml provides a language features called includes that also enables code reuse.
 This feature is similar to the object-oriented example we just gave:
 it enables a structure to include all the values defined by another structure,
 or a signature to include all the names declared by another signature.
 *)

module ListSetDupsExtended = struct
  include ListSetDups (* like Ruby's include - now all properties are present inside (Typescript extend) *)
  let of_list lst = List.fold_right add lst empty
  (* can also override  *)
  let add x xs = x :: xs
end



(* Includes vs Open *)

module M = struct
  let x = 0
end

module N = struct
  include M
  let y = x + 1
end

module O = struct
  open M
  let y = x + 1
end

(*

module M : sig val x : int end
module N : sig val x : int val y : int end
module O : sig val y : int end


Include brings something into scope and makes it exported - think of Including a module in ruby
Open brings something into scope but only for local use - like importing a file in JS

*)

