(* Abstract Types *)

module type Stack = sig
  type 'a stack

  val empty : 'a stack
  val is_empty : 'a stack -> bool

  val push : 'a stack -> 'a -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
end

(* The type 'a stack above is abstract: the Stack module type says that there is a type name 'a stack in any module that implements the module type,
 but it does not say what that type is defined to be.
 Once we add the : Stack module type annotation to ListStack, its 'a stack type also becomes abstract.
 Outside of the module, no one is allowed to know that 'a stack and 'a list are synonyms.

 *)

(* A module that implements a module type must specify concrete types for the abstract types in the signature
and define all the names declared in the signature.
Only declarations in the signature are accessible outside of the module.
For example, functions defined in the module's structure but not in the module type's signature are not accessible.
We say that the structure is sealed by the signature: nothing except what is revealed in the signature may be accessed.
 *)

module MyStack : Stack = struct
  type 'a stack =
  | Empty
  | Entry of ('a * 'a stack)

  let empty = Empty
  let is_empty s = s = Empty
  let push s x = Entry (x, s)
  let peek = function
    | Empty -> failwith "Empty"
    | Entry (x, _) -> x
  let pop = function
    | Empty -> failwith "Empty"
    | Entry (_, s) ->  s
end

(* Because 'a stack is abstract in the Stack module type, no client of this data structure will be able to discern
whether stacks are being implemented with the built-in list type or the custom one we just used.
Clients may only access the stack in the ways that are defined by the Stack interface, which nowhere mentions list or Empty or Entry. *)

(* making stack sig more terse *)

module type Stack = sig
  type 'a t

  val empty: 'a t
  val is_empty: 'a t -> bool

  val push: 'a t -> 'a -> 'a t
  val peek: 'a t -> 'a
  val pop: 'a t -> 'a t
end


(* CUSTOM PRINTERS *)
(*
# #install_printer ListStack.format;;

# open ListStack;;

# empty |> push 1 |> push 2;;
- : int stack = [2; 1; ]

*)

(* Here's how *)

module type Stack = sig
  type 'a stack

  val empty: 'a stack
  val is_empty: 'a stack -> bool

  val push: 'a stack -> 'a -> 'a stack
  val peek: 'a stack -> 'a
  val pop: 'a stack -> 'a stack

  val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a stack -> unit (* new *)
end

module ListStack : Stack = struct
  type 'a stack = 'a list
  (* ... all the usual operations ... *)

  let empty = []
  let is_empty s = (s = [])

  let push s x = x::s

  let peek = function
    | [] -> failwith "Empty"
    | x::_ -> x


  let pop = function
    | [] -> failwith "Empty"
    | _::xs -> xs

  let format fmt_elt fmt s =
    Format.fprintf fmt "[";
    List.iter (fun elt -> Format.fprintf fmt "%a; " fmt_elt elt) s;
    Format.fprintf fmt "]"
end