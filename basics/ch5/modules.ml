module ModuleName = struct
  (* stuff goes here *)
end

module ListStack = struct
  let empty = []
  let is_empty s = (s = [])

  let push x s = x::s

  let peek = function
    | [] -> failwith "Empty"
    | x::_ -> x


  let pop = function
    | [] -> failwith "Empty"
    | _::xs -> xs
end

(* Modules partition the namespace, so that any symbol x that is bound in the implementation of a module named Module must be referenced by the qualifed name Module.x
outside the implementation of the module (unless the namespace has been exposed using open). *)

(* You can use the open keyword to open a module and make everything in scope *)

(*

open List

map (fun x -> x) [1;2;3]
filter (fun x -> true) [ true; true; false]

 *)

(* you can also use the open keyword inside an expression to scope your module to just that express *)

(*

let f x =
  let open List in
  let y = filter ((>) 0) in x....

 *)

(* Syntactic sugar - use parents like M.(n) *)

(*
let s = "Hello "
let f = String.(s |> trim |> lowercase_ascii)
*)



(* MODULE TYPES *)

(* Module types are for describing groups of related modules (similar to regular types) *)

module type ModuleTypeName = sig
end

module type Stack = sig
  type 'a stack

  val empty : 'a stack
  val is_empty : 'a stack -> bool

  val push : 'a stack -> 'a -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
end

(* examples of matching signatures to modules *)

module type Example = sig
  val f: int -> int
end

module Test1 : Example = struct
  let f x = x + 1
end

module Test2 : Example = struct
  let f x = x - 1
end