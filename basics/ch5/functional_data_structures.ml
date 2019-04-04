(* a Functional Data Structure is one that does not make use of any imperative features *)

(* FDTs are persistent (immutable) in that any operation returns a new version *)
(* The opposite of this would be an "ephemeral" data structure *)

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

let s = ListStack.push 1 (ListStack.push 2 ListStack.empty)
(* val s : int list = [1; 2] *)

let s' = ListStack.pop s
(* val s' : int list = [2] *)

(* s *)
(* int list = [1; 2] *)