type 'a t = 'a list

let empty = []

let is_empty s = s = []

let push x xs = x::xs

let peek = function
  | [] -> failwith "Empty"
  | x::_ -> x

let pop = function
  | [] -> failwith "Empty"
  | _::xs -> xs