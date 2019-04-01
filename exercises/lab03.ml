(* Write a function that returns the product of all the elements in a list.
  The product of all the elements of an empty list is 1.
  Hint: recall the sum function we defined in lecture. *)

let rec product lst =
  match lst with
  | [] -> 1
  | h::t -> h * product t

(* Write a function that concatenates all the strings in a list.
  The concatenation of all the strings in an empty list is the empty string "".
  Hint: this function is really not much different than sum or product *)

let rec concat lst =
  match lst with
  | [] -> ""
  | h::t -> h ^ (concat t)

(* Using pattern matching, write three functions, one for each of the following properties.
Your functions should return true if the input list has the property and false otherwise.

the list's first element is "bigred"
the list has exactly two or four elements; do not use the length function
the first two elements of the list are equal *)

let bigred = function
  | [] -> false
  | h::t -> if h="bigred" then true else false
  | _ -> false


(* must be a better way *)
let two_or_four lst =
  match (let rec counter acc = function
    | [] -> acc
    | h::t -> counter (acc + 1) t
  in counter 0 lst) with
  | 2 -> true
  | 4 -> true
  | _ -> false

let first_two_eq = function
  | [] -> false
  | h::t -> match t with
    | [] -> false
    | a::b -> if h=a then true else false

(* Write a function that takes an int list and returns the fifth element of that list, if such an element exists.
If the list has fewer than five elements, return 0. Hint: List.length and List.nth. *)

(* OCaml Base returns an option for List.nth  *)
let fifth_element lst = if List.length lst < 5 then 0 else List.nth lst 6

(* Write a function that takes an int list and returns the list sorted in descending order.
Hint: List.sort with Pervasives.compare as its first argument, and List.rev. *)

let reverse_sort lst = List.rev (List.sort Pervasives.compare lst)

(* Write a function that returns the last element of a list. Your function may assume that the list is non-empty.
Hint: Use two library functions, and do not write any pattern matching code of your own. *)

let last_element lst = List.nth lst ((List.length lst) - 1)

(* Write a function any_zeroes : int list -> bool that returns true if and only if the input list contains at least one 0.
Hint: use one library function, and do not write any pattern matching code of your own. *)

let any_zeroes lst = List.exists (fun x -> x = 0) lst

(* Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n elements of lst.
If lst has fewer than n elements, return all of them. *)

let take n lst = if List.length lst < n then lst else
  let rec take_acc count l =
    match (count, l) with
    | (0, _) -> l
    | (_, []) -> l
    | (_, h::t) -> h :: take_acc (count-1) t
    in take_acc n lst

(* Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first n elements of lst.
If lst has fewer than n elements, return the empty list. *)

let drop n lst = if List.length lst < n then [] else
  let rec drop_acc count l =
    match (count, l) with
    | (0, _) -> l
    | (_, []) -> l
    | (_, _::t) -> drop_acc (count-1) t
    in drop_acc n lst



type poketype = Normal | Fire | Water

type pokemon = {
  name: string;
  hp: int;
  ptype: poketype
}

(* Write a function safe_hd : 'a list -> 'a option that returns
Some x if the head of the input list is x, and None if the input list is empty. *)

let safe_hd lst =
  match lst with
  | [] -> None
  | h::t -> Some h

(* Also write a function safe_tl : 'a list -> 'a list option that returns the tail of the list, or None if the list is empty. *)

let safe_tl lst =
  match lst with
  | [] -> None
  | h::t -> Some t

(* Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP. *)


(* todo: figure out how to keep a reference to last one seen *)
(* let max_hp (lst: pokemon list) =
  match lst with
  | [] -> None
  | lst -> List.sort *)