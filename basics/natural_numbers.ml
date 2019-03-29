type nat = Zero | Succ of nat

let zero = Zero
let one = Succ zero
let two = Succ one
let three = Succ two
let four = Succ three

let iszero (n : nat) : bool =
  match n with
    | Zero -> true
    | Succ _ -> false

let pred (n : nat) : nat =
  match n with
    | Zero -> failwith "undefined predecessor for zero"
    | Succ m -> m

let rec add (n1: nat) (n2: nat) : nat =
  match n1 with
  | Zero -> n2
  | Succ n_minus_1 -> add n_minus_1 (Succ n2)

let rec int_of_nat (n : nat) : int =
  match n with
  | Zero -> 0
  | Succ m -> 1 + int_of_nat m

let rec nat_of_int (i: int) : nat =
  if i < 0 then failwith "Cannot convert negative numbers"
  else if i = 0 then Zero
  else Succ (nat_of_int (i - 1))

let rec
  even (n:nat) : bool =
    match n with
      | Zero   -> true
      | Succ m -> odd m
and
  odd (n:nat) : bool =
    match n with
      | Zero   -> false
      | Succ m -> even m
