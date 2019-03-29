type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
  value: 'a;
  left: 'a tree;
  right: 'a tree
}

(* Essentially, any cycle of recursive types must include at least one record or variant type.
  Since the cycle between 'a tree and 'a node includes both kinds of types, it's legal. *)

let t = Node {
  value = 2;
  left = Node { value = 1; left = Leaf; right = Leaf;};
  right = Node { value = 3; left = Leaf; right = Leaf}
}

let rec mem x = function
  | Leaf -> false
  | Node {value; left; right} -> value = x || mem x left || mem x right

(* let rec preorder = function
  | Leaf -> []
  | Node {value; left; right} -> [value] @ preorder left @ preorder right *)

(* Text: Although the algorithm is beautifully clear from the code above, it takes quadratic time on unbalanced trees because of the @ operator. *)
(* @ sign is list concat *)

let preorder_lin t =
  let rec pre_acc acc = function
    | Leaf -> acc
    | Node {value; left; right} -> value :: (pre_acc (pre_acc acc right) left)
  in pre_acc [] t

(* start with an empty list, return the list if you find a Leaf, prepend the value the list and keep going  *)