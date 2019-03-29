let lst = [("charmander", 1); ("squirtle", 2); ("bulbasaur", 3)]

let insert k v d = (k,v)::d

let rec lookup k = function
  | [] -> None
  | (k',v)::t -> if k=k' then Some(v) else lookup k t;

lookup "h" lst
lookup "charmander" lst