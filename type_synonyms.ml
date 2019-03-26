type point = float * float
type label = string
type ptype = Fire | Water | Grass;;
type pokemon = {
  name: string;
  hp: int;
  ptype: ptype;
};;
type pokedex = pokemon list;;

let pokes = [{name = "c"; hp=42; ptype=Fire}; {name = "s"; hp=36; ptype=Water}; {name = "b"; hp=33; ptype=Grass}];;

(* This doesn't work in Utop with Base because of the signature difference in List.map? *)
let getHps lst =  List.map (fun m -> m.hp) lst;;