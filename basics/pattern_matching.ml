(* Pokemon types *)
type ptype =
  TNormal | TFire | TWater

(* A record to represent Pokemon *)
type mon = {name: string; hp : int; ptype: ptype}

let get_hp_1 m =
  match m with
  | {hp=h} -> h

(* best *)

let get_hp m = m.hp


(* getting element of tuple *)

let get_third_1 t =
  match t with
  | (_, _, s) -> s

(* best *)

let get_third (_, _, s) = s