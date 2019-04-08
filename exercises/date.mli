type date
(* compared to *)
(* type date = { month:int; day:int; }*)
(* using the Abtract date type means that when Utop (or anything) returns a date, nobody can see what the inside looks like *)
(* all they see is that it's of type Date.date *)


(*

utop # let j1 = Date.make_date 1 1;;
val j1 : Date.date = <abstr>

vs

val j1 : Date.date = {Date.month = 1; day = 1}

 *)
val make_date : int -> int -> date
val get_month : date -> int
val get_day : date -> int
val to_string : date -> string
