(* Chapter 5 *)

(* The next couple exercises play with compilation units.

Exercise: implementation without interface [✭]
Create a file named date.ml. In it put exactly the following code: *)

type date = { month:int; day:int }
let make_date month day = {month; day}
let get_month d = d.month
let get_day d = d.day
let to_string d = (string_of_int d.month) ^ "/" ^ (string_of_int d.day)
