(* Chapter 5 *)

(* see here for rules

http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/modules/exercises.html

 *)
module type Ring = sig
  type t
  val zero  : t
  val one   : t
  val (+)   : t -> t -> t
  val (~-)  : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val of_int : int -> t
end

module type Field = sig
  include Ring
  val (/) : t -> t -> t
end

module IntModule = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) = (+)
  let (~-) = (~-)
  let ( * ) = ( * )
  let to_string = string_of_int
  let of_int n = n
end


module FloatModule = struct
  type t = float
  let zero = 0.
  let one = 1.
  let (+) = (+.)
  let (~-) = (~-.)
  let ( * ) = ( *. )
  let to_string = string_of_float
  let of_int n = float_of_int n
end

module Rational (M: Field) = struct
  type t = (M.t * M.t)
  let zero = (M.zero, M.zero)
  let one = (M.one, M.one)
  let x a d = M.( * ) a d
  let (+) (a,b) (c,d) = M.( + ) (M.( * ) a d) (M.( * ) c b), M.( * ) b d
  let (~-) (a,b) = (M.(~-) a,b)
  let (/) (a,b) (c,d) = M.( * ) a d, M.( * ) b c
  let ( * ) (a,b) (c,d) = M.( * ) a c, M.( * ) b d
  let to_string (a, b) = M.to_string a ^ "/" ^ M.to_string b
  let of_int n = (M.of_int n, M.of_int 1)
end

module IntRing : Ring = struct
  include IntModule
end

module IntField : Field = struct
  include IntModule
  let (/) = (/)
end

module FloatRing : Ring = struct
  include FloatModule
end


module FloatField : Field = struct
  include FloatModule
  let (/) = (/.)
end

module IntRational : Field = Rational(IntField)
(* ORIGINAL *)
(* module IntRational : Field = struct
  type t = int*int
  let zero = (0,0)
  let one = (1,1)
  let (+) (a,b) (c,d) = (a*d + c*b, b*d)
  let (~-) (a,b) = (-a,b)
  let (/) (a,b) (c,d) = a*d, b*c
  let ( * ) (a,b) (c,d) = (a*c, b*d)
  let to_string (a,b) = string_of_int a ^ "/" ^ string_of_int b
  let of_int n = (n,1)
end *)

module FloatRational : Field = Rational(FloatField)
(* ORIGINAL
module FloatRational : Field = struct
  type t = float*float
  let zero = (0.,0.)
  let one = (1.,1.)
  let (+) (a,b) (c,d) = (a*.d +. c*.b, b*.d)
  let (~-) (a,b) = (-.a,b)
  let (/) (a,b) (c,d) = (a*.d, b*.c)
  let ( * ) (a,b) (c,d) = (a*.c, b*.d)
  let to_string (a,b) = string_of_float a ^ "/" ^ string_of_float b
  let of_int n = (float_of_int n, 1.)
end *)

