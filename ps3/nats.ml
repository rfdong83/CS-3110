(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t

  (*The additive identity: a + zero = a*)
  val zero : t
  (*The multiplicative indentity: one * a = a*)
  val one : t
  (*Addition function of natural numbers
      Properties:
        Commutative: a + b = b + a
        Associative: (a + b) + c === a + (b + c)
    Requires: 2 arguments of type t
    Returns: the sum of the two arguments
  *)
  val ( +- ) : t -> t -> t
  (*Multiplication of natural numbers
      Properties:
        Commutative: a * b = b * a
        Associative: (a * b) * c === a * (b * c)
        Distributive Over Addition: a * (b + c) === (a * b) + (a * c)
    Requires: 2 arguments of type t
    Returns: the product of the two arguments
  *)
  val ( *- ) : t -> t -> t 
  (*Less-than ordering of natural numbers
    Requires: 2 argumnts of type t
    Returns: true if first argument is less than the second, false otherwise
  *)
  val ( < ) : t -> t -> bool
  (*Equality of natural numbers
    Requires: 2 arguments of type t
    Returns: true if first argument is equivalent to the second, false otherwise
  *)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  (*Transforms natural numbers into ints
    Requires: argument is of type t
    Returns: int equivalent of t
  *)
  val int_of_nat: t -> int
  (*Transforms ints into natural numbers
    Requires: argument is of type int
    Returns: natural number equivalent of int
  *)
  val nat_of_int: int -> t
end

module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)

module IntNat: NATN = struct
    type t = int

    exception Unrepresentable

    let zero = 0

    let one = 1

    let ( +- ) (t1: t) (t2: t)=
        t1+t2

    let ( *- ) (t1: t) (t2: t)= 
        t1 * t2

    let ( === ) (t1: t) (t2: t) : bool =
        t1 = t2

    let ( < ) (t1: t) (t2: t) : bool =
        t1 < t2  

    let int_of_nat (num: t) : int =
        num

    let nat_of_int (num: int) : t= 
        if num < 0 then raise Unrepresentable else num


end

