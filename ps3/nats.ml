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

    let ( +- ) (t1: t) (t2: t) : t =
        if sum_overflows t1 t2 then raise Unrepresentable else t1 + t2


    let ( *- ) (t1: t) (t2: t) : t = 
        if ((max t1 t2) > t1*t2) 
            || (t1*t2 = 0 && t1 <> 0 && t2 <> 0)
            || ((t1*t2) < 0)
        then raise Unrepresentable else t1 * t2


    let ( === ) (t1: t) (t2: t) : bool =
        t1 = t2

 
    let ( < ) (t1: t) (t2: t) : bool =
        t1 < t2  


    let int_of_nat (num: t) : int =
        if sum_overflows zero num then raise Unrepresentable else num


    let nat_of_int (num: int) : t = 
        if num < 0 then raise Unrepresentable else num

end

module ListNat: NATN = struct
    (* The list [a1; ...; an] represents the
     * natural number n. That is , the list lst represents
     * length ( lst ). The empty list represents 0. The values of
     * the list elements are irrelevant . *)
    type t = int list

    exception Unrepresentable

    let zero = []
    let one = [1]

    let rec multiplier (count: int) (lst: int list) : int list =
            match count with
            | 0 -> lst
            | _ -> multiplier (count-1) (1::lst) 

    let ( +- ) (t1: t) (t2: t) : t =
        List.fold_left (fun a x -> x::a) t2 t1


    let ( *- ) (t1: t) (t2: t) : t = 
        List.fold_left (fun a x -> multiplier (List.length t1) a) [] t2


    let ( === ) (t1: t) (t2: t) : bool =
        List.length t1 = List.length t2

 
    let ( < ) (t1: t) (t2: t) : bool =
        List.length t1 < List.length t2  


    let int_of_nat (num: t) : int =
        List.length num


    let nat_of_int (num: int) : t = 
        multiplier num []

end

module NatConvertFn (N: NATN) : NATN = struct 
    type t = N.t

    exception Unrepresentable

    let zero = N.zero
    let one = N.one

    let ( +- ) (t1: t) (t2: t) : t =
        N.( +- ) t1 t2

    let ( *- ) (t1: t) (t2: t) : t =
        N.( *- ) t1 t2

    let ( === ) (t1: t) (t2: t) : bool =
        N.( === ) t1 t2

    let ( < ) (t1: t) (t2: t) : bool =
        N.( < ) t1 t2 

    let int_of_nat (n: t): int = 
        N.int_of_nat n

    let nat_of_int (n: int): t =
        N.nat_of_int n

end

module AlienNatFn (M : AlienMapping): NATN = struct
    type t = M.aliensym list
    
    exception Unrepresentable

    let zero = [M.zero]
    let one = [M.one]

    let ( +- ) (t1: t) (t2: t) : t =
        List.fold_left (fun a x -> x::a) t2 t1

    let ( *- ) (t1: t) (t2: t) : t =
        List.fold_left (fun a x -> x::a) t2 t1

    let ( === ) (t1: t) (t2: t) : bool =
        (List.fold_left (fun a x -> (M.int_of_aliensym x) + a) 0 t1) =
        (List.fold_left (fun a x -> (M.int_of_aliensym x) + a) 0 t2)

    let ( < ) (t1: t) (t2: t) : bool =
        (List.fold_left (fun a x -> (M.int_of_aliensym x) + a) 0 t1) <
        (List.fold_left (fun a x -> (M.int_of_aliensym x) + a) 0 t2)

    let int_of_nat (n: t) : int =
        List.fold_left (fun a x -> (M.int_of_aliensym x) + a) 0 n

    let nat_of_int (i: int) : t =
        let rec adder (i: int) (accum: int) (lst: t list) : t list =
            if accum = i then lst else adder i (accum + 1) (one::lst) in
        List.flatten (adder i 0 [])
 
end