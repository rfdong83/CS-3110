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
  val ( + ) : t -> t -> t
  (*Multiplication of natural numbers
      Properties:
        Commutative: a * b = b * a
        Associative: (a * b) * c === a * (b * c)
        Distributive Over Addition: a * (b + c) === (a * b) + (a * c)
    Requires: 2 arguments of type t
    Returns: the product of the two arguments
  *)
  val ( * ) : t -> t -> t 
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

    (*Takes t1 and t2 of type t and returns the sum of them. If the sum
      of t1 and t2 are beyond max_int then raise Unrepresentable error.

      Requires: t1 and t2 are of type t
      Returns: The natural number representation of the sum of t1 and t2 *)
    let ( + ) (t1: t) (t2: t) : t =
        if sum_overflows t1 t2 then raise Unrepresentable else t1 + t2


    (*Takes t1 and t2 of type t and returns the product of them. If the product
      of t1 and t2 are beyond max_int then raise Unrepresentable error.

      Requires: t1 and t2 are of type t
      Returns: The natural number representation of the product of t1 and t2 *)
    let ( * ) (t1: t) (t2: t) : t = 
        if (((max t1 t2) > t1*t2) && (t1*t2 <> zero))
            || (t1*t2 = zero && t1 <> zero && t2 <> zero)
            || ((t1*t2) < 0)
        then raise Unrepresentable else t1 * t2


    (*Takes t1 and t2 of type t and returns whether or not t1 is equal to t2.
      In this case, equality is defined as it being the same number.

      Requires: t1 and t2 are of type t
      Returns: bool representing whether t1 equals t2 or not *)
    let ( === ) (t1: t) (t2: t) : bool =
        t1 = t2


    (*Takes t1 and t2 of type t and returns whether or not t1 is less than t2.
      In this case, less than is defined as t1 have a value less than t2.

      Requires: t1 and t2 are of type t
      Returns: bool representing whether t1 is less than t2 or not *)
    let ( < ) (t1: t) (t2: t) : bool =
        t1 < t2  


    (*Takes a t num and then returns the interger equivalent of it.
      In this case, it would be the number itself, just as an int.
      Should the natural number be above max_int, raise Unrepresentable.

      Requires: num is of type t
      Returns: the integer representation of num *)
    let int_of_nat (num: t) : int =
        if sum_overflows zero num then raise Unrepresentable else num


    (*Takes a t int and then returns the natural number equivalent of it.
      In this case, it would be the number itself, just as a t.
      If the integer is negative, raise Unrepresentable.

      Requires: num is of type int
      Returns: the natural representation of num *)
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

    (*HELPER: Takes a integer count and int list lst and appends 1
              to lst a "count" number of times.
      Requires: count is an int, lst is an int list
      Returns: A list with 1 prepended to it "count" times. *)
    let rec multiplier (count: int) (lst: int list) : int list =
        match count with
        | 0 -> lst
        | _ -> multiplier (count-1) (1::lst) 

    (*HELPER: Our representation of length that checks for overflow
              at each iteration of addition. Should the length of the list
              be above max_int, raise Unrepresentable. This helper will
              account for all accounts of unrepresentabilities in this 
              module.

    Requires: lst is a list
    Returns: The length of a list (so long as it's less than max_int *)
    let length (lst: 'a list) : int =
        match lst with
        | [] -> 0
        | _ -> List.fold_left (fun a x -> 
                  if sum_overflows a 1 then
                      raise Unrepresentable
                  else
                      a + 1)
               0 lst


    (*Takes t1 and t2 of type t and returns the sum of them. The sum in this
      natural representation basically appends concats t1 and t2 together.

      Requires: t1 and t2 are of type t
      Returns: The natural number representation of the sum of t1 and t2 *)
    let ( + ) (t1: t) (t2: t) : t =
        List.fold_left (fun a x -> x::a) t2 t1


    (*Takes t1 and t2 of type t and returns the sum of them. The sum in this
      natural representation basically appends adds a list with length of t1
      to an empty list (length of t2) times.

      Requires: t1 and t2 are of type t
      Returns: The natural number representation of the product of t1 and t2 *)
    let ( * ) (t1: t) (t2: t) : t = 
        List.fold_left (fun a x -> multiplier (length t1) a) [] t2


    (*Takes t1 and t2 of type t and returns whether or not t1 is equal to t2.
      In this case, equality is defined as t1 and t2 having the same length.

      Requires: t1 and t2 are of type t
      Returns: bool representing whether t1 equals t2 or not *)
    let ( === ) (t1: t) (t2: t) : bool =
        length t1 = length t2


    (*Takes t1 and t2 of type t and returns whether or not t1 is equal to t2.
      In this case, less than is defined as the length of t1 being less than
      t2 or not.

      Requires: t1 and t2 are of type t
      Returns: bool representing whether t1 equals t2 or not *)
    let ( < ) (t1: t) (t2: t) : bool =
        length t1 < length t2  


    (*Takes a t num and then returns the interger equivalent of it.
      In this case, it would be the length of the list.

      Requires: num is of type t
      Returns: the integer representation of num *)
    let int_of_nat (num: t) : int =
        length num 


    (*Takes a t num and then returns the interger equivalent of it.
      In this case, it would be the number itself, just as an int.
      Should the natural number be above max_int, raise Unrepresentable.

      Requires: num is of type t
      Returns: the integer representation of num *)
    let nat_of_int (num: int) : t = 
        multiplier num []

end

module NatConvertFn (N: NATN) : NATN = struct 
    type t = N.t

    exception Unrepresentable

    let zero = N.zero
    let one = N.one

    (*Returns the sum of t1 and t2, with the idea of sum depending
      on the module N.

      Requires: t1 and t2 are of type t
      Returns: the sum of t1 and t2 based on N's rules. *)
    let ( + ) (t1: t) (t2: t) : t =
        N.( + ) t1 t2


    (*Returns the product of t1 and t2, with the idea of product depending
    on the module N.

    Requires: t1 and t2 are of type t
    Returns: the product of t1 and t2 based on N's rules. *)
    let ( * ) (t1: t) (t2: t) : t =
        N.( * ) t1 t2


    (*Returns whether t1 and t2 are equal, with the idea
      of equality depending on the module N.

      Requires: t1 and t2 are of type t
      Returns: bool representing whether t1 and t2 are equal. *)
    let ( === ) (t1: t) (t2: t) : bool =
        N.( === ) t1 t2


    (*Returns whether t1 is less than t2, with the notion of less than
      depending on the module N.

    Requires: t1 and t2 are of type t
    Returns: bool representing whether t1 is less than t2. *)
    let ( < ) (t1: t) (t2: t) : bool =
        N.( < ) t1 t2 


    (*Returns the integer representation of n.

      Requires: n is of type t
      Returns: The integer representation of n based on N's rules. *)
    let int_of_nat (n: t): int = 
        N.int_of_nat n


    (*Returns the natural number representation of n.

      Requires: n is an integer
      Returns: The natrual number representation of n based on N's rules. *)
    let nat_of_int (n: int): t =
        N.nat_of_int n

end

module AlienNatFn (M : AlienMapping): NATN = struct
    type t = M.aliensym list
    
    exception Unrepresentable

    let zero = [M.zero]
    let one = [M.one]

    (*Adds t1 and t2 together, which in this case means returning a list
      that contains all of the aliensyms in t1 and t2.

      Requires: t1 and t2 are of type t
      Returns: An aliensym list of all the elements in t1 and t2. *)
    let ( + ) (t1: t) (t2: t) : t =
        List.fold_left (fun a x -> x::a) t2 t1


    (*Multiplies t1 and t2 together, which in this case means returning a list
      that contains t1 repeated a number of times, with the number being the 
      integer sum of all the elements in t2.

      Requires: t1 and t2 are of type t
      Returns: An aliensym list dictating the product of t1 and t2. *)
    let ( * ) (t1: t) (t2: t) : t =
        (*HELPER: Multiplies lst a num amount of times *)
        let rec multiplies (num: int) (lst: t) : t list =
            if num = 0 then [] else lst::(multiplies (num-1) lst) in
        List.flatten(List.fold_left 
            (fun a x -> List.flatten((multiplies (M.int_of_aliensym x) t1))::a)
            [] 
            t2
        )


    (*Returns whether t1 and t2 are equal, which means that the integer 
      representations of both t1 and t2 are equal. Returns Unrepresentable
      if one of the integer repesentations is past max_int.

      Requires: t1 and t2 are of type t
      Returns: bool showing whether t1 and t2 are equal *)
    let ( === ) (t1: t) (t2: t) : bool =
        (List.fold_left (fun a x -> if (sum_overflows (M.int_of_aliensym x) a) then 
                                    raise Unrepresentable 
                                    else (Pervasives.(+) (M.int_of_aliensym x)  a)) 0 t1) =
        (List.fold_left (fun a x -> if (sum_overflows (M.int_of_aliensym x) a) then 
                                    raise Unrepresentable 
                                    else (Pervasives.(+) (M.int_of_aliensym x)  a)) 0 t2)


    (*Returns whether t1 is less than t2, which means that the integer 
      representation of t1 is less than t2. Returns Unrepresentable
      if one of the integer repesentations is past max_int.

      Requires: t1 and t2 are of type t
      Returns: bool showing whether t1 is less than t2 *)
    let ( < ) (t1: t) (t2: t) : bool =
        (List.fold_left (fun a x -> if (sum_overflows (M.int_of_aliensym x) a) then 
                                    raise Unrepresentable 
                                    else (Pervasives.(+) (M.int_of_aliensym x)  a)) 0 t1) <
        (List.fold_left (fun a x -> if (sum_overflows (M.int_of_aliensym x) a) then 
                                    raise Unrepresentable 
                                    else (Pervasives.(+) (M.int_of_aliensym x)  a)) 0 t2)


    (*Returns the integer representation of n, which is the sum of all the 
      integer equivalents of aliensyms in n.

      Requires: n is of type t
      Returns: An integer representing n *)
    let int_of_nat (n: t) : int =
        List.fold_left (fun a x -> if (sum_overflows (M.int_of_aliensym x)  a) then 
                                       raise Unrepresentable 
                                   else 
                                       (Pervasives.(+) (M.int_of_aliensym x)  a)) 0 n


    (*Returns the natural representation of i, which is a list of aliensyms
      whose integer representations sum up to i.

      Requires: i is of type int
      Returns: A aliensym list that is the natural representation of i. *)
    let nat_of_int (i: int) : t =
        let rec adder (i: int) (accum: int) (lst: t list) : t list =
            if accum = i then 
                lst 
              else 
                adder i (Pervasives.(+) accum 1) (one::lst) in
        List.flatten (adder i 0 [])
 
end