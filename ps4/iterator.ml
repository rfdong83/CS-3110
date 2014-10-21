module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct

  type 'a t =  'a list ref
  exception NoResult

(*
Returns: true if there are more results to show, otherwise false
Requires: l to be type 'a t
*)
  let has_next (l: 'a t): bool =
    if !l = [] then
      false
    else 
      true

(*
Returns: the next element of l, if there are any, otherwise raises NoResult
Requires: l to be type 'a t
*)
  let next (l: 'a t): 'a =
    match !l with 
    | [] -> raise NoResult
    | h::t -> 
      l := t;
      h

(*
Returns: an iterator, yielding the elements of l, exactly once and in the
  same order as they appear in l.
Requires: l to be type 'a list
*)
  let create (l: 'a list): 'a t =
    ref l 
end


type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end


module InorderTreeIterator : INORDER_TREE_ITERATOR = struct

  type 'a t = 'a list ref
  exception NoResult

(*
Returns: an iterator that will return the elements of t, exactly once
  and in the order they would be produced by an in-order traversal of t
Requires: t to be type 'a tree
*)
  let rec create (t: 'a tree): 'a t = 
    match t with
    | Leaf -> ref []
    | Node (x,l,r) -> ref ((!(create l))@[x]@(!(create r)))

(*
Returns: true if there are more results to show, otherwise false
Requires: l to be type 'a t
*)
  let has_next (iter: 'a t): bool = 
    if !iter = [] then
      false
    else 
      true

(*
Returns: the next element of l, if there are any, otherwise raises NoResult
Requires: l to be type 'a t
*)
  let next (iter: 'a t): 'a = 
    match !iter with
    | [] -> raise NoResult
    | h::t -> 
      iter := t;
      h

end


module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end


module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct

  type 'a t = ('a I.t)
  exception NoResult

  let count = ref 0

(*
Returns: true if there are more results to show, otherwise false
Requires: l to be type 'a t
*)
  let has_next (iter: 'a t): bool = 
    I.has_next iter

(*
Returns: the next element of l, if there are any, otherwise raises NoResult
Requires: l to be type 'a t
*)
  let next (iter: 'a t): 'a = 
    count := !count - 1;
    if !count < 0 then 
      raise NoResult
    else 
      I.next iter

(*
Returns: an iterator that behaves the same as i for exactly n calls of to 
  next, but afterwards raises NoResult
Requires: n to be type int, i to be type 'a I.t
*)
  let create (n: int) (i: 'a I.t): 'a t = 
    count := n;
    i
end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I


  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (i: 'a t) : unit =
    let uniti (i: 'a t): unit = 
      next i;
      () in 
    let x = ref n in
    while !x <> 0 do
      uniti i;
      x := !x - 1
    done 

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let fold (f : ('a -> 'b -> 'a)) (acc : 'a) (i: 'b I.t) : 'a =
    let a = ref acc in 
    while (has_next i) do
      a := (f !a (next i))
    done;
    !a

end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way I would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end


module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct

  type 'a t = 'a I.t
  exception NoResult

  let switch = ref true

(*
Returns: true if there are more results to show, otherwise false
Requires: l to be type 'a t
*)  
  let has_next (i: 'a t): bool = 
    I.has_next i

(*
Returns: the next element of l, if there are any, otherwise raises NoResult
Requires: l to be type 'a t
*)
  let next (i: 'a t): 'a = 
    if !switch then 
      I.next i
    else 
      raise NoResult

(*
Returns: an iterator that behaves the way i would between nth and mth calls
of next, but returns NoResult afterwards. If n > m, then always 
returns NoResult
Requires: n and m to be type int, i to be type 'a I.t
*)
  let create (n: int) (m: int) (i: 'a I.t): 'a t = 
    let advance (n: int) (i: 'a t) : unit =
      let uniti (i: 'a t): unit = 
          next i;
          () in 
        let x = ref n in
      while !x <> 0 do
        uniti i;
        x := !x - 1
      done in
    let count = ref n in
    advance n i; 
    count := !count + 1;
    if (!count <= m) && (n <= m) then
      i
    else
      raise NoResult
end
