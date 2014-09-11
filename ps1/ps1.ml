(** 
 * Returns true if an integer list, lst, is monotonically increasing otherwise
 * returns false. 
 *
 * Requires: lst is an int list
 * Returns: true if lst is monotonically increasing, false otherwise
 *)
let rec is_mon_inc (lst: int list) : bool = 
    match lst with
        [] -> true
        |[x] -> true 
        |f::s::t -> 
            if f<=s then is_mon_inc(s::t) else false

(** 
 * Returns true if an integer list, lst, is unimodal, otherwise returns false
 * 
 * Requires: lst is an int list
 * Returns: true if lst is unimodal, false otherwise
 *)
let rec is_unimodal (lst: int list) : bool =
    (* HELPER: does opposite of is_mon_inc *)
    let rec is_mon_dec (lst: int list) : bool = 
        match lst with
            [] -> true
            |[x] -> true 
            |f::s::t -> if f>=s then is_mon_dec(s::t) else false in
    match lst with
        [] -> true
        |[x] -> true
        |f::s::t ->
            if f<=s then is_unimodal(s::t) else is_mon_dec(s::t) 

(**
 * Returns the powerset of set S, represented by a the list s.
 * 
 * Requires: s is an int list
 * Returns: a list which represents the powerset of s
 *)
let rec powerset (s: int list) : int list list = 
    (* HELPER: returns a list with x prepended to every element *)
    let rec addon ((x: int), (lst: int list list)) : int list list =
        match lst with
            [] -> []
            |h::t -> (x::h)::addon(x,t) in
    match s with
        [] -> [[]]
        |x::t -> addon(x,powerset(t))@powerset(t)

(** 
 * Takes an integer i and returns an integer whose digits are
 * the reverse of i, while maintinaing the sign. Function not
 * responsible for integers i whose reverse is beyond max_int.
 *
 * Requires: i is an integer
 * Returns: An integer that has i's digits reversed so long as
 *          it's less than max_int
 *)
let rec rev_int (i: int) : int =
    (* HELPER: integer exponentiation because it wasn't built in?? *)
    let rec exp ((x: int), (y: int)) : int =
        match y with
        0 -> 1
        |_ -> x * exp(x,y-1) in
    match i with
        0 -> 0
        |_ -> (i mod 10) 
               * exp(10, String.length(string_of_int(abs(i))) - 1) 
               + rev_int((i - (i mod 10))/10)

(**
 * Takes a list lst and splits it into a list option of lists, each of size k.
 * The last element of the return list can have a size less than k if there
 * are not enough elements.

 * Requires: lst is an `a list, k is an integer
 * Returns: an `a list list option with order preserved
 *)
let rec unflatten ((k: int), (lst: 'a list)) : 'a list list option=
    (* HELPER: returns a tuple of the list split before the kth index 
               Ex. splitter([1;2;3;4;5],2,[]) = ([1;2], [3;4;5]) *)
    let rec splitter ((hlst: 'a list),(hk: int),(accum: 'a list)) 
                     : 'a list * 'b list =
        if hk = 0 then (List.rev(accum), hlst)
        else match hlst with
              |[] -> splitter([], 0, accum)
              |h::t -> splitter(t, hk-1, h::accum) in
    (* HELPER: takes the above helper function and adds each extracted
               list of size k to the return list*)
    let rec adder ((k: int), (lst: 'a list)) : 'a list list =
        match lst with
            |[] -> []
            |_ -> fst(splitter(lst, k, [])) :: 
                adder(k, snd(splitter(lst,k,[]))) in
    if k<= 0 then None 
    else match adder(k, lst) with
        [] -> Some [[]]
        |_ -> Some (adder(k, lst))

(** 
 * Returns the integer equivalent of a roman numeral. 
 * The roman numeral is represented by a list of numerals, roman.
 *
 * Requires: roman must be a list of numerals that depict only 
 *           valid roman numerals, as specified in hw1.pdf.
 * Returns: An intenger that is the same as the roman numeral
 *)
type numeral = I | V | X | L | C | D | M
type roman = numeral list

let rec int_of_roman ( r : roman ) : int =
    (*HELPER: matches each roman numeral to its respective integer *)
    let int_of_numeral = function
        | I -> 1
        | V -> 5
        | X -> 10
        | L -> 50
        | C -> 100
        | D -> 500
        | M -> 1000 in
            match r with
            [] -> 0
            |[x] -> int_of_numeral(x)
            |f::s::t -> if int_of_numeral(f) >= int_of_numeral(s) 
                then int_of_numeral(f) + int_of_roman(s::t)
                else - (int_of_numeral(f)) + int_of_roman(s::t)