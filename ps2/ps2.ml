type 'a exprTree =
| Val of 'a
| Unop of ('a -> 'a) * 'a exprTree
| Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree

type vector = int list
type matrix = vector list

type pat =
| WCPat (*" wildcard " , i.e. , underscore *)
| VarPat of string
| UnitPat
| ConstPat of int
| TuplePat of pat list
| StructorPat of string * pat option (*" constructor "*)

type value =
| ConstVal of int
| UnitVal
| TupleVal of value list
| StructorVal of string * value option

type bindings = (string * value) list option

exception MatrixFailure of string
exception NoAnswer 

(**
 * Takes an expression tree and returns the number of operations in it.
 * 
 * Requires: tree is an 'a exprTree 
 * Returns: an int that indicates how many functions the tree contains
 *)
let rec count_ops (tree: 'a exprTree): int =
	match tree with
    |Val _ -> 0
    |Unop (a,b) -> 1 + count_ops(b)
    |Binop (a,b,c) -> 1 + count_ops(b) + count_ops(c)


(**
 * Takes an integer x and returns an expression tree that represents the 
 * factorial of the integer 
 *
 * Requires: x is an integer that is at least 0
 * Returns: an expression tree that depicts the factorial of x
 *)
let rec make_fact_tree (x: int): int exprTree =
    if x<0 then raise (Failure "Negative number not allowed")
        else match x with
        |0 -> Val 1
        |_ -> Binop(( * ), Val x, make_fact_tree(x-1))


(**
 * Takes an expression tree and evaluates the expression described by 
 * the tree.
 *
 * Requires: tree is an 'a exprTree 
 * Returns: the value that the tree evaluates to
 *)
let rec eval (tree: 'a exprTree): 'a =
    match tree with
    |Val x -> x
    |Unop (a,b) -> a (eval b)
    |Binop (a,b,c) -> a (eval b) (eval c)

(**
 * Takes a float list lst and returns the product of all elements within
 * the list. If the list is empty, the product returned is 1.0.
 *
 * Requires: lst is a list of floats
 * Returns: the product of all floats within list lst
 *)
let product (lst: float list) : float = 
    List.fold_left ( *. ) 1. lst

(**
 * Takes a string list and returns the concatenation of all elements
 * using List.fold_left. An empty list will return "".
 *
 * Requires: lst is a list of strings
 * Returns: the concatenated string from all elements within lst
 *)
let concat_left (lst: string list) : string =
    List.fold_left (^) "" lst


(**
 * Takes a string list and returns the concatenation of all elements
 * using List.fold_right. An empty list will return "".
 *
 * Requires: lst is a list of strings
 * Returns: the concatenated string from all elements within lst
 *)
let concat_right (lst: string list) : string =
    List.fold_right (^) lst ""

(**
 * Maps a function f that takes the index of the element as the first argument
 * and the element itself as the second argument to the whole list lst.
 *
 * Requires: f is a function that takes an int and an element from the list lst 
 *           as its two arguments
 * Returns: the list after f is applied to all elements within lst
 *)
let mapi_lst (f : int -> 'a -> 'b) (lst: 'a list) : 'b list =
    (** HELPER: returns the length of a list by using fold_left *)
    let length (lst : 'b list) : int =
        List.fold_left(fun a _ -> a+1) 0 lst in
    
    List.rev(List.fold_left (fun (acc: 'b list) (a: 'a) -> 
              (f (length(acc)) a)::acc) [] lst)


(**
 * Takes a string list lst and produces a numbered outline by using mapi_lst
 * to prepend a number, period, and space to each element in lst.
 *
 * Requires: lst is a list of strings
 * Returns: lst with a numbering system added to each string
 *)
let outline (lst: string list) : string list =
    (** HELPER: Adds the number, period, and space to a string *)
    let outliner (x: int) (str: string) : string =
        string_of_int (x+1) ^ ". " ^ str in
    mapi_lst outliner lst 


(**
 * Returns a list that showcases the accumulator acc everytime function f
 * is applied to list lst from the left using fold_left
 *
 * Requires: f is a function that takes the accumulator acc and an element
 *           from lst and returns a new value of acc.
 * Returns: a list with every value the accumulator takes during the process
 *)
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
    List.rev(List.fold_left (fun a b-> 
        (f (List.hd(a)) b)::a) [acc] lst)


(**
 * Returns a list that showcases the accumulator acc everytime function f
 * is applied to list lst from the right using fold_right.
 *
 * Requires: f is a function that takes the accumulator acc and an element
 *           from lst and returns a new value of acc.
 * Returns: a list with every value the accumulator takes during the process
 *)
let scan_right (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b list =
    List.rev(List.fold_right (fun a b-> 
        (f a (List.hd(b)))::b) lst [acc])


(* requires : n >= 1
returns : the list [1;2;...; n] *)
let countup ( n : int ) : int list =
    (* tail - recursive helper function for countup :
    accumulate the answer in l,
    starting from n and working down *)
    let rec countup' i l =
        if i <=1 then l
        else countup' (i -1) ( i :: l )
            in countup'  n []


(**
 * Takes an integer n and returns the list of [1!; 2!; ... n!]
 * n will always be at least 1.
 *
 * Requires: n is an integer that is always at least 1
 * Returns: The list of factorials of every positive integer less
 *          than or equal to n
 *)
let fact_list (n: int): int list =
    scan_left ( * ) 1 (countup(n))

(**
 * Takes the matrix m and prints out all elements. If matrix is empty,
 * nothing is printed.
 *
 * Requires: m must be a matrix
 * Returns: all elements within m, preserving the order as well
 *)
let show (m: matrix): unit =
    (** HELPER: prints out all elements of vector v*)
    let print_vector (v: vector): unit =
        List.fold_left (fun acc x -> print_string((string_of_int(x)^" ")))
            () v in
    List.fold_left (fun acc x -> print_vector(x)) () m 


(**
 * Takes a matrix m and a vector c and adds c as the right-most column of 
 * m. Fails if the sizes of m and c do not equal.
 *
 * Requires: m is a matrix, and v is a vector
 * Returns: a new matrix with vector v added as the right-most column of m
 *)
let insert_col (m: matrix) (v: vector) : matrix = 
    (** HELPER: removes the first element of v' and appends it to row*)
    let rm_first (v': vector) (row: vector) : (vector * vector) =
        match v' with 
        |[] -> ([], [])
        |h::t -> ((row@[h]), t) in

    if not (List.length(m) = List.length(v)) then
        raise (MatrixFailure "Incompatible")
    else
        List.rev(
            fst(
               List.fold_left(
                    fun dft rows -> (fst(rm_first (snd(dft)) rows)::fst(dft),
                        snd((rm_first (snd(dft)) rows))))
                    ([], v) 
                    m))

(**
 * Takes a matrix m and returns its transpose. A transpose of a matrix is
 * when the rows are the new columns, and vice versa. If the matrix is empty,
 * the empty list is returned.
 *
 * Requires: m is a matrix
 * Returns: a new matrix whose rows are the columns of m and vice versa
 *)
let transpose (m: matrix) : matrix = 
    (**HELPER: creates the accumulator for which the return value is built off*)
    let accumulator (m': matrix) : matrix =
        match m' with
        |[] -> [[]]
        |h::t -> List.fold_left (fun acc _ -> []::acc) [] h in
    List.fold_left (fun acc x -> (insert_col acc x)) (accumulator(m)) m

(**
 * Adds matricies m1 and m2 together. Fails if the sizes of the matricies
 * do not match.
 *
 * Requires: m1 and m2 are both matricies with equivalent size
 * Returns: a matrix that is the sum of m1 and m2
 *)
let add_matrices (m1: matrix) (m2: matrix) : matrix = 
    (**HELPER: adds two vectors together, used to fold along the matricies*)
    let add_vector (v1: vector) (v2: vector) : vector =
            List.rev(List.fold_left2 (fun acc a1 a2 -> (a1+a2)::acc) [] v1 v2) in
    if not(List.length(m1) = List.length(m2)) then
        raise (MatrixFailure "Incompatible Matricies")
    else
        List.rev(List.fold_left2 (fun acc v1 v2 -> (add_vector v1 v2)::acc) [] m1 m2)

(**
 * Multiplies matricies m1 and m2 together. Fails if the sizes of the matricies
 * do not allow for valid multiplication.
 *
 * Requires: m1 and m2 are matricies with sizes that allow for valid multiplication.
             Basically, the number of columns of m1 must be equal to the number of rows
             of m2.
 * Returns: a matrix that is the product of m1 and m2
 *)
let multiply_matrices (m1: matrix) (m2: matrix) : matrix = 
    (**HELPER: returns the dot product of two vectors*)
    let dot_product (v1: vector) (v2: vector) : int =
        List.fold_left2 (fun acc a1 a2 -> (a1*a2)+acc) 0 v1 v2 in
    (**HELPER: returns a row of the product, will be folded along the matricies*)
    let row_by_row (v: vector) (m: matrix) : vector =
        List.rev(List.fold_left (fun acc x -> (dot_product v x)::acc) [] m) in
    if not(List.length(m1) = List.length((transpose(m2)))) then
        raise (MatrixFailure "Incompatible Matricies")
    else
        List.rev(List.fold_left (fun acc x -> (row_by_row x (transpose(m2)))::acc) [] m1)
        
(**
 * Cycles through pattern p and runs f1 or f2 on a WCPat or 
 * VarPat, respectively.
 *
 * Requires: f1 and f2 are functions, p is a pattern
 * Returns: an output based on f1 and f2 on whatever p is
 *)
let rec z f1 f2 p =
  let r = z f1 f2 in
    match p with
    | WCPat -> f1 ()
    | VarPat x -> f2 x
    | TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps
    | StructorPat (_,Some p) -> r p
    | _ -> 0      

(**
 * Counts the number of WCPats in pattern p
 *
 * Requires: p is a pattern
 * Returns: an int counting the number of WCPats in p
 *)
let count_wcs (p: pat) : int =
    z (fun x -> 1) (fun s -> 0) p


(**
 * Returns the sum of the number of wildcards and 
 * the total length of variable names in pattern p
 *
 * Requires: p is a pattern
 * Returns: an int depicting the number of wildcards + length of all variable names
 *)
let count_wcs_and_var_lengths (p: pat) : int =
    z (fun x -> 1) (fun s -> String.length s) p


(**
 * Returns the number of occurences of variable name var_name
 * in pattern p
 *
 * Requires: var_name is a string, p is a pattern
 * Returns: an int of the number of appearances of var_name in p
 *)
let count_var (var_name: string) (p: pat) : int =
    z (fun x -> 0) (fun s -> if s=var_name then 1 else 0) p


(**
 * HELPER: produces a list of all the variable names in pattern p 
 *
 * Requires: p is a pattern
 * Returns: a string list containing all the variable names in p
 *)
let rec extract_names (p: pat) : string list =
    match p with 
    | VarPat x -> [x]
    | TuplePat ps -> List.fold_left (fun acc exp -> (extract_names exp)@acc) [] ps
    | StructorPat (_, Some p) -> extract_names p
    | _ -> [] 


(**
 * HELPER: returns true if the list contains duplicates otherwise false
 *
 * Requires: lst is a list
 * Returns: true or false depending on whether or not all elements in p 
 *          are unique
 *)
let rec has_dups (lst: 'a list) : bool =
    match lst with
    | [] -> false
    | h::t -> (List.mem h t) || has_dups t


(**
 * Returns true if all variables in pattern p have unique
 * names or not, and false otherwise.
 *
 * Requires: p is a pattern
 * Returns: true or false depending on whether all variables in p
 *          are unique
 *)
let all_vars_unique (p: pat) : bool =
    not (has_dups (extract_names p))


(**
 * Implements a function f to every element in lst lst to return
 * Some list, and then takes each list and appends them together to 
 * some list l, then returns Some l.
 *
 * Requires: f is a function that takes an element from lst and returns
 *           Some list, and lst is a list
 * Returns: Some list that contains the answers to all the function's outputs
 *         on lst
 *)
let all_answers (f: 'a -> 'b list option) (lst: 'a list) : 'b list option =
    (**HELPER: applies f to every element in lst and generates list of outputs*)
    let solver (f: 'a -> 'b list option) (lst': 'a list) : 'b list option list =
        List.fold_left (fun acc x -> (f x)::acc) [] lst' in
    (**HELPER: removes Some from every element in a list*)
    let rm_some (b: 'b list option) : 'b list =
        match b with
        |None -> []
        |Some x -> x in
    if List.mem None (solver (f) lst) then
        None
    else
        Some (List.flatten (List.map (rm_some) (solver (f) lst)))


(**
 * Checks whether or not a value matches a pattern. Matching is depicted by 
 * the description on ps2.pdf. If it matches and produces a binding, then 
 * a list of bindings is returned, otherwise None.
 *
 * Requires: v is a value, and p is a pattern
 * Returns: bindings if the matching was successful, None otherwise
 *)
let rec match_pat ((v: value), (p: pat)) : bindings =
    (**HELPER: removes Some from every element in a list *)
    let rm_something (b: 'b list option) : 'b list =
            match b with
            |None -> []
            |Some x -> x in
    (**HELPER: checks to see if every element in v matches its corresponding 
    index in p *)
    let sweeper (v': value list) (p': pat list) : bindings list = 
        List.rev(List.fold_left2 (fun acc v p -> 
                                 (match_pat (v,p))::acc) [] v' p') in
    (**HELPER: checks to see if StructorVal matches StructorPat *)
    let opt_matcher ((somev: value option),(somep: pat option)) : bindings =
        match (somev, somep) with
        | (Some v, Some p) -> match_pat(v,p)
        | (None, None) -> Some []
        | _ -> None in
    match (v,p) with
    | (_, WCPat) -> Some []
    | (_, VarPat s) -> Some [(s,v)]
    | (UnitVal, UnitPat) -> Some []
    | (ConstVal i, ConstPat j) -> if i=j then Some [] else None
    | (TupleVal vals, TuplePat pats) -> 
        if List.length(vals) = List.length(pats) then 
            if List.mem None (sweeper vals pats) then
            None
            else
            Some (List.flatten (List.map (rm_something) (sweeper vals pats)))   
        else 
            None 
    | (StructorVal (s, vopt), StructorPat (s', popt)) -> 
        if s = s' then 
            opt_matcher (vopt, popt)
        else 
            None
    | _ -> None

(**
 * Applies function f to every element in list lst, and on the first
 * non-None output (Some v), returns v.
 *
 * Requires: f is a function that can be applied to each element in lst
 * Returns: The first output of f that isn't None with Some removed
 *)
let first_answer (f: 'a -> 'b option) (lst: 'a list) : 'b = 
    (**HELPER: Returns only the x in Some x *)
    let getter (x: 'b option): 'b =
        match x with
        |Some y -> y 
        |None -> raise (NoAnswer) in
    match List.filter (fun a -> not (a = None)) (List.map (f) lst) with
    |[] -> raise (NoAnswer)
    |h::t -> getter h

(**
 * Takes value v and checks whether it matches any of the patterns in the
 * list pats. Upon the first match, return the bindings it generates, and 
 * returns None if nothing matched
 *
 * Requires: v is a value, pats is a list of patterns
 * Returns: bindings as a result of v matching a pattern in pats, or None
 *)
let match_pats ((v: value), (pats: pat list)) : bindings =
    match List.filter (fun a -> not(a = None)) 
        (List.rev(List.fold_left(fun acc p -> match_pat (v,p)::acc) [] pats)) with
    |[] -> None
    |h::t -> h