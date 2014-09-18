type 'a exprTree =
| Val of 'a
| Unop of ('a -> 'a) * 'a exprTree
| Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree

type vector = int list
type matrix = vector list

exception MatrixFailure of string

let rec count_ops (tree: 'a exprTree): int =
	match tree with
    |Val _ -> 0
    |Unop (a,b) -> 1 + count_ops(b)
    |Binop (a,b,c) -> 1 + count_ops(b) + count_ops(c)


let rec make_fact_tree (x: int): int exprTree =
    if x<0 then raise (Failure "Negative number not allowed")
        else match x with
        |0 -> Val 1
        |_ -> Binop(( * ), Val x, make_fact_tree(x-1))


let rec eval (tree: 'a exprTree): 'a =
    match tree with
    |Val x -> x
    |Unop (a,b) -> a (eval b)
    |Binop (a,b,c) -> a (eval b) (eval c)


let product (lst: float list) : float = 
    List.fold_left ( *. ) 1. lst


let concat_left (lst: string list) : string =
    List.fold_left (^) "" lst


let concat_right (lst: string list) : string =
    List.fold_right (^) lst ""


let mapi_lst (f : int -> 'a -> 'b) (lst: 'a list) : 'b list =
    let length (lst : 'a list) : int =
        List.fold_left(fun a _ -> a+1) 0 lst in
    match lst with
    [] -> []
    |x::xs -> List.rev(List.fold_left (fun a x -> (x+length(a))::a) [] lst) 


let insert_col (m: matrix) (v: vector) : matrix = 
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


let transpose (m: matrix) : matrix = 
    let accumulator (m': matrix) : matrix =
        match m' with
        |[] -> [[]]
        |h::t -> List.fold_left (fun acc _ -> []::acc) [] h in
    List.fold_left (fun acc x -> (insert_col acc x)) (accumulator(m)) m


let add_matricies (m1: matrix) (m2: matrix) : matrix = 
    let add_vector (v1: vector) (v2: vector) : vector =
            List.rev(List.fold_left2 (fun acc a1 a2 -> (a1+a2)::acc) [] v1 v2) in
    if not(List.length(m1) = List.length(m2)) then
        raise (MatrixFailure "Incompatible Matricies")
    else
        List.rev(List.fold_left2 (fun acc v1 v2 -> (add_vector v1 v2)::acc) [] m1 m2)


let multiply_matricies (m1: matrix) (m2: matrix) : matrix = 
    
    let dot_product (v1: vector) (v2: vector) : int =
        List.fold_left2 (fun acc a1 a2 -> (a1*a2)+acc) 0 v1 v2 in
    let row_by_row (v: vector) (m: matrix) : vector =
        List.rev(List.fold_left (fun acc x -> (dot_product v x)::acc) [] m) in
    if not(List.length(m1) = List.length((transpose(m2)))) then
        raise (MatrixFailure "Incompatible Matricies")
    else
        List.rev(List.fold_left (fun acc x -> (row_by_row x (transpose(m2)))::acc) [] m1)
        
        

