type 'a exprTree =
| Val of 'a
| Unop of ('a -> 'a) * 'a exprTree
| Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree


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
    |x::xs -> List.fold_left (fun a x -> (x+length(a))::a) [] lst 


let insert_col (m: matrix) (v: vector) : matrix = 
	let rm_first (v': vector) (row: vector) : (matrix * vector) =
		match v' with 
		[] -> ([[]], [])
		|h::t -> [[([h]@row)]], t) in

	List.fold_left 
		(fun dft a -> rm_first(dft, a)) ([], v) m 


