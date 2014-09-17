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
    |_ -> List.rev(List.fold_left (fun acc b -> 
              (f (length(acc)) b)::acc) [] lst)


let outline (lst: string list) : string list =
    let outliner (x: int) (str: string) : string =
        string_of_int (x+1) ^ ". " ^ str in
    mapi_lst outliner lst 


let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
    match lst with 
    |[] -> [acc]
    |_ -> List.rev(List.fold_left (fun a b-> 
              (f (List.hd(a)) b)::a) [acc] lst)


let scan_right (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b list =
    match lst with 
    |[] -> [acc]
    |_ -> List.rev(List.fold_right (fun a b-> 
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


let fact_list (n: int): int list =
    scan_left ( * ) 1 (countup(n))


let show (m: matrix): unit =
    let print_vector(v: vector): unit =
        List.fold_left (fun acc x -> print_string((string_of_int(x)^" ")))
            () v in
    List.fold_left (fun acc x -> print_vector(x)) () m 

let insert_col (m: matrix) (v: vector): matrix =