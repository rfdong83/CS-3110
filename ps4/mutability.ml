(*
Takes n and adds k*i, where i represents the number of times the 
function has been called in the past.
Requires: n,k are ints
*)
let count_up_from n k =
  let i = ref (-1) in 
  fun () -> (i := !i + 1); n + k * !i


(*
Generates an array of length n, where f is called on the index at every
position.
Requires: f to be (int -> 'a) and n to be an int
Ex: tabulate (fun x -> x*x) 4 = {[0,1,4,9]}
*)
let tabulate f n = 
  Array.init n f 
  

(*
Recreates fold_left on arrays without using the rec keyword or the use of
for/while loops
Requires: f is type ('a -> 'b -> 'a), acc is type 'a, and xs is
type 'b array
*)
let fold_left_imp f acc xs = 
  let a = ref acc in 
  let x = ref xs in 
  while !x <> [] do 
    match !x with
    | h::t -> x := t; a := f !a h
    | _ -> a := !a
  done;
  !a


type t = string  
type u = int
let lst : t list = ["1";"20";"300"]
let count = ref 1
(*
A function that proves that reversing a list and applying map does not always
equal applying map and then reversing it, when allowed to use imperative 
features of Ocaml.
Requires: x to be type t
*)
let zardoz (x: t) : u =  
  (count := !count + 1);
  (String.length x) + !count