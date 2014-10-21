open Mutability
open Iterator
open Assertions

(*Intiation of objects for testing:*)
let c = count_up_from 3 2


(* ------------------------ Testing count_up_from --------------------- *)

TEST_UNIT "count_up_from_test1" = assert_true (
  c () = 3
)

TEST_UNIT "count_up_from_test2" = assert_true (
  c () = 5
)

TEST_UNIT "count_up_from_test3" = assert_true (
  c () = 7
)

(* ----------------------- Testing tabulate --------------------------- *)
TEST_UNIT "tablate_test1" = assert_true (
  tabulate (fun x -> x*x) 4 = [|0,1,4,9|]  
)

TEST_UNIT "tablate_test2" = assert_true (
  tabulate (fun x -> x*x) 0 = [||]  
)

TEST_UNIT "tablate_test3" = assert_true (
  tabulate (fun x -> x) 2 = [|0,1|]  
)

(* ----------------------- Testing fold_left_imp ---------------------- *)
TEST_UNIT "fold_let_imp_test1" = assert_true (
  fold_left_imp (fun a x -> a+1) 0 [1;2;3;4] = 4
)

TEST_UNIT "fold_left_imp_test2" = assert_true (
  fold_left_imp (fun a x -> a+1) 0 [] = 0
)

TEST_UNIT "fold_left_imp_test3" = assert_true (
  fold_left_imp (fun a x -> x::a) [] [1;2;3;4] = [4;3;2;1]
)

(* ----------------------- Testing zardoz ----------------------------- *)
TEST_UNIT "zardoz_test1" = assert_true (
  not (List.map zardoz (List.rev lst) =
       List.rev (List.map zardoz lst))
)

