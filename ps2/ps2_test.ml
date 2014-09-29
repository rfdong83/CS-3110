open Ps2
open Assertions

let tester (a: int) : 'b list option =
    if a mod 2 = 0 then Some [a] else None

let tester2 (a: 'a) : 'b list option =
    Some [a]

let tester3 (a: 'a) : 'b list option = 
    None

let tester4 (a: 'a) : 'b option =
    Some a

let tester5 (a: 'a) : 'b option =
    if a mod 2 = 0 then Some a else None

TEST_UNIT "count_ops_test1" = assert_true (count_ops (Val 2) = 0)
TEST_UNIT "count_ops_test2" = assert_true (count_ops (Unop ((~-), Val 3)) = 1)
TEST_UNIT "count_ops_test3" = assert_true (count_ops (Binop ((+), Val 2, Val 3)) = 1)
TEST_UNIT "count_ops_test4" = assert_true (count_ops (Binop ((+), Val 3, Unop ((~-), Binop ((/), Val 5, Val 2)))) = 3)

TEST_UNIT "make_fact_tree_test1" = assert_true (make_fact_tree 0 = Val 1)
TEST_UNIT "make_fact_tree_test2" = assert_true (eval(make_fact_tree 1) = 1)
TEST_UNIT "make_fact_tree_test3" = assert_true (eval(make_fact_tree 5) = 120)

TEST_UNIT "eval_test1" = assert_true (eval (Val 2) = 2)
TEST_UNIT "eval_test2" = assert_true (eval (Unop ((~-), Val 3)) = -3)
TEST_UNIT "eval_test3" = assert_true (eval (Binop ((+), Val 2, Val 3)) = 5)

TEST_UNIT "product_test1" = assert_true (product [] = 1.)
TEST_UNIT "product_test2" = assert_true (product [2.] = 2.)
TEST_UNIT "product_test3" = assert_true (product [1.;2.;3.] = 6.)

TEST_UNIT "concat_left_test1" = assert_true (concat_left [] = "")
TEST_UNIT "concat_left_test2" = assert_true (concat_left ["a"] = "a")
TEST_UNIT "concat_left_test3" = assert_true (concat_left ["zar"; "doz"] = "zardoz")

TEST_UNIT "concat_right_test1" = assert_true (concat_right [] = "")
TEST_UNIT "concat_right_test2" = assert_true (concat_right ["a"] = "a")
TEST_UNIT "concat_right_test3" = assert_true (concat_right ["zar"; "doz"] = "zardoz")


TEST_UNIT "mapi_lst_test1" = assert_true (mapi_lst (+) [] = [])
TEST_UNIT "mapi_lst_test2" = assert_true (mapi_lst (+) [0;0;0;0] = [0;1;2;3])
TEST_UNIT "mapi_lst_test3" = assert_true (mapi_lst ( * ) [1;2;3;4] = [0;2;6;12])

TEST_UNIT "outline_test1" = assert_true (outline [] = [])
TEST_UNIT "outline_test2" = assert_true (outline ["hi"] = ["1. hi"])
TEST_UNIT "outline_test3" = assert_true (outline ["cs"; "3110"] = ["1. cs"; "2. 3110"])

TEST_UNIT "scan_left_test1" = assert_true (scan_left (+) 0 [] = [0])
TEST_UNIT "scan_left_test2" = assert_true (scan_left ( * ) 1 [3] = [1;3])
TEST_UNIT "scan_left_test3" = assert_true (scan_left ( * ) 10 [1;2;3] = [10;10;20;60])

TEST_UNIT "scan_right_test1" = assert_true (scan_right (+) [] 0 = [0])
TEST_UNIT "scan_right_test2" = assert_true (scan_right ( * ) [3] 1 = [1;3])
TEST_UNIT "scan_right_test3" = assert_true (scan_right ( * ) [1;2;3] 10 = [10;30;60;60])

TEST_UNIT "fact_list_test1" = assert_true (fact_list 1 = [1])
TEST_UNIT "fact_list_test2" = assert_true (fact_list 3 = [1;2;6])

TEST_UNIT "show_test1" = assert_true (show [[]] = ())

TEST_UNIT "insert_col_test1" = assert_true (insert_col [[1]] [2] = [[1;2]])
TEST_UNIT "insert_col_test2" = assert_true (insert_col [[1;2];[4;5]] [3;6] = [[1;2;3];[4;5;6]])

TEST_UNIT "transpose_test1" = assert_true (transpose [[]] = [])
TEST_UNIT "transpose_test2" = assert_true (transpose [[1]] = [[1]])
TEST_UNIT "transpose_test3" = assert_true (transpose [[1;2;3];[4;5;6]] = [[1;4];[2;5];[3;6]])

TEST_UNIT "add_matrices_test1" = assert_true (add_matrices [[]] [[]] = [[]])
TEST_UNIT "add_matrices_test2" = assert_true (add_matrices [[1]] [[2]] = [[3]])
TEST_UNIT "add_matrices_test3" = assert_true (add_matrices [[1;2;3];[4;5;6]] [[1;2;3];[4;5;6]] = [[2;4;6];[8;10;12]])

TEST_UNIT "multiply_matrices_test1" = assert_true (multiply_matrices [[4]] [[2]] = [[8]])
TEST_UNIT "multiply_matrices_test2" = assert_true (multiply_matrices [[1;2]] [[3];[4]] = [[11]])
TEST_UNIT "multiply_matrices_test3" = assert_true (multiply_matrices [[1;2;3];[4;5;6]] [[7;8];[9;10];[11;12]] = [[58;64];[139;154]])

TEST_UNIT "count_wcs_test1" = assert_true (count_wcs(VarPat "hi") = 0)
TEST_UNIT "count_wcs_test2" = assert_true (count_wcs(WCPat) = 1)
TEST_UNIT "count_wcs_test3" = assert_true (count_wcs(TuplePat [WCPat; WCPat; WCPat; VarPat "hi"]) = 3)

TEST_UNIT "count_wcs_and_var_lengths_test1" = assert_true (count_wcs_and_var_lengths (TuplePat[WCPat; WCPat; WCPat; VarPat "hi"]) = 5)
TEST_UNIT "count_wcs_and_var_lengths_test2" = assert_true (count_wcs_and_var_lengths (TuplePat[UnitPat]) = 0)
TEST_UNIT "count_wcs_and_var_lengths_test3" = assert_true (count_wcs_and_var_lengths (TuplePat[WCPat]) = 1)
TEST_UNIT "count_wcs_and_var_lengths_test4" = assert_true (count_wcs_and_var_lengths (TuplePat[VarPat ""]) = 0)
TEST_UNIT "count_wcs_and_var_lengths_test5" = assert_true (count_wcs_and_var_lengths (TuplePat[VarPat "h"]) = 1)
TEST_UNIT "count_wcs_and_var_lengths_test6" = assert_true (count_wcs_and_var_lengths (TuplePat[WCPat; WCPat]) = 2)

TEST_UNIT "count_var_test1" = assert_true (count_var "hi" (VarPat "hi") = 1 )
TEST_UNIT "count_var_test2" = assert_true (count_var "hi" (VarPat "no") = 0 )
TEST_UNIT "count_var_test3" = assert_true (count_var "hi" (TuplePat[VarPat "hi";VarPat "hi"; VarPat "no"]) = 2 )
TEST_UNIT "count_var_test4" = assert_true (count_var "hi" (TuplePat[VarPat "no";VarPat "no"]) = 0 )

TEST_UNIT "all_vars_unique_test1" = assert_true (all_vars_unique (VarPat "hi") = true)
TEST_UNIT "all_vars_unique_test2" = assert_true (all_vars_unique (TuplePat[VarPat "hi";VarPat "hi"]) = false)
TEST_UNIT "all_vars_unique_test3" = assert_true (all_vars_unique (TuplePat[VarPat "hi"; VarPat "no"]) = true)

TEST_UNIT "all_answers_test1" = assert_true (all_answers tester2 [1;2;3;4;5] = Some [5;4;3;2;1] )
TEST_UNIT "all_answers_test2" = assert_true (all_answers tester2 [] = Some [] )
TEST_UNIT "all_answers_test3" = assert_true (all_answers tester3 ["hi" ; "boo"] = None)

TEST_UNIT "match_pat_test1" = assert_true (match_pat (UnitVal, UnitPat) = Some [])
TEST_UNIT "match_pat_test2" = assert_true (match_pat (UnitVal, VarPat "booyah") = Some ["booyah", UnitVal] )
TEST_UNIT "match_pat_test3" = assert_true (match_pat (UnitVal, WCPat) = Some [])
TEST_UNIT "match_pat_test4" = assert_true (match_pat (ConstVal 1, ConstPat 1) = Some [])
TEST_UNIT "match_pat_test5" = assert_true (match_pat (ConstVal 1, UnitPat) = None)
TEST_UNIT "match_pat_test6" = assert_true (match_pat ((TupleVal[ConstVal 1; ConstVal 2]), (TuplePat[ConstPat 1; ConstPat 2])) = Some [])
TEST_UNIT "match_pat_test7" = assert_true (match_pat ((StructorVal("hi", (Some (TupleVal[UnitVal])) )), (StructorPat("hi" ,(Some (TuplePat[UnitPat])) ))) = Some [])

TEST_UNIT "first_answer_test1" = assert_true (first_answer tester4 [1;2;3;4] = 1)
TEST_UNIT "first_answer_test2" = assert_true (first_answer tester5 [1;2;3;4] = 2)

TEST_UNIT "match_pats_test1" = assert_true ( match_pats ((UnitVal), [UnitPat]) = Some [])
TEST_UNIT "match_pats_test2" = assert_true ( match_pats ((UnitVal), [ConstPat 1]) = None)

