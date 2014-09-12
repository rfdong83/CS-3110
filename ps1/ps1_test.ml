open Ps1
open Assertions

TEST_UNIT "is_mon_inc_test1" = assert_true (is_mon_inc [1;2;3])
TEST_UNIT "is_mon_inc_test2" = assert_true (is_mon_inc [1;2;2])
TEST_UNIT "is_mon_inc_test3" = assert_true (is_mon_inc [1;1;1])
TEST_UNIT "is_mon_inc_test4" = assert_true (is_mon_inc [])
TEST_UNIT "is_mon_inc_test5" = assert_true (is_mon_inc [5])
TEST_UNIT "is_mon_inc_test6" = assert_false (is_mon_inc [3;2;1])
TEST_UNIT "is_mon_inc_test7" = assert_false (is_mon_inc [1;2;1])
TEST_UNIT "is_mon_inc_test8" = assert_false (is_mon_inc [3;2;3])

TEST_UNIT "is_unimodal_test1" = assert_true (is_unimodal [])
TEST_UNIT "is_unimodal_test2" = assert_true (is_unimodal [1])
TEST_UNIT "is_unimodal_test3" = assert_true (is_unimodal [1;1])
TEST_UNIT "is_unimodal_test4" = assert_true (is_unimodal [1;3;5])
TEST_UNIT "is_unimodal_test5" = assert_true (is_unimodal [6;4;2])
TEST_UNIT "is_unimodal_test6" = assert_true (is_unimodal [1;20;1])
TEST_UNIT "is_unimodal_test7" = assert_true (is_unimodal [1;2;2;3;3;2;2;1])
TEST_UNIT "is_unimodal_test8" = assert_false (is_unimodal [22;11;22])
TEST_UNIT "is_unimodal_test9" = assert_false (is_unimodal [1;6;7;-5;20])

TEST_UNIT "powerset_test1" = assert_true (powerset [] = [[]])
TEST_UNIT "powerset_test2" = assert_true (powerset [5] = [[5];[]])
TEST_UNIT "powerset_test3" = assert_true (powerset [1;2] = [[1;2];[1];[2];[]])

TEST_UNIT "rev_int_test1" = assert_true (rev_int 0 = 0)
TEST_UNIT "rev_int_test2" = assert_true (rev_int 1 = 1)
TEST_UNIT "rev_int_test3" = assert_true (rev_int 10 = 1)
TEST_UNIT "rev_int_test4" = assert_true (rev_int 123 = 321)
TEST_UNIT "rev_int_test5" = assert_true (rev_int (-123) = (-321))
TEST_UNIT "rev_int_test6" = assert_true (rev_int 88888 = 88888)

TEST_UNIT "unflatten_test1" = assert_true (unflatten 0 [1;2;3] = None)
TEST_UNIT "unflatten_test1" = assert_true (unflatten (-2) [1;2;3] = None)
TEST_UNIT "unflatten_test1" = assert_true (unflatten 2 [] = Some [[]])
TEST_UNIT "unflatten_test1" = assert_true (unflatten 2 [1;2;3] = Some [[1;2];[3]])
TEST_UNIT "unflatten_test1" = assert_true (unflatten 2 [1;2] = Some [[1;2]])
TEST_UNIT "unflatten_test1" = assert_true (unflatten 5 [1;2;3;4] = Some [[1;2;3;4]])


TEST_UNIT "int_of_roman_test1" = assert_true (int_of_roman [L] = 50)
TEST_UNIT "int_of_roman_test1" = assert_true (int_of_roman [I;I;I] = 3)
TEST_UNIT "int_of_roman_test1" = assert_true (int_of_roman [C;D] = 400)
TEST_UNIT "int_of_roman_test1" = assert_true (int_of_roman [M;M;M;C;X] = 3110)  