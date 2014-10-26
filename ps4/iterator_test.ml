open Assertions
open Iterator

(* <<<<<<<<<<<<<<<<<<<<<<<<<<< ITERATOR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*)

(* Initializing objects to test with: *)
let filled = ListIterator.create [1;2;3;4]
let filled2 = ListIterator.create [1;2;3;4]
let yum = ListIterator.create [1;2;3;4]
let bum = ListIterator.create [1;2;3;4]


let tree = Node (10, 
              Node (5, 
                Node (3, Leaf, Leaf), 
                Node (7, Leaf, Leaf)),
              Node (15, 
                Node (13, Leaf, Leaf), 
                Node (17, Leaf, Leaf))
           )
let itree = InorderTreeIterator.create tree

module Test = TakeIterator (ListIterator)
let take = Test.create 3 filled2

module Test2 = IteratorUtilsFn (ListIterator)

module Test3 = RangeIterator (ListIterator)
let hurrah = ListIterator.create [0;1;2;3;4;5]
let range = Test3.create 3 5 hurrah

(* ----------------------- Testing ListIterator ----------------------- *)

TEST_UNIT "ListIterator_test1" = assert_true (
  ListIterator.has_next filled
)

TEST_UNIT "ListIterator_test2" = assert_true (
  ListIterator.next filled = 1
)

TEST_UNIT "ListIterator_test3" = assert_true (
  ListIterator.next filled = 2
)

TEST_UNIT "ListIterator_test4" = assert_true (
  ListIterator.next filled = 3
)

TEST_UNIT "ListIterator_test5" = assert_true (
  ListIterator.next filled = 4
)

TEST_UNIT "ListIterator_test6" = assert_true (
  ListIterator.has_next filled = false 
)

TEST_UNIT "ListIterator_test7" = assert_raises (
  (Some ListIterator.NoResult)) (ListIterator.next) (filled) 



(* ---------------- Testing InOrderTree Iterator --------------------- *)

TEST_UNIT "InorderTreeIterator_test1" = assert_true (
  InorderTreeIterator.has_next itree
)

TEST_UNIT "InorderTreeIterator_test2" = assert_true (
  InorderTreeIterator.next itree = 3
)

TEST_UNIT "InorderTreeIterator_test3" = assert_true (
  InorderTreeIterator.next itree = 5
)

TEST_UNIT "InorderTreeIterator_test4" = assert_true (
  InorderTreeIterator.next itree = 7
)

TEST_UNIT "InorderTreeIterator_test5" = assert_true (
  InorderTreeIterator.next itree = 10
)

TEST_UNIT "InorderTreeIterator_test6" = assert_true (
  InorderTreeIterator.next itree = 13
)

TEST_UNIT "InorderTreeIterator_test7" = assert_true (
  InorderTreeIterator.next itree = 15
)

TEST_UNIT "InorderTreeIterator_test8" = assert_true (
  InorderTreeIterator.next itree = 17
)

TEST_UNIT "InorderTreeIterator_test8" = assert_true (
  InorderTreeIterator.has_next itree = false
)

TEST_UNIT "InorderTreeIterator_test9" = assert_raises (
  (Some InorderTreeIterator.NoResult)) (InorderTreeIterator.next) (itree)



(* ---------------------- Testing TakeIterator --------------------- *)

TEST_UNIT "TakeIterator_test1" = assert_true (
  Test.has_next take
)

TEST_UNIT "TakeIterator_test2" = assert_true (
  Test.next take = 1
)

TEST_UNIT "TakeIterator_test3" = assert_true (
  Test.next take = 2
)

TEST_UNIT "TakeIterator_test4" = assert_true (
  Test.next take = 3
)

TEST_UNIT "TakeIterator_test5" = assert_raises (
  (Some Test.NoResult)) (Test.next) (take)


TEST_UNIT "TakeIterator_test6" = assert_true (
  Test.has_next take = true 
)

(* ------------------- Testing IteratorUtilsFn -------------------- *)

TEST_UNIT "IteratorUtilsFn_test1" = assert_true (
  Test2.fold (fun a x -> a + 1) 0 bum = 4
)

TEST_UNIT "IteratorUtilsFn_test2" = assert_true (
  Test2.advance 2 yum;
  ListIterator.create [3;4] = yum
)

TEST_UNIT "IteratorUtilsFn_test3" = assert_true (
  Test2.advance 1 yum;
  ListIterator.create [4] = yum
)

(* ------------------ Testing RangeIterator ----------------------- *)

TEST_UNIT "RangeIterator_test1" = assert_true (
  Test3.has_next range = true
)

TEST_UNIT "RangeIterator_test2" = assert_true (
  Test3.next range = 2
)

TEST_UNIT "RangeIterator_test3" = assert_true (
  Test3.next range = 3
)

TEST_UNIT "RangeIterator_test4" = assert_true (
  Test3.next range = 4
)

TEST_UNIT "RangeIterator_test5" = assert_true (
  Test3.has_next range = false
)