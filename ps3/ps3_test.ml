open Quadtree
open Nats
open City_search
open Parser
open Assertions

(*Intiating stuff to test with:*)
module AlienTest : AlienMapping = struct
    type aliensym = string 

    let one = "a"
    let zero = ""

    let int_of_aliensym (sym: aliensym) : int =
        String.length(sym)

end

module Test1 = NatConvertFn(IntNat) 
module Test2 = AlienNatFn(AlienTest)


(* BEGIN TESTING QUADTREE *)


(*Testing regions*)
let r1 = ((-10.,-10.),(10.,10.))
let r2 = ((0.,0.),(10.,10.))
(*Quadrant 1*)
let q1 = Leaf (((0., 0.), (10., 10.)), [])
let q1full = Leaf (((0., 0.), (10., 10.)), [((4.,4.),"hi")])
let q1test = Leaf (((0., 0.), (10., 10.)), [((5.,5.),"testing")])
let q1small = Leaf ( ((0.,0.),(0.00000001,0.00000001)), [((0.00000001,0.00000001),"hi")])
(*Quadrant 2*)
let q2 = Leaf (((-10., 0.), (0., 10.)), [])
let q2test = Leaf (((-10., 0.), (0., 10.)), [(((-5.),5.),"testing")])
(*Quadrant 3*)
let q3 = Leaf (((-10., -10.), (0., 0.)), [])
let q3test = Leaf (((-10., -10.), (0., 0.)), [(((-5.),(-5.)),"testing")])
(*Quadrant 4*)
let q4 = Leaf (((0., -10.), (10., 0.)), [])
let q4test = Leaf (((0., -10.), (10., 0.)), [((5.,(-5.)),"testing")])
(*Basic Node *)
let n = Node (r1,q1,q2,q3,q4)

TEST_UNIT "new_tree_test" = assert_true (
   (new_tree r1) = Leaf (r1,[])
)

TEST_UNIT "insert_test1" = assert_true (
   insert n (5.,5.) "testing" = 
   Node (r1,q1test,q2,q3,q4)
)

TEST_UNIT "insert_test2" = assert_true (
   insert n ((-5.),5.) "testing" = 
   Node (r1,q1,q2test,q3,q4)
)

TEST_UNIT "insert_test3" = assert_true (
   insert n ((-5.),(-5.)) "testing" = 
   Node (r1,q1,q2,q3test,q4)
)

TEST_UNIT "insert_test4" = assert_true (
   insert n (5.,(-5.)) "testing" = 
   Node (r1,q1,q2,q3,q4test)
)

TEST_UNIT "insert_test5" = assert_true (
   insert (Node (r1,q1test,q2,q3,q4)) (4.,4.) "hello" = 
   Node (r1,
      Node (r2, 
         Leaf (((5.,5.),(10.,10.)), [((5.,5.),"testing")]),
         Leaf (((0.,5.),(5.,10.)), []),
         Leaf (((0.,0.),(5.,5.)), [((4.,4.), "hello")]),
         Leaf (((5.,0.),(10.,5.)), [])
      )   
      ,q2,q3,q4
   )
)

TEST_UNIT "insert_test6" = assert_true ( 
   insert q1small (0.000000002,0.000000002) "testing" = 
      Leaf (((0.,0.),(0.00000001,0.00000001)),
      	[((0.00000001,0.00000001),"hi");
      	((0.000000002,0.000000002),"testing")])
)

TEST_UNIT "insert_test7" = assert_raises 
  (Some OutOfBounds) 
  (insert n (11.,11.)) 
  "testing"



TEST_UNIT "fold_quad_test1" = assert_true (
	fold_quad (fun a x -> a+1) 0 n = 0
)

TEST_UNIT "fold_quad_test2" = assert_true (
	fold_quad (fun a x -> a+1) 0 (Node (r1,q1test,q2test,q3test,q4test)) = 4
)

TEST_UNIT "fold_region_test1" = assert_true (
	fold_region (fun a x -> a+1) 0 n ((-6.,-6.),(6.,6.)) = 0
)

TEST_UNIT "fold_region_test2" = assert_true (
	fold_region (fun a x -> a+1)
		0 
		(Node (r1,q1test,q2test,q3test,q4test)) 
		r1 = 4
)

TEST_UNIT "fold_region_test3" = assert_true (
	fold_region (fun a x -> a+1)
		0
		(Node (r1,q1test,q2test,q3test,q4test))
		((-5.,-5.),(5.,5.)) = 0
)


(* FINISH TESTING QUADTREE, BEGIN TESTING CITY_SEARCH *)


TEST_UNIT "load_city_data_test" = assert_true (
	load_city_data "test.csv" = 
	Quadtree.Node (((-90., -180.), (90., 180.)),                                     Quadtree.Node (((0., 0.), (90., 180.)),                                        
  		Quadtree.Leaf (((45., 90.), (90., 180.)), [((89., 179.), " high")]),
  		Quadtree.Leaf (((0., 90.), (45., 180.)), []),
  		Quadtree.Leaf (((0., 0.), (45., 90.)), [((1., 2.), " buckle my shoe")]),
  		Quadtree.Leaf (((45., 0.), (90., 90.)), [])),
 	Quadtree.Leaf (((-90., 0.), (0., 180.)), [((-1., 2.), " test")]),
 	Quadtree.Leaf (((-90., -180.), (0., 0.)), [((-1., -2.), " hello")]),
 	Quadtree.Leaf (((0., -180.), (90., 0.)), [((1., -2.), " abc")]))

)

TEST_UNIT "city_search_test" = assert_true (
	city_search (load_city_data "ithaca.csv") ((42.44,(-76.5)),(42.46,(-76.49))) =
		["First Presbyterian Church";"Fall Creek School"]
)

TEST_UNIT "city_search_test2" = assert_true (
	city_search (load_city_data "ithaca.csv") ((0.,0.),(1.,1.)) = []
)


(* FINISH TESTING CITY_SEARCH, BEGIN TESTING INTNAT *)


TEST_UNIT "int_of_nat_test1" = let open IntNat in assert_true (
	int_of_nat zero = 0
)

TEST_UNIT "int_of_nat_test2" = let open IntNat in assert_true (
	int_of_nat one = 1
)

TEST_UNIT "nat_of_int_test1" = let open IntNat in assert_true (
	nat_of_int 0 = zero
)

TEST_UNIT "nat_of_int_test2" = let open IntNat in assert_true (
	nat_of_int 1 = one
)

TEST_UNIT "( + )_test1" = let open IntNat in assert_true (
	int_of_nat ( ( + ) (nat_of_int 34) (nat_of_int 666) ) = 700
)

TEST_UNIT "( + )_test2" = let open IntNat in assert_true (
	( + ) zero one  = one
)

TEST_UNIT "( + )_test3" = let open IntNat in assert_true (
	( + ) zero one  = ( + ) one zero
)

TEST_UNIT "( + )_test4" = let open IntNat in assert_true (
	( + ) (( + ) zero one) one  = ( + ) zero (( + ) one one)
)

TEST_UNIT "( + )_test5" = let open IntNat in assert_raises 
	(Some Unrepresentable)
	(( + ) (nat_of_int max_int)) (nat_of_int 1) 


TEST_UNIT "( * )_test1" = let open IntNat in assert_true (
	int_of_nat ( ( * ) (nat_of_int 70) (nat_of_int 10) ) = 700
)

TEST_UNIT "( * )_test2" = let open IntNat in assert_true (
	int_of_nat ( ( * ) (nat_of_int 3) (one) ) = 3 
)

TEST_UNIT "( * )_test3" = let open IntNat in assert_true (
	( * ) zero one  = ( * ) one zero
)

TEST_UNIT "( * )_test4" = let open IntNat in assert_true (
	( * ) (( * ) zero one) one  = ( * ) zero (( * ) one one)
)

TEST_UNIT "( + )_test5" = let open IntNat in assert_raises 
	(Some Unrepresentable)
	(( * ) (nat_of_int max_int)) (nat_of_int 2) 

TEST_UNIT "( === )_test1" = let open IntNat in assert_true (
	( === ) (nat_of_int 31) (nat_of_int 31)
)

TEST_UNIT "( === )_test2" = let open IntNat in assert_false (
	( === ) (nat_of_int 31) (nat_of_int 2)
)

TEST_UNIT "( < )_test1" = let open IntNat in assert_true (
	( < ) (nat_of_int 31) (nat_of_int 32)
)

TEST_UNIT "( < )_test2" = let open IntNat in assert_false (
	( < ) (nat_of_int 31) (nat_of_int 31)
)


(* FINISH TESTING INTNAT, BEGIN TESTING LISTNAT *)


TEST_UNIT "int_of_nat_test1" = let open ListNat in assert_true (
	int_of_nat zero = 0
)

TEST_UNIT "int_of_nat_test2" = let open ListNat in assert_true (
	int_of_nat one = 1
)

TEST_UNIT "nat_of_int_test1" = let open ListNat in assert_true (
	nat_of_int 0 = zero
)

TEST_UNIT "nat_of_int_test2" = let open ListNat in assert_true (
	nat_of_int 1 = one
)

TEST_UNIT "( + )_test1" = let open ListNat in assert_true (
	int_of_nat ( ( + ) (nat_of_int 34) (nat_of_int 666) ) = 700
)

TEST_UNIT "( + )_test2" = let open ListNat in assert_true (
	( + ) zero one  = one
)

TEST_UNIT "( + )_test3" = let open ListNat in assert_true (
	( + ) zero one  = ( + ) one zero
)

TEST_UNIT "( + )_test4" = let open ListNat in assert_true (
	( + ) (( + ) zero one) one  = ( + ) zero (( + ) one one)
)

TEST_UNIT "( + )_test5" = let open IntNat in assert_raises 
	(Some Unrepresentable)
	(( + ) (nat_of_int max_int)) (nat_of_int 1) 

TEST_UNIT "( * )_test1" = let open ListNat in assert_true (
	int_of_nat ( ( * ) (nat_of_int 70) (nat_of_int 10) ) = 700
)

TEST_UNIT "( * )_test2" = let open ListNat in assert_true (
	int_of_nat ( ( * ) (nat_of_int 3) (one) ) = 3 
)

TEST_UNIT "( * )_test3" = let open ListNat in assert_true (
	( * ) zero one  = ( * ) one zero
)

TEST_UNIT "( * )_test4" = let open ListNat in assert_true (
	( * ) (( * ) zero one) one  = ( * ) zero (( * ) one one)
)

TEST_UNIT "( * )_test5" = let open IntNat in assert_raises 
	(Some Unrepresentable)
	(( * ) (nat_of_int max_int)) (nat_of_int 2) 

TEST_UNIT "( === )_test1" = let open ListNat in assert_true (
	( === ) (nat_of_int 31) (nat_of_int 31)
)

TEST_UNIT "( === )_test2" = let open ListNat in assert_false (
	( === ) (nat_of_int 31) (nat_of_int 2)
)

TEST_UNIT "( < )_test1" = let open ListNat in assert_true (
	( < ) (nat_of_int 31) (nat_of_int 32)
)

TEST_UNIT "( < )_test2" = let open ListNat in assert_false (
	( < ) (nat_of_int 31) (nat_of_int 31)
)


(* FINISH TESTING LISTNAT, BEGIN TESTING NATCONVERTFN *)


TEST_UNIT "int_of_nat_test1" = let open Test1 in assert_true (
	int_of_nat zero = 0
)

TEST_UNIT "int_of_nat_test2" = let open Test1 in assert_true (
	int_of_nat one = 1
)

TEST_UNIT "nat_of_int_test1" = let open Test1 in assert_true (
	nat_of_int 0 = zero
)

TEST_UNIT "nat_of_int_test2" = let open Test1 in assert_true (
	nat_of_int 1 = one
)

TEST_UNIT "( + )_test1" = let open Test1 in assert_true (
	int_of_nat ( ( + ) (nat_of_int 34) (nat_of_int 666) ) = 700
)

TEST_UNIT "( + )_test2" = let open Test1 in assert_true (
	( + ) zero one  = one
)

TEST_UNIT "( + )_test3" = let open Test1 in assert_true (
	( + ) zero one  = ( + ) one zero
)

TEST_UNIT "( + )_test4" = let open Test1 in assert_true (
	( + ) (( + ) zero one) one  = ( + ) zero (( + ) one one)
)

TEST_UNIT "( + )_test5" = let open Test1 in assert_raises 
	(Some IntNat.Unrepresentable)
	(( + ) (nat_of_int max_int)) (nat_of_int 1) 


TEST_UNIT "( * )_test1" = let open Test1 in assert_true (
	int_of_nat ( ( * ) (nat_of_int 70) (nat_of_int 10) ) = 700
)

TEST_UNIT "( * )_test2" = let open Test1 in assert_true (
	int_of_nat ( ( * ) (nat_of_int 3) (one) ) = 3 
)

TEST_UNIT "( * )_test3" = let open Test1 in assert_true (
	( * ) zero one  = ( * ) one zero
)

TEST_UNIT "( * )_test4" = let open Test1 in assert_true (
	( * ) (( * ) zero one) one  = ( * ) zero (( * ) one one)
)

TEST_UNIT "( + )_test5" = let open Test1 in assert_raises 
	(Some IntNat.Unrepresentable)
	(( * ) (nat_of_int max_int)) (nat_of_int 2)

TEST_UNIT "( === )_test1" = let open Test1 in assert_true (
	( === ) (nat_of_int 31) (nat_of_int 31)
)

TEST_UNIT "( === )_test2" = let open Test1 in assert_false (
	( === ) (nat_of_int 31) (nat_of_int 2)
)

TEST_UNIT "( < )_test1" = let open Test1 in assert_true (
	( < ) (nat_of_int 31) (nat_of_int 32)
)

TEST_UNIT "( < )_test2" = let open Test1 in assert_false (
	( < ) (nat_of_int 31) (nat_of_int 31)
)


(*-----------------TESTING ALIEN FUNCTOR----------------------*)

(*TESTING INT_OF_NAT AND NAT_OF INT*)
TEST_UNIT "AlienNatFn_test1" = let open Test2 in assert_true (
  (int_of_nat(nat_of_int 4)) = 4
)

TEST_UNIT "AlienNatFn_test2" = let open Test2 in assert_true (
  (int_of_nat(nat_of_int 5)) = 5
)

TEST_UNIT "AlienNatFn_test3" = let open Test2 in assert_true (
  (nat_of_int 1) = one
)

TEST_UNIT "AlienNatFn_test4" = let open Test2 in assert_true (
  (nat_of_int 0) = zero
) 


(*TESTING ADDITION*)
TEST_UNIT "AlienNatFn_test5" = let open Test2 in assert_true (
  int_of_nat((nat_of_int 5) + (nat_of_int 6)) = 11
)

TEST_UNIT "AlienNatFn_test6" = let open Test2 in assert_true (
  (nat_of_int 6) + (nat_of_int 5) = (nat_of_int 5) + (nat_of_int 6)
)

TEST_UNIT "AlienNatFn_test7" = let open Test2 in assert_true (
  int_of_nat((nat_of_int 5) + (nat_of_int 0)) = 5
)

TEST_UNIT "AlienNatFn_test8" = let open Test2 in assert_true (
  ((nat_of_int 5) + (nat_of_int 6)) + (nat_of_int 7) =
  (nat_of_int 5) + ((nat_of_int 6) + (nat_of_int 7))
)


(*TESTING MUTLIPLICATION*)
TEST_UNIT "AlienNatFn_test9" = let open Test2 in assert_true (
  int_of_nat((nat_of_int 10) * (nat_of_int 15)) = 150
)

TEST_UNIT "AlienNatFn_test10" = let open Test2 in assert_true (
  (nat_of_int 10) * zero = zero
)

TEST_UNIT "AlienNatFn_test11" = let open Test2 in assert_true (
  int_of_nat((nat_of_int 10) * one) = 10
)

TEST_UNIT "AlienNatFn_test12" = let open Test2 in assert_true (
  ((nat_of_int 2) * (nat_of_int 3)) * (nat_of_int 4) = 
  (nat_of_int 2) * ((nat_of_int 3) * (nat_of_int 4))
)

TEST_UNIT "AlienNatFn_test13" = let open Test2 in assert_true (
  (nat_of_int 10) * (nat_of_int 15) = (nat_of_int 15) * (nat_of_int 10)
)


(*TESTING EQUALITY*)
TEST_UNIT "AlienNatFn_test14" = let open Test2 in assert_true (
  (nat_of_int 10) === (nat_of_int 10)
)

TEST_UNIT "AlienNatFn_test15" = let open Test2 in assert_true (
  not ((nat_of_int 10) === (nat_of_int 9))
)


(*TESTING LESS THAN*)
TEST_UNIT "AliennatFn_test16" = let open Test2 in assert_true (
  (nat_of_int 5) < (nat_of_int 6)
)

TEST_UNIT "AlienNatFn_test17" = let open Test2 in assert_true (
  not ((nat_of_int 5) < (nat_of_int 4))
)

