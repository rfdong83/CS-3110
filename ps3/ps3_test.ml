open Quadtree
open City_search
open Parser
open Nats
open Assertions

let r = ((42.44,(-76.5)),(42.46,(-76.49))) in
let l = city_search (load_city_data "ithaca.csv") r

TEST_UNIT "city_search_test" =
assert_true (match l with
    |["Fall Creek School"] -> true 
    |_-> false)