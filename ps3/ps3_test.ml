

(*)
let r = ((42.44,(-76.5)),(42.46,(-76.49)))
let l = city_search (load_city_data "ithaca.csv") r

TEST_UNIT "city_search_test" =
assert_true (match l with
    |["Fall Creek School"] -> true 
    |_-> false)
*)

module AlienTest : AlienMapping = struct
    type aliensym = string 

    let one = "a"
    let zero = ""

    let int_of_aliensym (sym: aliensym) : int =
        String.length(sym)

end
