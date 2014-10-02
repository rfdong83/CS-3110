open Parser
open Quadtree

let load_city_data (s:string) : string quadtree = 
    let match_city (acc: string quadtree) (data: city) : string quadtree =
        match data with
        | (x,y,citi) ->
            insert acc (x,y) citi in
    List.fold_left (match_city) 
        (new_tree ((-90.,-180.),(90.,180.))) 
        (parse s) 

let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun a x -> snd(x)::a) [] q r 
