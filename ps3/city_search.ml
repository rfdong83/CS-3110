open Parser
open Quadtree

(*Takes a CSV file represented by string s and loads all the cities within
  the file. It then creates a quadtree where all the cities are objects within
  the region of the quadtree. 

  Requires: s is the name of a CSV file (with .csv included)
  Returns: The quadtree representing the cities within the file s *)
let load_city_data (s:string) : string quadtree = 
    (*Takes a city data and inserts it into the accumulating tree acc. *)
    let match_city (acc: string quadtree) (data: city) : string quadtree =
        match data with
        | (x,y,citi) ->
            insert acc (x,y) citi in
    List.fold_left (match_city) 
        (new_tree ((-90.,-180.),(90.,180.))) 
        (parse s) 
(*Returns all of the cities in string quadtree q within a given region r.
  
  Requires: r is a region, q is a string quadtree of cities
  Returns: A list of all the names of q's cities within region r. *)
let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun a x -> snd(x)::a) [] q r 
