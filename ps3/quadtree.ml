(*QuadTree is a geometric representation of 2D space, allowing us to store
    objects in this 2D space at specific locations.

  coord - represents a location within a defined region, and is defined 
    by a float pair.
  region - represents an area, defined by a coordinate pair that label the 
    bottom left and top right corners respectively.
  quadtree - represents the 2D space, containing regions cut into 4 more 
    quadtrees up until the diagonal of the region is smaller than min_diagonal
*)
type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let q1 = Leaf (((0., 0.), (10., 10.)), [(5.,5.),"hi"])
let q2 = Leaf (((-10., 0.), (0., 10.)), [])
let q3 = Leaf (((-10., -10.), (0., 0.)), [])
let q4 = Leaf (((0., -10.), (10., 0.)), [])
let n = Node (((-10., -10.) , (10.,10.)), q1,q2,q3,q4)

let new_tree (r:region) : 'a quadtree = 
    Leaf(r, [])
        

let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
    let y_midpoint (tree: 'a quadtree) : float =
        match tree with
        | Node (r, q1, q2, q3, q4) -> 
             (snd(fst(r)) +. snd(snd(r)))/.2.0
        | Leaf(r,_) -> 
             (snd(fst(r)) +. snd(snd(r)))/.2.0 in

    let x_midpoint (tree: 'a quadtree) : float =
        match tree with
        | Node (r,q1,q2,q3,q4) ->
            (fst(fst(r)) +. fst(snd(r)))/.2.0
        | Leaf (r,_) -> 
            (snd(fst(r)) +. snd(snd(r)))/.2.0 in

    let check_region (place: coord) (q': 'a quadtree) : int =
        if fst(place) >= x_midpoint(q') then
            if snd(place) >= y_midpoint(q') then
                1
            else
                4
        else
            if snd(place) >= y_midpoint(q') then
                2
            else
                3 in

    let splitter (l: 'a quadtree) : 'a quadtree =
        match l with 
        | Leaf (r,_) ->
            Node (r, 
                new_tree ((x_midpoint(l), y_midpoint(l)) , snd(r)) , 
                new_tree ((fst(fst(r)) , y_midpoint(l)) , (x_midpoint(l), snd(snd(r)))) , 
                new_tree (fst(r) , (x_midpoint(l), y_midpoint(l))) , 
                new_tree ((x_midpoint(l), snd(fst(r))) , (fst(snd(r)) , y_midpoint(l))) 
            ) 
        | _ -> l in

    let diagonal (r: region) : float =
        match r with
        | ((x1,y1),(x2,y2)) -> ((x1-.x2)**2.+. (y1-.y2)**2.)**0.5 in

    match q with
    | Leaf (r,lst) ->
        if List.length(lst) > 0 && diagonal(r) >= min_diagonal then
            insert (splitter(q)) c s
        else
            Leaf (r, lst @ [(c,s)])
    | Node (r,t1,t2,t3,t4) ->
        if (check_region c q) = 1 then
            insert t1 c s
        else if (check_region c q) = 2 then
            insert t2 c s
        else if (check_region c q) = 3 then
            insert t3 c s
        else 
            insert t4 c s

      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a) (a: 'a) (t: 'b quadtree): 'a =
    match t with
    | Leaf (r, lst) -> 
        List.fold_left f a lst
    | Node (r,t1,t2,t3,t4) ->
        fold_quad f (fold_quad f (fold_quad f (fold_quad f a t1) t2) t3 ) t4

   
let rec fold_region (f: 'a -> (coord * 'b) -> 'a) (a : 'a) (t : 'b quadtree) (r : region): 'a =
    let contains (r: region) (c: coord) : bool = 
        match r with
        | ((x1, y1), (x2, y2)) -> 
            fst(c) > x1 && fst(c) < x2 && snd(c) > y1 && snd(c) < y2 in
    match t with
    | Leaf (r, h::t) -> 
        List.fold_left (fun a x -> 
            if (contains r (fst(h))) then f a x else a) a (h::t)
    | Node (r,t1,t2,t3,t4) ->
        fold_region f (fold_region f (fold_region f (fold_region f a t1) t2) t3 ) t4