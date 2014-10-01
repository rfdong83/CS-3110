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

let q1 = Leaf (((0., 0.), (10., 10.)), [((4.,4.),"hi")])
let q2 = Leaf (((-10., 0.), (0., 10.)), [])
let q3 = Leaf (((-10., -10.), (0., 0.)), [])
let q4 = Leaf (((0., -10.), (10., 0.)), [])
let n = Node (((-10., -10.) , (10.,10.)), q1,q2,q3,q4)

let new_tree (r:region) : 'a quadtree = 
    Leaf(r, [])
        

let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
    let y_midpoint (tree: 'a quadtree) : float =
        match tree with
        | Node (((x1,y1),(x2,y2)), q1, q2, q3, q4) -> 
             (y1 +. y2)/.2.0
        | Leaf(((x1,y1),(x2,y2)),_) -> 
             (y1 +. y2)/.2.0 in

    let x_midpoint (tree: 'a quadtree) : float =
        match tree with
        | Node (((x1,y1),(x2,y2)),q1,q2,q3,q4) ->
            (x1 +. x2)/.2.0
        | Leaf (((x1,y1),(x2,y2)),_) -> 
            (x1 +. x2)/.2.0 in

    let contains (r': region) (c: coord) : bool = 
        match r' with
        | ((x1, y1), (x2, y2)) -> 
            fst(c) > x1 && fst(c) < x2 && snd(c) > y1 && snd(c) < y2 in


    let splitter (l: 'a quadtree) : 'a quadtree =
        match l with
        | Leaf (((x1,y1),(x2,y2)), ((coord,obj)::t)) -> 
             insert  
                (Node (((x1,y1),(x2,y2)),
                    new_tree ((x_midpoint(l), y_midpoint(l)), (x2,y2)) ,
                    new_tree ((x1 , y_midpoint(l)) , (x_midpoint(l), y2)) , 
                    new_tree ((x1,y1) , (x_midpoint(l), y_midpoint(l))) , 
                    new_tree ((x_midpoint(l), y1) , (x2 , y_midpoint(l))) 
                        )
                    )
                coord
                obj

        | Leaf (((x1,y1),(x2,y2)), []) -> 
            (Node (((x1,y1),(x2,y2)),
                    new_tree ((x_midpoint(l), y_midpoint(l)), (x2,y2)) ,
                    new_tree ((x1 , y_midpoint(l)) , (x_midpoint(l), y2)) , 
                    new_tree ((x1,y1) , (x_midpoint(l), y_midpoint(l))) , 
                    new_tree ((x_midpoint(l), y1) , (x2 , y_midpoint(l))) 
                        )
                    )

        | _ -> l in

    let diagonal (r: region) : float =
        match r with
        | ((x1,y1),(x2,y2)) -> ((x1-.x2)**2.+. (y1-.y2)**2.)**0.5 in

    let in_bounds (q: 'a quadtree) (c: coord) : bool =
        match q with
        | Leaf (r,_) -> (contains r c)
        | Node (r,_,_,_,_) -> (contains r c) in 
    
    let rec inserter (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
        match q with
            | Leaf (r,lst) ->
                if (contains r c) then
                    if List.length(lst) > 0 && diagonal(r) >= min_diagonal then
                        inserter (splitter q) c s
                    else
                        Leaf (r, lst @ [(c,s)])
                else
                    Leaf (r,lst)
            | Node (r,t1,t2,t3,t4) ->
                Node (r, (inserter t1 c s), (inserter t2 c s), (inserter t3 c s), (inserter t4 c s)) in

    if in_bounds q c then inserter q c s else raise OutOfBounds


let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a) (a: 'a) (t: 'b quadtree): 'a =
    match t with
    | Leaf (r, lst) -> 
        List.fold_left f a lst
    | Node (r,t1,t2,t3,t4) ->
        fold_quad f (fold_quad f (fold_quad f (fold_quad f a t1) t2) t3 ) t4

   
let rec fold_region (f: 'a -> (coord * 'b) -> 'a) (a : 'a) (t : 'b quadtree) (r : region): 'a =
    let contains (r': region) (c: coord) : bool = 
        match r' with
        | ((x1, y1), (x2, y2)) -> 
            fst(c) > x1 && fst(c) < x2 && snd(c) > y1 && snd(c) < y2 in
    let rec scanner (r': region) (t': 'b quadtree) (accum : (coord * 'b) list ): (coord * 'b) list =
        match t' with
        | Leaf (r, []) -> 
            accum
        | Leaf (r, h::t) ->
            List.rev (List.fold_left (fun a x-> if (contains r' (fst(x))) then  x::a else a) accum (h::t))
        | Node (r,t1,t2,t3,t4) ->
            scanner r' t4 (scanner r' t3 (scanner r' t2 (scanner r' t1 accum))) in
    List.fold_left f a (scanner r t [])
