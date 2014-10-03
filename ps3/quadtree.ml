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

(*Creates a new tree that spans region r.

  Requires: r must be a valid region
  Returns: An empty tree spanning region r *)
let new_tree (r:region) : 'a quadtree = 
    Leaf(r, [])
        

(*Inserts an object s at coordinate c within quadtree q, so long as the 
  coordinate c falls within the region that q spans. If it doesn't, an 
  OutOfBounds error is raised.
  Also, if an object s is inseerted at coordinate c within a Leaf that already
  contains an object, the Leaf will then split into a Node with all of its 
  objects retained as long as the length of the diagonal of the Leaf is greater
  than min_diagonal.
  If an object happens to be on an "axis" after a Leaf is split, it will be 
  considered within the quadrant above the x-axis or to the right of the y-axis,
  whichever applies.

  Boundary cases: A coordinate is not considered to be a part of a region if it
  lies on it's boundary (coordinate (5.,5.) is not in the region from (0.,0.) to 
  (5.,5.) UNLESS it is on it's lower left corner, so (0.,0.) is in the 
  aforementioned region.)

  Requires: q is a tree, c is a coordinate, and s is an object that matches 
            the type of the quadtree
  Returns: A quadtree with everything preserved, and the new object s added at
           coordinate c so long as it was within the region (and a split Leaf 
           if the proper conditions are met. *)
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
    (*HELPER: Returns the y coordinate of the origin of tree's region *)
    let y_midpoint (tree: 'a quadtree) : float =
        match tree with
        | Node (((x1,y1),(x2,y2)), q1, q2, q3, q4) -> 
             (y1 +. y2)/.2.0
        | Leaf(((x1,y1),(x2,y2)),_) -> 
             (y1 +. y2)/.2.0 in
    (*HELPER: Returns the x coordinate of the origin of tree's region *)
    let x_midpoint (tree: 'a quadtree) : float =
        match tree with
        | Node (((x1,y1),(x2,y2)),q1,q2,q3,q4) ->
            (x1 +. x2)/.2.0
        | Leaf (((x1,y1),(x2,y2)),_) -> 
            (x1 +. x2)/.2.0 in
    (*HELPER: Returns whether or not coordinate c is conatined in region r *)
    let contains (r': region) (c: coord) : bool = 
        match r' with
        | ((x1, y1), (x2, y2)) -> 
            fst(c) >= x1 && fst(c) < x2 && snd(c) >= y1 && snd(c) < y2 in
    (*HELPER: Splits a Leaf l into a Node, while preserving all elements
              of the Leaf and moving them to their proper quadrants in the 
              new Node*)
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
    (*HELPER: Returns the length of the diagonal of a region r, the diagonal
              extending from the bottom left to the top right of the region *)
    let diagonal (r: region) : float =
        match r with
        | ((x1,y1),(x2,y2)) -> ((x1-.x2)**2.+. (y1-.y2)**2.)**0.5 in
    (*HELPER: Recursive helper that actually implements the inserting of the
              object s at coordinate c in quadtree q. *)
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
                Node (r, (inserter t1 c s), (inserter t2 c s), 
                         (inserter t3 c s), (inserter t4 c s)) in
    match q with
        | Leaf (r,_) | Node (r,_,_,_,_) -> 
            if (contains r c) then 
                inserter q c s 
            else 
                raise OutOfBounds


(*Applies a function f to all objects within quadtree t. The function f should 
  add to the accumulator a, and the folding should return the final accumulator a.

  Requires: f is a function that takes the accumulator a and changes it according
            to the second argument. t is a quadtree)
  Returns: The accumulator after f has been applied to all objects in quadtree t *)
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a) (a: 'a) (t: 'b quadtree): 'a =
    match t with
    | Leaf (r, lst) -> 
        List.fold_left f a lst
    | Node (r,t1,t2,t3,t4) ->
        fold_quad f (fold_quad f (fold_quad f (fold_quad f a t1) t2) t3 ) t4


(*Same as fold_quad, but instead applies function f to all objects within quadtree
  t that fall under the region r. 
  Boundary cases: A coordinate on the boundary of region r will not be included.

  Requires: r is a region, f is a function that modifies an accumulator according
            to the object argument, t is a tree
  Returns: The accumulator after f has been applied to all objects in quadtree t 
           that are within the given region r. *)
let rec fold_region (f: 'a -> (coord * 'b) -> 'a) (a : 'a) (t : 'b quadtree) 
                    (r : region): 'a =
    (*HELPER: Returns whether or not coordinate c is contianed in region r'. *)
    let contains (r': region) (c: coord) : bool = 
        match r' with
        | ((x1, y1), (x2, y2)) -> 
            fst(c) > x1 && fst(c) < x2 && snd(c) > y1 && snd(c) < y2 in
    (*HELPER: Returns the list of objects of t that are within region r'. *)
    let rec scanner (r': region) (t': 'b quadtree) (accum : (coord * 'b) list ): 
                    (coord * 'b) list =
        match t' with
        | Leaf (r, []) -> 
            accum
        | Leaf (r, h::t) ->
            List.rev (List.fold_left (fun a x-> if (contains r' (fst(x))) 
                                                then  x::a else a) accum (h::t))
        | Node (r,t1,t2,t3,t4) ->
            scanner r' t4 (scanner r' t3 (scanner r' t2 (scanner r' t1 accum))) in
    List.fold_left f a (scanner r t [])
