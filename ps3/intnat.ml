module IntNat: NATN = struct
    type t = int

    let zero = 0

    let one = 1

    let ( + ) (t1: t) (t2: t) =
        t1 + t2

    let ( * ) (t1: t) (t2: t) = 
        t1 * t2

    let ( === ) (t1: t) (t2: t) =
        t1 = t2

    let ( < ) (t1: t) (t2: t) =
        t1 < t2 






end