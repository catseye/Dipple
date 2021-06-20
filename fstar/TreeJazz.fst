module TreeJazz

// A simple inductively-defined type, some simple functions on it,
// with some simple intrinsic and extrinsic proofs of their properties.

type tree a =
    | Leaf   : leaf:a -> tree a
    | Branch : left:tree a -> right:tree a -> tree a

let rec size #a (t:tree a) : result:nat { result >= 1 } =
    match t with
        | Leaf _            -> 1
        | Branch left right -> size left + size right

let rec reflect #a (t:tree a) : result:tree a { size t = size result } =
    match t with
        | Leaf n            -> Leaf n
        | Branch left right -> Branch (reflect right) (reflect left)

let rec reflect_involutive #a (t:tree a) : Lemma (reflect (reflect t) == t) =
    match t with
        | Leaf n            -> ()
        | Branch left right ->
            reflect_involutive right;
            reflect_involutive left
