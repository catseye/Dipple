module ListJazz

// It's possible to use lowercase greek letters for type variables in an inductive type definition:

type list α =
    | Nil  : list α
    | Cons : head:α -> tail:list α -> list α

// But, as of this writing, not as parameters to a function (implicit or otherwise).

let rec length #a (l:list a) =
    match l with
        | Nil            -> 0
        | Cons head tail -> 1 + length tail

let rec append #a (la:list a) (lb:list a) : result:list a { length result = length la + length lb } =
    match la with
        | Nil            -> lb
        | Cons head tail -> Cons head (append tail lb)

let rec append_has_nil_as_unit #a (l:list a)
    : Lemma ((append Nil l == l) /\ (append l Nil == l))
    =
    match l with
        | Nil            -> ()
        | Cons head tail -> append_has_nil_as_unit tail
