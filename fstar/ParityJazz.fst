module ParityJazz

let even n:nat : bool =
    n % 2 = 0

let odd n:nat : bool =
    n % 2 = 1

//
// It is the case that every natural number is even or odd.
//

let _ = assert (forall (n:nat) . (even n) \/ (odd n))

//
// It is the case that every natural number is not both even and odd.
//

let _ = assert (forall (n:nat) . ~((even n) /\ (odd n)))

//
// It is the case that every natural number is either even or odd, but not both.
//

let _ = assert (forall (n:nat) . ((even n) \/ (odd n)) /\ (~((even n) /\ (odd n))))

let xor a b = (a \/ b) /\ ~(a /\ b)

//
// It is the case that every natural number is either even or odd.
//

let _ = assert (forall (n:nat) . (xor (even n) (odd n)))
