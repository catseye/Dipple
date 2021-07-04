module ArityJazz

let even n:nat : bool =
    n % 2 = 0

let odd n:nat : bool =
    n % 2 = 1

let _ = assert (forall (n:nat) . (even n) \/ (odd n))

let _ = assert (forall (n:nat) . ~((even n) /\ (odd n)))

let _ = assert (forall (n:nat) . ((even n) \/ (odd n)) /\ (~((even n) /\ (odd n))))

let xor a b = (a \/ b) /\ ~(a /\ b)

let _ = assert (forall (n:nat) . (xor (even n) (odd n)))

