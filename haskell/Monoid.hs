-- An example of an instance of Monoid.

import Data.Monoid

data Thingy x = Thingy ([x] -> [x]) [x]

instance Monoid (Thingy x) where
    mappend (Thingy fa sa) (Thingy fb sb) = Thingy (fa . fb) (sa ++ sb)
    mempty = Thingy id []

-- Not something monoids need, but this makes Thingys a bit like functions.

mapply (Thingy f s) = f s

add = Thingy (\(x:y:s) -> (x+y:s)) []
lit n = Thingy id [n]

test1 = mapply $ mconcat [lit 10, lit 55, add]
test2 = mapply $ mconcat [lit 10, add, lit 55]
test3 = mapply $ mconcat [add, lit 10, lit 55]

-- (Monoid, Monoid) is already an instance of Monoid.  So we could just
-- use a pair instead of our Thingy type constructor.  However, the
-- definition of mappend for pairs of monoids is apparently quite different
-- than what we used above.  I am not entirely sure what it's doing.

padd = ((\(x:y:s) -> (x+y:s)), [])
plit n = (id, [n])

papply (f, s) = f s

test4 = papply $ mconcat [padd, plit 10, plit 55]
test5 = snd $ mconcat [padd, plit 10, plit 55]
test6 = let f = fst $ mconcat [padd, plit 10, plit 55] in f [7,7]
test7 = let f = fst $ mconcat [padd] in f [7,7]
test8 = let f = fst $ mconcat [plit 3, plit 4] in f [5, 6]
