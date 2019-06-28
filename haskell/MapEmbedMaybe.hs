module MapEmbedMaybe where

-- I needed a general "mapEmbedMaybe" function (below) for something I was doing.
-- This source just shows how I derived it from more concrete examples.

-- First, a simple function for tests:
-- Given a list of pairs, show those pairs that are not equal.
-- Anything other than an empty lists returns indicates a mistake.

expect [] = []
expect ((a, b):rest) = if a == b then expect rest else ((a, b):expect rest)


-- Given a list of integers, return a list of lists of integers
-- where each list has one of the elements replaced by 0.

mapEmbedInts list =
    mapEmbedInts' list []
    where
        mapEmbedInts' [] prev = []
        mapEmbedInts' (x:xs) prev =
            [prev ++ [0] ++ xs] ++ mapEmbedInts' xs (prev ++ [x])

testMapEmbedInts = expect
  [
    (mapEmbedInts [1, 2, 3], [[0,2,3], [1,0,3], [1,2,0]])
  ]


-- Given a list of a's and an a, return a list of lists of a's
-- where each list has one of the a's replaced by the given a.

mapEmbedRepl list a =
    mapEmbedRepl' list a []
    where
        mapEmbedRepl' [] a prev = []
        mapEmbedRepl' (x:xs) a prev =
            [prev ++ [a] ++ xs] ++ mapEmbedRepl' xs a (prev ++ [x])

testMapEmbedRepl = expect
  [
    (mapEmbedRepl "abc" 'z', ["zbc", "azc", "abz"])
  ]


-- Given a list of a's and a function f from a to a, return a list of lists of a's
-- where each list has one of the a's replaced by the result of applying f to it.

mapEmbed list f =
    mapEmbed' list f []
    where
        mapEmbed' [] f prev = []
        mapEmbed' (x:xs) f prev =
            [prev ++ [f x] ++ xs] ++ mapEmbed' xs f (prev ++ [x])

testMapEmbed = expect
  [
    (mapEmbed [1, 2, 3] (\x -> x + 200), [[201,2,3],[1,202,3],[1,2,203]])
  ]


-- Like mapEmbed, except that f can fail (f maps a to Maybe a), and the lists
-- are only included in the result when f has succeeded.

mapEmbedMaybe list f =
    mapEmbedMaybe' list f []
    where
        mapEmbedMaybe' [] f prev = []
        mapEmbedMaybe' (x:xs) f prev =
            case f x of
                Just y  -> [prev ++ [y] ++ xs] ++ mapEmbedMaybe' xs f (prev ++ [x])
                Nothing -> mapEmbedMaybe' xs f (prev ++ [x])

testMapEmbedMaybe = expect
  [
    (mapEmbedMaybe [1, 2, 3, 4, 5] flt, [[1,2,3,99,5]])
  ]
  where flt 4 = Just 99
        flt _ = Nothing
