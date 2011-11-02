module TransitiveClosure where

transitiveClosure :: Eq a => (a -> a) -> a -> a
transitiveClosure fun x
    | (fun x) == x = x
    | otherwise    = transitiveClosure (fun) (fun x)

replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst x y [] = []
replaceFirst x y (c:cs)
    | x == c    = y:cs
    | otherwise = c:(replaceFirst x y cs)

-- *TransitiveClosure> replaceFirst 'a' 'x' "Banana"
-- "Bxnana"
-- *TransitiveClosure> transitiveClosure (replaceFirst 'a' 'x') "Banana"
-- "Bxnxnx"
