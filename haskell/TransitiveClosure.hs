module TransitiveClosure where

--
-- First some crude set theoretic functions
-- (which, unlike Data.Set, don't rely on Ord a =>)
--

insert :: Eq a => a -> [a] -> [a]
insert x xs = if x `elem` xs then xs else x:xs

union :: Eq a => [a] -> [a] -> [a]
union [] b = b
union (x:xs) b = union xs (insert x b)

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) other = x `elem` other && xs `subset` other

seteq :: Eq a => [a] -> [a] -> Bool
seteq a b = a `subset` b && b `subset` a

--
-- Compute f^n(x) until f^n(x) == f^(n-1)(x)
--

iterateToFixpoint :: Eq a => (a -> a) -> a -> a
iterateToFixpoint fun x
    | (fun x) == x = x
    | otherwise    = iterateToFixpoint (fun) (fun x)

--
-- Compute the transitive closure of the function over the set
--

transitiveClosure :: Eq a => (a -> a) -> [a] -> [a]
transitiveClosure fun set
    | seteq set set' = set
    | otherwise      = transitiveClosure (fun) set'
    where
        accum input set = insert (fun input) set
        set' = union set (foldr (accum) [] set)

--
-- Example
--

replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst x y [] = []
replaceFirst x y (c:cs)
    | x == c    = y:cs
    | otherwise = c:(replaceFirst x y cs)

-- *TransitiveClosure> replaceFirst 'a' 'x' "Banana"
-- "Bxnana"
-- *TransitiveClosure> iterateToFixpoint (replaceFirst 'a' 'x') "Banana"
-- "Bxnxnx"
-- *TransitiveClosure> transitiveClosure (replaceFirst 'a' 'x') ["Banana"]
-- ["Banana","Bxnana","Bxnxna","Bxnxnx"]
