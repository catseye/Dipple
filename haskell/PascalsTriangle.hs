--
-- Naive Lazy Pascal's Triangle calculator.  It's inefficient enough that
-- you can just type 'pascalsTriangle' at the ghci prompt, and watch it chug
-- along!
--

pascal row 1 = 1
pascal row col
   | col == row = 1
   | col > row = outOfBounds
   | row < 1 = outOfBounds
   | col < 1 = outOfBounds
   | otherwise = (pascal (row-1) (col-1)) + (pascal (row-1) col)

pascalsTriangle = [[pascal row col | col <- [1..row]] | row <- [1..]]

demo = take 10 pascalsTriangle

outOfBounds = error "Out of bounds"

--
-- A less naive, and generally weirder, version.  Unlike pascalsTriangle,
-- this implementation WILL start quickly scrolling off your screen if
-- you just type 'paTri' at the ghci prompt.
--

paTri = paTri' 1
paTri' 1 = [1]:(paTri' 2)
paTri' 2 = [1,1]:(paTri' 3)
paTri' n =
    [gorb x | x <- [1..n]]:(paTri' (n+1))
    where
        prevRow = head $ paTri' (n-1)
        gorb 1 = 1
        gorb col
            | col == n = 1
            | otherwise = (prevRow !! (col-2)) + (prevRow !! (col-1))
