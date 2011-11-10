--
-- Naive Lazy Pascal's Triangle calculator.  It's inefficient enough that
-- you can just type 'pascalsTriangle' at the ghci prompt, and watch it grow!
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
