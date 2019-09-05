--
-- A simple way to evaluate a purely concatenative language is to
-- to sequentially compose the list of functions that the program represents
-- into a single function, and then apply that function.
--
-- Can we use mconcat to do this?  Yes, if we treat them as endofunctors.
--

import Data.Monoid

push n s = (n:s)
add (a:b:s) = (a+b:s)
divide (a:b:s) = (a `div` b:s)
swap (a:b:s) = (b:a:s)
pop (a:s) = s

ops = [(push 6), (push 6), (add), (push 3), (swap), (divide)]

-- fold functions into a single function and apply it (try "run []")
run = foldl (.) id (reverse ops)

-- shorter
frun = foldl (flip (.)) id ops

-- mconcat Endos into a single Endo and appEndo it (try "erun []")
erun = appEndo $ mconcat $ reverse $ map (Endo) ops
