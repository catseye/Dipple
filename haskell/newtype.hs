--
-- We want a function of type T that takes a state and returns a new state and a new function of type T.
-- This is a recursive, i.e. infinite type.
-- Haskell doesn't, by default, allow infinite types.
-- But we can simulate one with 'newtype'!
--

newtype Processor s = Processor (s -> (s, Processor s))

-- Let's have our state be a list of Integers and a boolean "halted" flag.

data Running = Running | Halted

-- Let's define some basic processors.

halt s@(xs, r)     = ((xs, Halted),    Processor halt)

-- A basic processor to remove an element from this list if there is one,
-- and to halt otherwise.

shrink ([], r)     = (([], r),         Processor halt)
shrink ((x:xs), r) = ((xs, r),         Processor shrink)

-- Another, somewhat more interesting processor.

grow ((8:xs), r)   = ((xs, r),         Processor shrink)
grow ((5:xs), r)   = ((xs, r),         Processor halt)
grow ((x:xs), r)   = (((x+1:x:xs), r), Processor grow)

-- An executor, to run processors repeatedly.
-- Note if we leave out the 'Processor' newtype constructor, Haskell complains about an infinite type.

ex s p = case p s of
    ((xs, Halted), _) -> xs
    (s', Processor p') -> ex s' p'

testAll =
  [
    ex ([], Running) halt,
    ex ([], Running) shrink,
    ex ([1,2,3], Running) shrink,
    ex ([1], Running) grow,            -- this one should be [4,3,2,1], all others are []
    ex ([6,5,4,3,2,1], Running) grow
  ]
