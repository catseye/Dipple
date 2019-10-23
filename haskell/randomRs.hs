import System.Random

-- You don't need monads to generate pseudorandom numbers in Haskell.

test seed =
    let
        generator = mkStdGen seed
        infiniteListOfPseudoRandomNumbers = randomRs (0, 99) generator
    in
        take 50 infiniteListOfPseudoRandomNumbers
