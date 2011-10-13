module Main where

import Control.Monad.State

data Quad = Quad Integer Integer Integer Integer
    deriving (Show)

inner :: State Quad ()
inner = do
    Quad a b c d <- get
    put (Quad a 0 c d)
    return ()

outer :: State Quad Integer
outer = do
    Quad a b c d <- get
    put (Quad a b c (d + 1))
    inner
    return a

quad = Quad 7 8 9 10

transform (Quad a b c d) = (Quad (-5) b c d)

main =
    let
        a = evalState outer quad
        b = execState outer quad
        c = runState outer quad
        d = runState (withState (transform) inner) quad
    in do
        putStrLn (show a)
        putStrLn (show b)
        putStrLn (show c)
        putStrLn (show d)
        return ()
