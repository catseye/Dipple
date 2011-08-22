module Main where

import System.IO
import System.Process

cat handle = do
    eof <- hIsEOF handle
    if
        eof
      then do
        return ()
      else do
        line <- hGetLine handle
        putStr ">>>"
        putStr line
        putStrLn "<<<"
        cat handle

main =
    let
        cmd = (shell "ls -la *.hs"){ std_out = CreatePipe }
    in do
        (_, Just hStdout, _, proc) <- createProcess cmd
        cat hStdout
        exitCode <- waitForProcess proc
        putStrLn (show exitCode)
        return ()
