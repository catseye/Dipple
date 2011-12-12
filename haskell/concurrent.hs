module Main where

import Control.Concurrent
import Control.Concurrent.Chan

data Done = Done ThreadId
    deriving (Show, Ord, Eq)

-- For orderly output, there is a single reporter thread.

reporter reportChan controlChan = do
    message <- readChan reportChan
    case message of
        "QUIT" -> do
            putStrLn "reporter thread done now"
            tid <- myThreadId
            writeChan controlChan $ Done tid
            return ()
        _ -> do
            putStrLn ("-->" ++ message)
            reporter reportChan controlChan

worker reportChan controlChan 20 = do
    tid <- myThreadId
    writeChan controlChan $ Done tid
    return ()
worker reportChan controlChan x = do
    writeChan reportChan (show x)
    worker reportChan controlChan (x+1)

-- Channels can be shared.

main = do
    reportChan <- newChan
    controlChan <- newChan
    reporterThread <- forkIO (reporter reportChan controlChan)
    thread1 <- forkIO (worker reportChan controlChan 0)
    thread2 <- forkIO (worker reportChan controlChan 0)
    result1 <- readChan controlChan
    writeChan reportChan (show result1)
    result2 <- readChan controlChan
    writeChan reportChan (show result2)
    writeChan reportChan "QUIT"
    result2 <- readChan controlChan
    putStrLn "All done!"
    return ()
