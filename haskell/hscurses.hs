module Main where

-- ghc -package hscurses hscurses.hs -o hscurses

import UI.HSCurses.Curses

loop :: Window -> Int -> Int -> IO ()

loop screen x y = do
    refresh
    let x' = if x < 0 then 0 else x
    let y' = if y < 0 then 0 else y
    move y' x'
    keyCode <- getch
    let arf = decodeKey keyCode
    -- wAddStr screen (show arf)
    case arf of
        KeyChar c -> do
            wAddStr screen [c]
            loop screen x' y'
        KeyHome -> do
            loop screen 0 0
        KeyLeft -> do
            loop screen (x'-1) y'
        KeyRight -> do
            loop screen (x'+1) y'
        KeyUp -> do
            loop screen x' (y'-1)
        KeyDown -> do
            loop screen x' (y'+1)
        KeyEnd ->
            return ()

main = do
    initCurses
    screen <- initScr
    cBreak True -- raw True
    echo False
    keypad screen True
    loop screen 0 0    
    keypad screen False
    echo True
    cBreak False -- raw True
    endWin
    return ()
