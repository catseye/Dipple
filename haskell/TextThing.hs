module TextThing where

import System.Environment

data TextGroup = Chars [String]
               | Words [TextGroup]
               | Lines [TextGroup]
    deriving (Show, Ord, Eq)


whitespace "\n" = True
whitespace "\t" = True
whitespace " "  = True
whitespace _    = False


main = do
    [fileName] <- getArgs
    run fileName

run fileName = do
    a <- readToChars fileName
    let b = groupCharsToLines a
    --let b = groupCharsToWords a
    let c = groupLinesToWords b
    -- ...
    print c

readToChars fileName = do
    a <- readFile fileName
    return $ Chars $ map (\x -> [x]) a

groupCharsToLines (Chars cs) =
    Lines $ g cs
    where
        g [] = []
        g cs =
            let
                (line, rest) = scanLine cs []
            in
                ((Chars line):(g rest))

scanLine [] acc = (reverse acc, [])
scanLine ("\n":cs) acc = (reverse acc, cs)
scanLine (c:cs) acc = scanLine cs (c:acc)


groupCharsToWords (Chars cs) =
    Words $ g cs
    where
        g [] = []
        g cs =
            let
                (word, rest) = scanWord cs []
                (spc, rest') = scanSpaces rest []
            in
                ((Chars word):(Chars spc):(g rest'))

scanWord [] acc = (reverse acc, [])
scanWord (c:cs) acc =
    if whitespace c then (reverse acc, (c:cs)) else scanWord cs (c:acc)

scanSpaces [] acc = (reverse acc, [])
scanSpaces (c:cs) acc =
    if not (whitespace c) then (reverse acc, (c:cs)) else scanSpaces cs (c:acc)


groupLinesToWords (Lines ls) =
    Lines $ map (groupCharsToWords) ls
