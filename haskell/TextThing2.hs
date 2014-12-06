module TextThing2 where

import System.Environment

data Text = Char String
          | Word Text
          | Line Text
          | Para Text
          | Seq [Text]
    deriving (Show, Ord, Eq)


whitespace (Char "\n") = True
whitespace (Char "\t") = True
whitespace (Char " ")  = True
whitespace _           = False


main = do
    [fileName] <- getArgs
    run fileName

run fileName = do
    a <- readToChars fileName

    let b = groupCharsToLines a
    print b
    print (replace (Char " ") (Char "!") b)
    putStrLn ""

    let b' = groupCharsToWords a
    print b'
    print (replace (Char " ") (Char "!") b')
    putStrLn ""

    let c = groupLinesToWords b
    print c
    print (replace (Char " ") (Char "!") c)
    putStrLn ""

    let d = groupLinesToParas b
    print d
    print (replace (Char " ") (Char "!") d)
    putStrLn ""

    let d' = groupLinesToParas c
    print d'
    print (replace (Char " ") (Char "!") d')
    putStrLn ""

readToChars fileName = do
    a <- readFile fileName
    return $ Seq $ map (\x -> Char [x]) a

groupCharsToLines (Seq many) =
    Seq $ g many
    where
        g [] = []
        g cs =
            let
                (line, rest) = scanLine cs []
            in
                ((Line $ Seq $ line):(g rest))

scanLine [] acc = (reverse acc, [])
scanLine (Char "\n":cs) acc = (reverse acc, cs)
scanLine (c:cs) acc = scanLine cs (c:acc)


groupCharsToWords (Seq many) =
    Seq $ g many
    where
        g [] = []
        g cs =
            let
                (word, rest) = scanWord cs []
                (spc, rest') = scanSpaces rest []
            in
                ((Word $ Seq $ word):(Word $ Seq $ spc):(g rest'))

scanWord [] acc = (reverse acc, [])
scanWord (c:cs) acc =
    if whitespace c then (reverse acc, (c:cs)) else scanWord cs (c:acc)

scanSpaces [] acc = (reverse acc, [])
scanSpaces (c:cs) acc =
    if not (whitespace c) then (reverse acc, (c:cs)) else scanSpaces cs (c:acc)


groupLinesToWords (Seq many) =
    Seq $ map (\(Line x) -> Line $ groupCharsToWords x) many


groupLinesToParas (Seq many) =
    Seq $ g many
    where
        g [] = []
        g cs =
            let
                (lines, rest) = scanPara cs []
                (spc, rest') = scanBlankLines rest []
            in
                ((Para $ Seq $ lines):(g rest'))

scanPara [] acc = (reverse acc, [])
scanPara (Line (Seq []):ls) acc = (reverse acc, ls)
scanPara (l:ls) acc = scanPara ls (l:acc)

scanBlankLines [] acc = (reverse acc, [])
scanBlankLines (l@(Line (Seq [])):ls) acc = scanBlankLines ls (l:acc)
scanBlankLines (l:ls) acc = (reverse acc, (l:ls))


-- == experimental == --

replace a b (Seq many) =
    Seq $ map (replace a b) many
replace a b t@(Word txt)
    | t == a    = b
    | otherwise = Word $ replace a b txt
replace a b t@(Line txt)
    | t == a    = b
    | otherwise = Line $ replace a b txt
replace a b t@(Para txt)
    | t == a    = b
    | otherwise = Para $ replace a b txt
replace a b t@(Char c)
    | t == a    = b
    | otherwise = t
