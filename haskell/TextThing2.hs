module TextThing2 where

import System.Environment

data Type = Word | Line | Para
    deriving (Show, Ord, Eq)

data Text = Char String
          | Grp Type Text
          | Seq [Text]
    deriving (Show, Ord, Eq)


whitespace (Char "\n") = True
whitespace (Char "\t") = True
whitespace (Char " ")  = True
whitespace _           = False


notEmptyWord (Grp Word (Seq [])) = False
notEmptyWord _ = True

notChar c (Char d) = not (c == d)
notChar c _ = True


-- could be implemented as the Show...

render (Char s) = s
render (Grp Line t) = (render t) ++ "\n"
render (Grp Para t) = (render t) ++ "\n"
render (Grp Word t) = render t
render (Seq []) = ""
render (Seq (t:ts)) = (render t) ++ render (Seq ts)


main = do
    [fileName] <- getArgs
    run fileName

run fileName = do
    a <- readToChars fileName

    let b = groupCharsToLines a
    let c = groupLinesToWords b
    let d = groupLinesToParas c
    putStr (render d)

    --let d' = filt notEmptyWord d
    --putStr (render d')

    let d' = filt (notChar "a") d
    putStr (render d')


demo fileName = do
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
                ((Grp Line $ Seq $ line):(g rest))

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
                ((Grp Word $ Seq $ word):(Grp Word $ Seq $ spc):(g rest'))

scanWord [] acc = (reverse acc, [])
scanWord (c:cs) acc =
    if whitespace c then (reverse acc, (c:cs)) else scanWord cs (c:acc)

scanSpaces [] acc = (reverse acc, [])
scanSpaces (c:cs) acc =
    if not (whitespace c) then (reverse acc, (c:cs)) else scanSpaces cs (c:acc)


groupLinesToWords (Seq many) =
    Seq $ map (\(Grp Line x) -> Grp Line $ groupCharsToWords x) many


groupLinesToParas (Seq many) =
    Seq $ g many
    where
        g [] = []
        g cs =
            let
                (lines, rest) = scanPara cs []
                (spc, rest') = scanBlankLines rest []
            in
                ((Grp Para $ Seq $ lines):(g rest'))

scanPara [] acc = (reverse acc, [])
scanPara (Grp Line (Seq []):ls) acc = (reverse acc, ls)
scanPara (l:ls) acc = scanPara ls (l:acc)

scanBlankLines [] acc = (reverse acc, [])
scanBlankLines (l@(Grp Line (Seq [])):ls) acc = scanBlankLines ls (l:acc)
scanBlankLines (l:ls) acc = (reverse acc, (l:ls))


-- == experimental == --

replace a b (Seq many) =
    Seq $ map (replace a b) many
replace a b t@(Grp typ txt)
    | t == a    = b
    | otherwise = Grp typ $ replace a b txt
replace a b t@(Char c)
    | t == a    = b
    | otherwise = t


concatSeq t (Seq ts) =
   Seq (t:ts)


filt pred (Seq []) = (Seq [])
filt pred (Seq (t:ts)) =
    case pred t of
        True  -> concatSeq (filt pred t) (filt pred (Seq ts))
        False -> filt pred (Seq ts)
filt pred (Grp typ txt) =
    Grp typ $ filt pred txt
filt pred c@(Char _) =
    c
