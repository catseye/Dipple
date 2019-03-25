module Sexpr where

import Data.Char

data Sexpr = Atom String
           | Cons Sexpr Sexpr
           | Null
  deriving (Ord, Eq)

instance Show Sexpr where
    show (Atom s)   = s
    show (Cons a b) = "(" ++ (show a) ++ (showTail b)
        where showTail Null       = ")"
              showTail (Cons h t) = " " ++ (show h) ++ showTail t
              showTail x          = " . " ++ (show x) ++ ")"
    show Null       = "()"


parse :: String -> (Maybe Sexpr, String)
parse "" = (Nothing, "")
parse (c:rest)
    | isSpace c = parse rest
    | c == '('  = parseList rest
    | c == ')'  = (Nothing, rest)
    | isAlpha c = parseAtom (c:rest)

parseList :: String -> (Maybe Sexpr, String)
parseList s =
    case parse s of
        (Just head, rest) ->
            case parseList rest of
                (Just tail, rest') -> (Just $ Cons head tail, rest')
                (Nothing, rest')   -> (Just Null, rest')
        (Nothing, rest) ->
            (Just Null, rest)

parseAtom :: String -> (Maybe Sexpr, String)
parseAtom "" = (Nothing, "")
parseAtom (c:rest)
    | isAlpha c =
        case parseAtom rest of
            (Nothing, rest')       -> (Just $ Atom [c], rest')
            (Just (Atom a), rest') -> (Just $ Atom ([c] ++ a), rest')
    | otherwise = (Nothing, rest)


testCases = [
    "atom",
    "(cons a b)",
    "    atom    ",
    "   (  cons   a     b   )  ",
    "(zork (cons (cdr a) (list a b () d)) r)"
    ]

test = testAll testCases

testAll [] = return ()
testAll (t:rest) =
    let
        (Just s, remainder) = parse t
    in do
        print (t, s)
        testAll rest
