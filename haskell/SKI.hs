-- Evaluator for Combinatory Logic (SKI-calculus)
-- I am not entirely convinced that it is correct

data Term = S
          | K
          | I
          | Pair Term Term
    deriving (Ord, Show, Eq)


step S = S
step K = K
step I = I
step (Pair I x) = x
step (Pair (Pair K x) y) = x
step (Pair (Pair (Pair S x) y) z) = (Pair (Pair x z) (Pair y z))
step (Pair l r) = (Pair (eval l) (eval r))

eval term =
    let
        term' = step term
    in
        if term == term' then
            term
        else
            eval term'

parseChar 'S' = S
parseChar 'K' = K
parseChar 'I' = I

kParse (' ':rest) =
    kParse rest
kParse ('(':rest) =
    let
        (t, rest') = kParse rest
    in
        bParse rest' t
kParse (char:rest) =
    bParse rest (parseChar char)

bParse [] acc =
    (acc, [])
bParse (' ':rest) acc =
    bParse rest acc
bParse (')':rest) acc =
    (acc, rest)
bParse ('(':rest) acc =
    let
        (t, rest') = kParse rest
    in
        bParse rest' (Pair acc t)
bParse (char:rest) acc =
    bParse rest $ Pair acc (parseChar char)


run x = eval $ fst $ kParse x
