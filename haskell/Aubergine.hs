-- Interpreter for Aubergine http://esolangs.org/wiki/Aubergine/aubergine.hs
-- does not handle input at all

import qualified Data.Char as Char

--                 a       b       i       program
data State = State Integer Integer Integer [Integer]
     deriving (Ord, Eq, Show)

getAt 0 (head:_)    = head
getAt n (head:tail) = getAt (n-1) tail

getCharAt n l = Char.chr $ fromIntegral $ getAt n l

setAt 0 v (_:tail)    = v:tail
setAt n v (head:tail) = head:setAt (n-1) v tail

getCmd (State _ _ i p) =
    (getCharAt i p, getCharAt (i+1) p, getCharAt (i+2) p)

get '1' _               = 1
get 'a' (State a _ _ _) = a
get 'b' (State _ b _ _) = b
get 'i' (State _ _ i _) = i
get 'A' (State a _ _ p) = getAt a p
get 'B' (State _ b _ p) = getAt b p

set 'a' a (State _ b i p) = State a b i p
set 'b' b (State a _ i p) = State a b i p
set 'i' i (State a b _ p) = State a b i p
set 'A' x (State a b i p) = State a b i $ setAt a x p
set 'B' x (State a b i p) = State a b i $ setAt b x p

advance (State a b i p) = State a b (i+3) p

step :: State -> IO State
step s@(State a b i p) = do
    s' <- case getCmd s of
        ('=', 'o', src) -> do
            putChar $ Char.chr $ fromIntegral $ get src s
            return s
        ('=', dest, src) -> do
            return $ set dest (get src s) s
        ('+', dest, src) -> do
            return $ set dest (get dest s + get src s) s
        ('-', dest, src) -> do
            return $ set dest (get dest s - get src s) s
        (':', dest, src) ->
            case get src s of
                0 -> do return s
                _ -> do return $ State a b (get dest s) p
    return $ advance s'

run :: State -> IO State
run s = do
    s'@(State _ _ i p) <- step s
    let size = fromIntegral $ length p
    if i >= size then return s' else run s'

parse string =
    State 0 0 0 $ map (fromIntegral . Char.ord) string

runString string = run $ parse string

