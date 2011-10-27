module NFA where

type State = Integer

data (Eq a) => Transition a = Transition State a State


moveOnce :: (Eq a) => [Transition a] -> [State] -> a -> [State]
moveOnce nfa states input =
    let
        test (Transition here x there) =
            x == input && elem here states
        extract (Transition here x there) =
            there
    in
        map (extract) $ filter (test) nfa


moveMany :: (Eq a) => [Transition a] -> [State] -> [a] -> [State]
moveMany nfa = foldl (moveOnce nfa)

--
-- Accepts (in state 6) the strings "ask" and "apt",
-- but not "ast" or "apk"
--

egNFA :: [Transition Char]
egNFA = [
          Transition 1 'a' 2,
          Transition 1 'a' 4,
          Transition 2 's' 3,
          Transition 3 'k' 6,
          Transition 4 'p' 5,
          Transition 5 't' 6
        ]

test str = moveMany egNFA [1] str
