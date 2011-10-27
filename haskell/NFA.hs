module NFA where

import Char

type State = Integer

data Transition = Transition State Char State
    deriving (Show, Ord, Eq)

type NFA = [Transition]


transitionOnce :: NFA -> [State] -> Char -> [State]

transitionOnce nfa states input =
    let
        test (Transition here x there) =
            x == input && elem here states
        extract (Transition here x there) =
            there
    in
        map (extract) $ filter (test) nfa


transitionMany :: NFA -> [State] -> String -> [State]

transitionMany nfa states [] =
    states
transitionMany nfa states (input:rest) =
    transitionMany nfa (transitionOnce nfa states input) rest


egNFA = [
          Transition 1 'a' 2,
          Transition 1 'a' 3,
          Transition 1 'b' 4,
          Transition 2 'b' 1,
          Transition 3 'b' 2,
          Transition 4 'c' 1
        ]
