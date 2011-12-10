module VariableExpansion where

expand "" alist =
    ""
expand ('$':'{':rest) alist =
    let
        (name, rest') = getName rest
    in
        case lookup name alist of
            Just value ->
                value ++ (expand rest alist)
            Nothing ->
                expand rest alist
expand (c:rest) alist =
    expand rest alist

getName "" =
    ("", "")
getName ('}':rest) =
    ("", rest)
getName (c:rest) =
    let
        (remainder, rest') = getName rest
    in
        ((c:remainder), rest')

test =
    [
      expand "Hello, ${name}!" [("name", "Joe")],
      expand "${a}, ${b}" [("b", "A"), ("a", "B")],
      expand "${no}${way}" [],
      expand "${hmm" [("hmm", "Oh!")]
    ]
