module SplitLines where

splitLines :: String -> String -> [String]
splitLines [] line = [reverse line]
splitLines ('\n':rest) line = [reverse line] ++ (splitLines rest [])
splitLines (char:rest) line = splitLines rest (char:line)
