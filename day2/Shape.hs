module Shape (Shape(..), score, parse) where

data Shape = Rock | Paper | Scissors deriving (Show, Eq) 

score :: Shape -> Int
score Rock = 1
score Paper = 2
score Scissors = 3

parse :: Char -> Shape
parse 'A' =  Rock
parse 'B' =  Paper
parse 'C' =  Scissors
parse 'X' =  Rock
parse 'Y' =  Paper
parse 'Z' =  Scissors
parse x = error $ "impossible shape " <> show x
