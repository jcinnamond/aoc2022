module Outcome (Outcome(..), score, parse) where

data Outcome = Win | Loss | Draw deriving (Show, Eq)

score :: Outcome -> Int
score Win = 6
score Draw = 3
score Loss = 0

parse :: Char -> Outcome
parse 'X' = Loss
parse 'Y' = Draw
parse 'Z' = Win
parse x = error $ "impossible outcome " <> show x
