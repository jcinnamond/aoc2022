module Part1 (solve) where

import Game (Relief (Relief), runTurns)
import Monkey (parseMonkeys)

solve :: String -> String
solve = show . runTurns (Relief (`div` 3)) 20 . parseMonkeys . lines
