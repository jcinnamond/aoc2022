module Part2 (solve) where

import Game (Relief (Relief), runTurns)
import Monkey (Monkey (..), Test (getTest), parseMonkeys)

solve :: String -> String
solve s = show . runTurns relief 10000 $ monkeys
  where
    monkeys = parseMonkeys $ lines s
    relief = Relief (`mod` commonMultiples)
    commonMultiples = product $ map getMonkeyTest monkeys

    getMonkeyTest :: Monkey -> Int
    getMonkeyTest (Monkey _ _ t _ _) = getTest t
