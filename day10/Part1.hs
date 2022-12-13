module Part1 (solve) where

import CPU (eval, parseOp)

solve :: String -> String
solve = show . sum . signals . eval . map parseOp . lines

signals :: [Int] -> [Int]
signals xs = map signalStrength [20, 60, 100, 140, 180, 220]
  where
    signalStrength c = c * xs !! (c - 1)