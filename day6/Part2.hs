module Part2 (solve) where

import Data.List (nub)

solve :: String -> String
solve = show . map startOfMessage . lines

startOfMessage :: String -> Int
startOfMessage = go 0
  where
    go :: Int -> String -> Int
    go offset s
      | unique (take 14 s) = offset + 14
      | otherwise = go (succ offset) (drop 1 s)

unique :: String -> Bool
unique s = s == nub s