module Part1 (solve) where

import Priorities (priority)

solve :: String -> String
solve = show . sum . map (priority . duplicates . split) . lines

split :: String -> (String, String)
split s = splitAt midpoint s
  where
    midpoint = length s `div` 2

duplicates :: (String, String) -> Char
duplicates (x : xs, ys)
  | x `elem` ys = x
  | otherwise = duplicates (xs, ys)
duplicates _ = error "no duplicates"
