module Part2 (solve) where

import Priorities (priority)

solve :: String -> String
solve = show . sum . map (priority . common) . chunks . lines

chunks :: [String] -> [[String]]
chunks s = go s []
  where
    go :: [String] -> [[String]] -> [[String]]
    go [] acc = acc
    go xs acc = let (x, rest) = splitAt 3 xs in go rest (x : acc)

common :: [String] -> Char
common (s : rest) = go s rest
  where
    go :: String -> [String] -> Char
    go (x : xs) ys
      | and (fmap (elem x) ys) = x
      | otherwise = go xs ys
    go _ _ = error "no common element"
common _ = error "no list"