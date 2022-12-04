module Part2 (solve) where

import Parse (parse)

solve :: String -> String
solve = show . length . filter contains . map parse . lines

contains :: ((Int, Int), (Int, Int)) -> Bool
contains ((a, b), (a', b')) =
  (a <= a' && a' <= b)
    || (a' <= a && a <= b')