module Parse (parse, coords) where

import Data.Char (ord)

parse :: [String] -> [[Int]]
parse = map (map toInt)
  where
    toInt :: Char -> Int
    toInt c = ord c - 48

coords :: [[Int]] -> [(Int, Int)]
coords g = concatMap (zip [0 .. length (head g) - 1] . repeat) [0 .. length g - 1]
