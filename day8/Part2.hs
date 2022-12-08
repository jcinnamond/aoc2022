module Part2 (solve) where

import Parse (coords, parse)

solve :: String -> String
solve s = show $ maximum $ map (scenic grid) $ coords grid
  where
    grid = parse $ lines s

scenic :: [[Int]] -> (Int, Int) -> Int
scenic g p@(x, y) = up * down * left * right
  where
    up = distance (at p g) colUp
    down = distance (at p g) colDown
    left = distance (at p g) rowLeft
    right = distance (at p g) rowRight

    colUp = reverse $ map (!! x) (take y g)
    colDown = map (!! x) (drop (y + 1) g)
    rowLeft = reverse $ take x (g !! y)
    rowRight = drop (x + 1) (g !! y)

distance :: Int -> [Int] -> Int
distance _ [] = 0
distance h (t : rest)
  | t < h = 1 + distance h rest
  | otherwise = 1

at :: (Int, Int) -> [[Int]] -> Int
at (x, y) g = g !! y !! x
