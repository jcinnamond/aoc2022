module Part1 (solve) where

import Parse (coords, parse)

solve :: String -> String
solve s = show $ length $ filter (== True) $ map (visible grid) $ coords grid
  where
    grid = parse $ lines s

visible :: [[Int]] -> (Int, Int) -> Bool
visible _ (0, _) = True
visible _ (_, 0) = True
visible g (x, y)
  | x == length (head g) - 1 = True
  | y == length g - 1 = True
  | otherwise =
      checkUp (x, y) g
        || checkDown (x, y) g
        || checkLeft (x, y) g
        || checkRight (x, y) g

checkUp :: (Int, Int) -> [[Int]] -> Bool
checkUp p@(x, y) g = all ((< at p g) . (!! x)) (take y g)

checkDown :: (Int, Int) -> [[Int]] -> Bool
checkDown p@(x, y) g = all ((< at p g) . (!! x)) (drop (y + 1) g)

checkLeft :: (Int, Int) -> [[Int]] -> Bool
checkLeft p@(x, y) g = all (< at p g) (take x (g !! y))

checkRight :: (Int, Int) -> [[Int]] -> Bool
checkRight p@(x, y) g = all (< at p g) (drop (x + 1) (g !! y))

at :: (Int, Int) -> [[Int]] -> Int
at (x, y) g = g !! y !! x
