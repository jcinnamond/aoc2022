module Part1 (solve) where

solve :: String -> String
solve = show . map startOfSignal . lines

startOfSignal :: String -> Int
startOfSignal = go 0
  where
    go :: Int -> String -> Int
    go offset (w : x : y : z : rest)
      | allDifferent w x y z = offset + 4
      | otherwise = go (succ offset) (x : y : z : rest)
    go _ _ = error "not enough characters"

    allDifferent :: Eq a => a -> a -> a -> a -> Bool
    allDifferent w x y z =
      w /= x
        && w /= y
        && w /= z
        && x /= y
        && x /= z
        && y /= z