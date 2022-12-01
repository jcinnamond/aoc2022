module Main where
import Data.List (sortBy)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy fn inp = go fn inp [] []
    where
        go :: (a -> Bool) -> [a] -> [a] -> [[a]] -> [[a]]
        go _ [] [] zs = zs
        go _ [] ys zs = ys : zs
        go f (x:xs) ys zs 
          | f x = go f xs [] (ys : zs)
          | otherwise = go f xs (x : ys) zs

sumCalories :: [String] -> Int
sumCalories = sum . map read

calories :: String -> [Int]
calories s = map sumCalories $ splitBy (== "") (lines s)

solvePart1 :: String -> String
solvePart1 s = show $ maximum $ calories s

solvePart2 :: String -> String
solvePart2 s = show $ sum $ take 3 $ sortBy (flip compare) $ calories s

solveParts :: String -> String
solveParts s = solvePart1 s <> "\n" <> solvePart2 s

main :: IO ()
main = interact solveParts 