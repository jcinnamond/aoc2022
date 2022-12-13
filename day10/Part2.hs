module Part2 (solve) where

import CPU (eval, parseOp)
import Data.List.Extra (chunksOf)

solve :: String -> String
solve = unlines . draw . eval . map parseOp . lines

draw :: [Int] -> [String]
draw xs = chunksOf 40 $ map drawPx $ pxs xs

drawPx :: (Int, Int) -> Char
drawPx (px, x)
  | px >= x - 1 && px <= x + 1 = '#'
  | otherwise = ' '

pxs :: [Int] -> [(Int, Int)]
pxs = zip (cycle [0 .. 39])
