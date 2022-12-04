module Main (main) where

import qualified Part1
import qualified Part2

main :: IO ()
main = interact solveParts

solveParts :: String -> String
solveParts s = Part1.solve s <> "\n" <> Part2.solve s <> "\n"
