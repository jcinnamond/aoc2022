module Part1 (solve) where

import qualified Data.Map.Strict as Map
import Parse (DirSizes, parse)

solve :: String -> String
solve = show . sum . Map.elems . small . parse

small :: DirSizes -> DirSizes
small = Map.filter (<= 100000)
