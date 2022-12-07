module Part2 (solve) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Parse (DirSizes, parse)

solve :: String -> String
solve = show . minimum . Map.elems . sufficient . parse

total :: Int
total = 70000000

needed :: Int
needed = 30000000

shortfall :: DirSizes -> Int
shortfall ds = needed - (total - fromJust (Map.lookup ["/"] ds))

sufficient :: DirSizes -> DirSizes
sufficient ds = Map.filter (>= shortfall ds) ds