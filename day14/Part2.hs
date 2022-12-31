module Part2 (solve) where

import Cave (Cave (..), Pos (..), isAirWithFloor, parse, startPos)
import Control.Applicative ((<|>))
import qualified Data.Set as Set

solve :: String -> String
solve = show . (+ 1) . length . sand . trickle . parse

trickle :: Cave -> Cave
trickle g =
  let p = dropSand startPos
   in if p == startPos
        then g
        else trickle $ g {sand = Set.insert p $ sand g}
  where
    dropSand p = maybe p dropSand (fallDown p <|> fallLeft p <|> fallRight p)

    fallDown (Pos (x, y)) = fall $ Pos (x, y + 1)
    fallLeft (Pos (x, y)) = fall $ Pos (x - 1, y + 1)
    fallRight (Pos (x, y)) = fall $ Pos (x + 1, y + 1)

    fall p = if isAirWithFloor p g then Just p else Nothing
