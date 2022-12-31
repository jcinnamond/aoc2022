module Part1 (solve) where

import Cave (Cave (..), Pos (..), isAir, onGrid, parse, startPos)
import Control.Applicative ((<|>))
import qualified Data.Set as Set

solve :: String -> String
solve = show . length . sand . trickle . parse

trickle :: Cave -> Cave
trickle g = case dropSand startPos of
  Just p -> trickle $ g {sand = Set.insert p $ sand g}
  Nothing -> g
  where
    dropSand p = case fallDown p <|> fallLeft p <|> fallRight p of
      Just p' ->
        if onGrid p' g
          then dropSand p'
          else Nothing
      Nothing -> Just p

    fallDown (Pos (x, y)) = fall $ Pos (x, y + 1)
    fallLeft (Pos (x, y)) = fall $ Pos (x - 1, y + 1)
    fallRight (Pos (x, y)) = fall $ Pos (x + 1, y + 1)

    fall p = if isAir p g then Just p else Nothing
