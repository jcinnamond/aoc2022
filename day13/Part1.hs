module Part1 (solve) where

import Control.Applicative ((<|>))
import Data.List (elemIndices)
import Data.Maybe (fromJust)
import Packets (List (..), parseInput)

solve :: String -> String
solve = show . sum . map succ . elemIndices True . map correctOrder . parseInput

correctOrder :: (List, List) -> Bool
correctOrder = fromJust . go
  where
    go :: (List, List) -> Maybe Bool
    go (Number l, Number r)
      | l < r = Just True
      | l > r = Just False
      | l == r = Nothing
    go (List [], List (_ : _)) = Just True
    go (List (_ : _), List []) = Just False
    go (l@(Number _), r@(List _)) = go (List [l], r)
    go (l@(List _), r@(Number _)) = go (l, List [r])
    go (List (l : ls), List (r : rs)) = go (l, r) <|> go (List ls, List rs)
    go (List [], List []) = Nothing
    go p = error $ "cannot compare: " <> show p