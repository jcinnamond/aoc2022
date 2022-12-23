module Part2 (solve) where

import Control.Applicative ((<|>))
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Packets (List (..), parseAllInputs)

solve :: String -> String
solve = show . product . findDividers . sortBy sortPackets . (<> dividers) . parseAllInputs

findDividers :: [List] -> [Int]
findDividers l = map (succ . fromJust) [elemIndex divider1 l, elemIndex divider2 l]

dividers :: [List]
dividers = [divider1, divider2]

divider1 :: List
divider1 = List [List [Number 2]]

divider2 :: List
divider2 = List [List [Number 6]]

sortPackets :: List -> List -> Ordering
sortPackets p1 p2 = fromJust $ go p1 p2
  where
    go :: List -> List -> Maybe Ordering
    go (Number l) (Number r)
      | l < r = Just LT
      | l > r = Just GT
      | l == r = Nothing
    go (List []) (List (_ : _)) = Just LT
    go (List (_ : _)) (List []) = Just GT
    go l@(Number _) r@(List _) = go (List [l]) r
    go l@(List _) r@(Number _) = go l (List [r])
    go (List (l : ls)) (List (r : rs)) = go l r <|> go (List ls) (List rs)
    go (List []) (List []) = Nothing
    go l r = error $ "cannot compare: " <> show l <> " " <> show r