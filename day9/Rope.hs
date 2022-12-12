module Rope (parse, moveKnot, follow, Pos, Movements (..)) where

type Pos = (Int, Int)

type Movement = Int -> Int

data Movements = M (Movement, Movement) Int

parse :: String -> Movements
parse ('L' : ' ' : v) = M (pred, id) $ read v
parse ('R' : ' ' : v) = M (succ, id) $ read v
parse ('U' : ' ' : v) = M (id, succ) $ read v
parse ('D' : ' ' : v) = M (id, pred) $ read v
parse x = error $ "unexpected instruction: " <> x

moveKnot :: Pos -> (Movement, Movement) -> Pos
moveKnot (x, y) (mx, my) = (mx x, my y)

follow :: Pos -> Pos -> (Movement, Movement)
follow h t
  | h `touching` t = (id, id)
  | otherwise = (moveX, moveY)
  where
    moveX :: Movement
    moveX
      | h `leftOf` t = pred
      | h `rightOf` t = succ
      | otherwise = id
    moveY :: Movement
    moveY
      | h `above` t = succ
      | h `below` t = pred
      | otherwise = id

leftOf :: Pos -> Pos -> Bool
leftOf (x, _) (x', _) = x < x'

rightOf :: Pos -> Pos -> Bool
rightOf (x, _) (x', _) = x > x'

above :: Pos -> Pos -> Bool
above (_, y) (_, y') = y > y'

below :: Pos -> Pos -> Bool
below (_, y) (_, y') = y < y'

touching :: Pos -> Pos -> Bool
touching (x, y) (x', y') = abs (x - x') <= 1 && abs (y - y') <= 1