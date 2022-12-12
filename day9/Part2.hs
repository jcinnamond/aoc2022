module Part2 (solve) where

import qualified Data.Set as S
import Rope (Movements (M), Pos, follow, moveKnot, parse)

solve :: String -> String
solve = show . length . snd . foldl eval (replicate 10 (0, 0), S.empty) . map parse . lines

type Rope = [Pos]

type Visited = S.Set Pos

eval :: (Rope, Visited) -> Movements -> (Rope, Visited)
eval r (M _ 0) = r
eval (r, vs) m@(M ms v) =
  let r' = move r m
   in eval (r', S.insert (last r') vs) (M ms (v - 1))

move :: Rope -> Movements -> Rope
move (h : ks) (M ms _) =
  let h' = moveKnot h ms
   in h' : moveRest h' ks
move _ _ = error "unexpected empty rope"

moveRest :: Pos -> Rope -> Rope
moveRest _ [] = []
moveRest h (k : ks) =
  let k' = moveKnot k (h `follow` k)
   in k' : moveRest k' ks