module Part1 (solve) where

import qualified Data.Set as S
import Rope (Movements (M), Pos, follow, moveKnot, parse)

solve :: String -> String
solve = show . length . snd . foldl eval (((0, 0), (0, 0)), S.empty) . map parse . lines

type Rope = (Pos, Pos)

type Visited = S.Set Pos

eval :: (Rope, Visited) -> Movements -> (Rope, Visited)
eval x (M _ 0) = x
eval (r, vs) m@(M ms v) = let r' = move r m in eval (r', S.insert (snd r') vs) (M ms (v - 1))

move :: Rope -> Movements -> Rope
move (h, t) (M ms _) =
  let h' = moveKnot h ms
      t' = moveKnot t (h' `follow` t)
   in (h', t')
