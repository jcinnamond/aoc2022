module CPU (eval, parseOp, evalOp, Instruction (..)) where

import Data.List (isPrefixOf)

data Instruction
  = Noop
  | AddX Int
  deriving (Show, Eq)

parseOp :: String -> Instruction
parseOp s
  | s == "noop" = Noop
  | "addx " `isPrefixOf` s = AddX (read $ last $ words s)
  | otherwise = error $ "unrecognised instruction " <> s

eval :: [Instruction] -> [Int]
eval instructions = 1 : go 1 instructions
  where
    go _ [] = []
    go x (i : is) = let (x', xs) = evalOp x i in xs <> go x' is

evalOp :: Int -> Instruction -> (Int, [Int])
evalOp x Noop = (x, [x])
evalOp x (AddX x') = (x + x', [x, x + x'])
