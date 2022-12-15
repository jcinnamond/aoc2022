module Game (runTurns, Relief (..)) where

import Data.List (sort)
import GHC.Integer (divInteger, modInteger)
import Monkey (Monkey (..), Test (..), ToMonkey (..))
import Prelude hiding (round)

newtype Relief = Relief {getRelief :: Int}

runTurns :: Relief -> Int -> [Monkey] -> Int
runTurns r t = product . take 2 . reverse . sort . map activity . nTimes t (round r)

nTimes :: Int -> (a -> a) -> a -> a
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

activity :: Monkey -> Int
activity (Monkey _ _ _ _ i) = i

round :: Relief -> [Monkey] -> [Monkey]
round = go 0
  where
    go :: Int -> Relief -> [Monkey] -> [Monkey]
    go p r ms
      | p == length ms = ms
      | otherwise = go (p + 1) r (turn p r ms)

turn :: Int -> Relief -> [Monkey] -> [Monkey]
turn p r monkeys = go (monkeys !! p) monkeys
  where
    go :: Monkey -> [Monkey] -> [Monkey]
    go monkey@(Monkey [] _ _ _ _) ms = splice p monkey ms
    go (Monkey (i : is) op test outputs interests) ms =
      go
        (Monkey is op test outputs (interests + 1))
        (newMonkeys level (getTest test) outputs ms)
      where
        level = op i `divInteger` toInteger (getRelief r)

newMonkeys :: Integer -> Int -> (ToMonkey, ToMonkey) -> [Monkey] -> [Monkey]
newMonkeys level test outputs = updateMonkey level target
  where
    target = (if level `modInteger` toInteger test == 0 then fst else snd) outputs

updateMonkey :: Integer -> ToMonkey -> [Monkey] -> [Monkey]
updateMonkey level (ToMonkey target) ms = splice target (newM (ms !! target)) ms
  where
    newM m@(Monkey items _ _ _ _) = newMonkey m (items <> [level])

newMonkey :: Monkey -> [Integer] -> Monkey
newMonkey (Monkey _ op test outputs interests) levels = Monkey levels op test outputs interests

splice :: Int -> a -> [a] -> [a]
splice p x xs = take p xs <> [x] <> drop (p + 1) xs