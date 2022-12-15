module Monkey (parseMonkeys, Monkey (..), Test (..), ToMonkey (..)) where

import Data.Char (isDigit)

type Operation = (Integer -> Integer)

newtype ToMonkey = ToMonkey Int deriving (Show, Eq)

newtype Test = Test {getTest :: Int} deriving (Show, Eq)

newtype Interests = Interests Int deriving (Show, Eq)

data Monkey = Monkey [Integer] Operation Test (ToMonkey, ToMonkey) Int

instance Show Monkey where
  show (Monkey items op test to interests) =
    "Monkey: "
      <> show interests
      <> ", holding "
      <> show items
      <> ", Operation 2 -> "
      <> show (op 2)
      <> ", divisible by "
      <> show test
      <> ", : True -> "
      <> show (fst to)
      <> ", False -> "
      <> show (snd to)

parseMonkeys :: [String] -> [Monkey]
parseMonkeys [] = []
parseMonkeys ls = let (m, rest) = parseMonkey ls in m : parseMonkeys rest

parseMonkey :: [String] -> (Monkey, [String])
parseMonkey input =
  let (ls, rest) = splitAt 7 input
   in (monkify ls, rest)
  where
    monkify m = Monkey (startingItems m) (operation m) (test m) (outputs m) 0
    startingItems = map (read . takeWhile isDigit) . drop 2 . words . (!! 1)
    operation = parseOperation . (!! 2)
    test = Test . finalNum 3
    outputs m' = (ToMonkey $ finalNum 4 m', ToMonkey $ finalNum 5 m')
    finalNum :: Int -> [String] -> Int
    finalNum x = read . last . words . (!! x)

parseOperation :: String -> Operation
parseOperation s = toFunc $ drop 4 $ words s
  where
    toFunc ["*", "old"] = \x -> x * x
    toFunc ["*", v] = \x -> x * read v
    toFunc ["+", "old"] = \x -> x + x
    toFunc ["+", v] = \x -> x + read v
    toFunc _ = error "unrecognised operation"
