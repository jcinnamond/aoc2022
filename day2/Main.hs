module Main where

import qualified Shape as S
import Shape(Shape(..))
import qualified Outcome as O
import Outcome(Outcome(..))

parseLine :: String -> (Shape, Shape)
parseLine s = (S.parse $ head s, S.parse $ last s)

parseInput :: String -> [(Shape, Shape)]
parseInput = map parseLine . lines

outcome :: (Shape, Shape) -> Outcome
outcome (Rock, Scissors) = Loss
outcome (Rock, Paper) = Win
outcome (Scissors, Paper) = Loss
outcome (Scissors, Rock) = Win
outcome (Paper, Rock) = Loss
outcome (Paper, Scissors) = Win
outcome _ = Draw

score :: (Shape, Shape) -> Int
score r@(_, self) = O.score (outcome r) + S.score self

solvePart1 :: String -> String
solvePart1 = show . sum . map score . parseInput


parsePredictiveLine :: String -> (Shape, Outcome)
parsePredictiveLine s = (S.parse $ head s, O.parse $ last s)

parsePredictiveInput :: String -> [(Shape, Outcome)]
parsePredictiveInput = map parsePredictiveLine . lines

fromOutcome :: (Shape, Outcome) -> Shape
fromOutcome (Rock, Win) = Paper
fromOutcome (Rock, Loss) = Scissors
fromOutcome (Paper, Win) = Scissors
fromOutcome (Paper, Loss) = Rock
fromOutcome (Scissors, Win) = Rock
fromOutcome (Scissors, Loss) = Paper
fromOutcome (x, Draw) = x

predictScore :: (Shape, Outcome) -> Int
predictScore r@(_, o) = O.score o + S.score (fromOutcome r)
         
solvePart2 :: String -> String
solvePart2 = show . sum . map predictScore . parsePredictiveInput

solveParts :: String -> String
solveParts s = solvePart1 s <> "\n" <> solvePart2 s

main :: IO ()
main = interact solveParts