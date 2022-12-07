module Parse (parse, DirSizes) where

import Control.Arrow ((***))
import Data.List (isPrefixOf, tails)
import qualified Data.Map.Strict as Map

parse :: String -> DirSizes
parse = eval [] Map.empty . map parseLine . lines

data Line
  = Cd String
  | Back
  | Ls
  | FileListing Int String
  | DirListing String
  deriving (Show)

type DirSizes = Map.Map [String] Int

parseLine :: String -> Line
parseLine s
  | "$ ls" == s = Ls
  | "$ cd .." == s = Back
  | "$ cd" `isPrefixOf` s = Cd (drop 5 s)
  | "dir" `isPrefixOf` s = DirListing (drop 4 s)
  | otherwise = uncurry FileListing $ (read *** tail) $ break (== ' ') s

eval :: [String] -> DirSizes -> [Line] -> DirSizes
eval _ m [] = m
eval path m (Back : rest) = eval (tail path) m rest
eval path m (Cd p : rest) = eval (p : path) m rest
eval path m (Ls : rest) = let (m', rest') = parseListing rest path m in eval path m' rest'
eval _ _ _ = error "unexpected input"

parseListing :: [Line] -> [String] -> DirSizes -> (DirSizes, [Line])
parseListing (DirListing _ : rest) path m = parseListing rest path m
parseListing (FileListing size _ : rest) path m = parseListing rest path (addFile path size m)
parseListing rest _ m = (m, rest)

addFile :: [String] -> Int -> DirSizes -> DirSizes
addFile path s ds = foldl insert ds (tails path)
  where
    insert :: DirSizes -> [String] -> DirSizes
    insert ds' k = Map.insertWith (+) k s ds'
