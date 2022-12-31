module Cave (prettyPrint, parse, isAir, isAirWithFloor, onGrid, startPos, Cave (..), Pos (..)) where

import Control.Arrow ((***))
import Data.Set (Set)
import qualified Data.Set as Set

newtype Pos = Pos {unPos :: (Int, Int)} deriving stock (Show, Eq, Ord)

startPos :: Pos
startPos = Pos (500, 0)

data Cave = Cave
  { xMin :: Int,
    xMax :: Int,
    yMax :: Int,
    rocks :: Set Pos,
    sand :: Set Pos
  }
  deriving stock (Show)

line :: Pos -> Pos -> [Pos]
line (Pos (x, y)) (Pos (x', y'))
  | y == y' = Pos <$> zip [min x x' .. max x x'] (repeat y)
  | otherwise = Pos <$> zip (repeat x) [min y y' .. max y y']

pairs :: [a] -> [(a, a)]
pairs (x : y : zs) = (x, y) : pairs (y : zs)
pairs _ = []

parse :: String -> Cave
parse = grid . concatMap parseRocks . lines
  where
    grid :: [Pos] -> Cave
    grid ps = Cave xmin xmax ymax (Set.fromList ps) Set.empty
      where
        xmin = minimum xs
        xmax = maximum xs
        ymax = maximum ys
        xs = map (fst . unPos) ps
        ys = map (snd . unPos) ps

    parseRocks :: String -> [Pos]
    parseRocks s = concatMap (uncurry line) $ (pairs . parsePaths) s

    parsePaths :: String -> [Pos]
    parsePaths = map parsePos . filter (/= "->") . words

    parsePos :: String -> Pos
    parsePos = Pos . (read *** read . tail) . break (== ',')

prettyPrint :: Cave -> String
prettyPrint g = showHeader ++ go 0
  where
    showHeader = showHeaderLine 0 ++ showHeaderLine 1 ++ showHeaderLine 2
    showHeaderLine y = "   " ++ map (showHeaderPoint y) xRange ++ "\n"
    showHeaderPoint y x
      | x == xMin g || x == xMax g = show x !! y
      | x == 500 = "500" !! y
      | otherwise = ' '

    go y
      | y > yMax g + 1 = ""
      | otherwise = showRow y ++ go (y + 1)

    showRow y = show y ++ " " ++ (showPoint <$> zip xRange (repeat y)) ++ "\n"
    showPoint p
      | isStart p = '+'
      | isRock p = '#'
      | isSand p = 'o'
      | otherwise = '.'

    xRange = [minx .. maxx]
    minx = min (xMin g) sandMin
    sandMin = maybe 0 (fst . unPos) (Set.lookupMin $ sand g)
    maxx = max (xMax g) sandMax
    sandMax = maybe 999 (fst . unPos) (Set.lookupMax $ sand g)

    isRock p = Pos p `elem` rocks g
    isSand p = Pos p `elem` sand g

    isStart p = Pos p == startPos

isAir :: Pos -> Cave -> Bool
isAir p g = not (isRock p) && not (isSand p)
  where
    isRock p' = Set.member p' $ rocks g
    isSand p' = Set.member p' $ sand g

isAirWithFloor :: Pos -> Cave -> Bool
isAirWithFloor p g = not (onFloor p) && isAir p g
  where
    onFloor (Pos (_, y)) = y == yMax g + 2

onGrid :: Pos -> Cave -> Bool
onGrid (Pos (_, y)) g = y < yMax g
