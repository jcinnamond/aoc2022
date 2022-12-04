module Parse (parse) where

import Data.Bifunctor (Bifunctor (bimap))

parse :: String -> ((Int, Int), (Int, Int))
parse s = bimap parseRange (parseRange . tail) (break (== ',') s)

parseRange :: String -> (Int, Int)
parseRange s = bimap read (read . tail) (break (== '-') s)
