module Priorities (priority) where

import Data.Char (ord)

priority :: Char -> Int
priority c
  | c > 'a' = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27