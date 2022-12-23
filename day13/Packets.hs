module Packets (parseInput, parseAllInputs, List (..)) where

import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, many, optional, parse, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, eol)

data List = List [List] | Number Int deriving (Show, Eq)

type Parser = Parsec Void String

parseInput :: String -> [(List, List)]
parseInput = fromRight [] . parse parsePairs ""

parseAllInputs :: String -> [List]
parseAllInputs = fromRight [] . parse (many (parseList <* many eol)) ""

parseNumber :: Parser List
parseNumber = Number . read <$> some digitChar

parseList :: Parser List
parseList = List <$> between (char '[') (char ']') parseListContents

parseListContents :: Parser [List]
parseListContents = (parseList <|> parseNumber) `sepBy` char ','

parsePair :: Parser (List, List)
parsePair = (,) <$> parseList <* eol <*> parseList <* optional eol

parsePairs :: Parser [(List, List)]
parsePairs = parsePair `sepBy` eol <* eof
