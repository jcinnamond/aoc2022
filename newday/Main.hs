module Main (main) where

import qualified Data.Text as T
import Data.Time (DayOfMonth, UTCTime (utctDay), getCurrentTime, toGregorian)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO (readFile')

main :: IO ()
main = do
  day <- getDay
  let dir = "day" <> show day
  existing <- doesDirectoryExist dir
  if existing then putStrLn (dir <> " already set up") else setUpNewDay dir

setUpNewDay :: FilePath -> IO ()
setUpNewDay p = do
  createDirectory p
  createTemplateFiles p
  addCabalEntry p
  putStrLn $ "set up " <> p

createTemplateFiles :: FilePath -> IO ()
createTemplateFiles p = do
  readFile' "templates/Main" >>= writeFile (p <> "/Main.hs")
  readFile' "templates/Part1" >>= writeFile (p <> "/Part1.hs")
  readFile' "templates/Part2" >>= writeFile (p <> "/Part2.hs")

addCabalEntry :: FilePath -> IO ()
addCabalEntry p = do
  template <- readFile' "templates/cabal"
  let entry = T.replace "DAY" (T.pack p) (T.pack template)
  appendFile "aoc2022.cabal" (T.unpack entry)

getDay :: IO DayOfMonth
getDay = do
  args <- getArgs
  case args of
    [] -> currentDay
    (x : _) -> pure $ read x

currentDay :: IO DayOfMonth
currentDay = do
  t <- utctDay <$> getCurrentTime
  let (_, _, day) = toGregorian t
  pure day
