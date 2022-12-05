module Parse (parse, Instruction(..)) where
import Control.Arrow ((***))
import Data.List.Extra (trim, dropEnd, chunksOf)
import Data.List (transpose)

data Instruction = Move Int Int Int deriving (Show)

parse :: String -> ([String], [Instruction])
parse s = (stacks *** (instructions . tail)) $ break (== "") $ lines s

stacks :: [String] -> [String]
stacks = map trim . transpose . dropEnd 1 . map (map (!! 1) . chunksOf 4)

instructions :: [String] -> [Instruction]
instructions = map instruction

instruction :: String -> Instruction
instruction s = Move (read $ ws !! 1) (read $ ws !! 3) (read $ ws !! 5)
    where 
        ws = words s

