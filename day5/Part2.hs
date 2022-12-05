module Part2 (solve) where

import Parse (Instruction (..), parse)

solve :: String -> String
solve s = let (ss, is) = parse s in
    map head $ foldl execute ss is

execute :: [String] -> Instruction  -> [String]
execute ss (Move c f t) = splice t newt $ splice f newf ss
    where
        lifted = splitAt c (ss !! (f - 1))
        newf = snd lifted
        newt = fst lifted <> (ss !! (t - 1))

        splice :: Int -> String -> [String] -> [String] 
        splice n x xs = take (n - 1) xs ++ [x] ++ drop n xs
