module OddsOnly where

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
    | odd x = x : oddsOnly xs
    | otherwise = oddsOnly xs
