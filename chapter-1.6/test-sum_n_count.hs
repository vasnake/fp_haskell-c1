module SumAndCount where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = helper (abs x) 0 0 -- x, sum, count
    where
        helper 0 sum count = (sum, count)
        helper num sum count = helper
            (num `div` 10) -- 9 // 10 = 0; 10 // 10 = 1; 19 // 10 = 1
            (sum + (num `mod` 10)) -- 9 % 10 = 9; 10 % 10 = 0; 19 % 10 = 9
            (count + 1)
