module Avg where

avg :: Int -> Int -> Int -> Double
avg a b c = (d a + d b + d c) / 3.0 where
    d :: Int -> Double
    d = fromIntegral
