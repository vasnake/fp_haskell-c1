module SeqA where

seqA :: Integer -> Integer
seqA n
    | n < 0 = error "arg must be >= 0"
    | otherwise = let
        loop :: Integer -> Integer -> Integer -> Integer -> Integer
        loop 0 a0 a1 a2 = a0
        loop 1 a0 a1 a2 = a1
        loop 2 a0 a1 a2 = a2
        loop i a0 a1 a2 = let
                a3 = a2 + a1 - 2 * a0
            in loop (i - 1) a1 a2 a3
        in loop n 1 2 3 -- term-check, a0, a1, a2
