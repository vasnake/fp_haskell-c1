module Demo where

fibonacci :: Integer -> Integer
fibonacci (-2)          = (-1)
fibonacci (-1)          = 1
fibonacci 0             = 0
fibonacci 1             = 1
fibonacci n | n > 0     = fibonacci (n - 1) + fibonacci (n - 2)
            | otherwise = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' :: Integer -> Integer
fibonacci' = fib 0 1 -- fib(0), fib(1), n -- Eta reduced

fib :: Integer -> Integer -> Integer -> Integer
fib a b n | n == 0  = a
          | n > 0   = fib b (a + b) (n - 1)
          | n < 0   = fib (b - a) a (n + 1)
