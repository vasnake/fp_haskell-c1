module FibStream where
{--
Реализуйте c использованием функции `zipWith`
функцию `fibStream`
возвращающую бесконечный список чисел Фибоначчи

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]

ghci> :t zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

если вы возьмете лист с числами Фиббоначи и сложите его с тем же листом, у которого выкинута голова, вы получите снова лист чисел Фиббоначи
--}
fibStream :: [Integer]
fibStream = undefined

fibonacci :: Integer -> Integer
fibonacci (-2)          = (-1)
fibonacci (-1)          = 1
fibonacci 0             = 0
fibonacci 1             = 1
fibonacci n | n > 0     = fibonacci (n - 1) + fibonacci (n - 2)
            | otherwise = fibonacci (n + 2) - fibonacci (n + 1)

fibonacciAcc :: Integer -> Integer
fibonacciAcc = fib 0 1 -- fib(0), fib(1), n -- Eta reduced
fib :: Integer -> Integer -> Integer -> Integer
fib a b n | n == 0  = a
          | n > 0   = fib b (a + b) (n - 1)
          | n < 0   = fib (b - a) a (n + 1)

nats n = n : nats (n + 1)
