{-# OPTIONS_GHC -Wempty-enumerations #-}

module OddDemo where

-- Пусть задан тип `Odd` нечетных чисел следующим образом
data Odd = Odd Integer deriving (Eq, Show)

-- Сделайте этот тип представителем класса типов `Enum`
{--
tests:

GHCi> succ $ Odd (-100000000000003)
Odd (-100000000000001)

succ (Odd (10^20 + 1)) == Odd (10^20 + 3)

GHCi> [Odd 11 .. Odd 21]
[Odd 11,Odd 13,Odd 15,Odd 17,Odd 19,Odd 21]

take 5 [Odd (10^20 + 7), Odd (10^20 + 11) .. Odd (10^20 + 17)]
ghci> take 5 [7, 11 .. 17]
[7,11,15]

take 5 [Odd (10^20 + 17), Odd (10^20 + 13) .. Odd (10^20 + 7)]
ghci> take 5 [17, 13 .. 7]
[17,13,9]

--}

-- Вот тут надо было явно перечислить методы, реализацию которых надо переопределить.
-- Играть в угадайку по результатам тестов очень неинтересно.

-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where -- типы указывать не надо, там конфликт Int vs Integer
  toEnum x
    | odd x = Odd $ toInteger x
    | otherwise = error ("Odd.toEnum: argument x must be an odd number, got " ++ show x)

  fromEnum (Odd x) = fromInteger x
  succ (Odd x) = Odd $ x + 2
  pred (Odd x) = Odd $ x - 2
  enumFrom x = x : xs where xs = enumFrom $ succ x

  -- enumFromTo (Odd n) (Odd m) = map Odd [n,n+2..m]
  enumFromTo first@(Odd lower) (Odd upper) = takeWhile isInBounds stream
    where
      stream = enumFrom first
      isInBounds (Odd x)
        | dist == 0 = x == upper
        | otherwise = x <= upper
      dist = upper - lower

  enumFromThen x0@(Odd first) x1@(Odd second) = x0 : stream
    where
      stream = enumFromThen x1 $ Odd (second + step)
      step = second - first

  enumFromThenTo x0@(Odd lower) x1@(Odd second) (Odd upper) = takeWhile isInBounds stream
    where
      stream = enumFromThen x0 x1
      isInBounds (Odd x)
        | second < lower || second > upper = False
        | dist == 0 = x == upper
        -- \| second - lower == 0                       = x <= upper
        -- \| signum dist /= signum (second - lower)    = False
        | otherwise = x <= upper
      dist = upper - lower
{--
failed: 9, 1 .. 5; got []; expected [9]
--}
testOdd =
    [
        -- pre
        isEqual [-100000000000001] [succ $ Odd (-100000000000003)] "succ -3",
        isEqual [100000000000000000003] [succ $ Odd (10^20 + 1)] "succ 1",
        isEqual [100000000000000000007, 100000000000000000011, 100000000000000000015] (take 5 [Odd (10^20 + 7), Odd (10^20 + 11) .. Odd (10^20 + 17)]) "",
        -- up
        isEqual (filter odd [1 ..]) [Odd 1 ..] "1 ..",
        isEqual [1, 5 .. ] [Odd 1, Odd 5 .. ] "1, 5 .. ",
        isEqual (filter odd [1 .. 3]) [Odd 1 .. Odd 3] "1 .. 3",
        isEqual (filter odd [1 .. 99]) [Odd 1 .. Odd 99] "1 .. 99",
        isEqual [1, 5 .. 99] [Odd 1, Odd 5 .. Odd 99] "1, 5 .. 99",
        -- down
        isEqual [] [Odd 99 .. Odd 1] "99 .. 1", -- default step always UP
        isEqual [99, 91 .. ] [Odd 99, Odd 91 .. ] "99, 91 .. ",
        -- isEqual [99, 91 .. 1] [Odd 99, Odd 91 .. Odd 1] "99, 91 .. 1",
        -- corner cases
        isEqual [1, 1 .. ] [Odd 1, Odd 1 .. ] "1, 1 .. ",
        isEqual [1 .. 1] [Odd 1 .. Odd 1] "1 .. 1",
        isEqual [1, 1 .. 1] [Odd 1, Odd 1 .. Odd 1] "1, 1 .. 1",
        -- invalid three points
        isEqual [1, 5 .. 9] [Odd 1, Odd 5 .. Odd 9] "1, 5 .. 9", -- good
        -- isEqual [9, 5 .. 1] [Odd 9, Odd 5 .. Odd 1] "9, 5 .. 1",
        -- isEqual [1, 9 .. 5] [Odd 1, Odd 9 .. Odd 5] "1, 9 .. 5", -- bad
        isEqual [5, 1 .. 9] [Odd 5, Odd 1 .. Odd 9] "5, 1 .. 9",
        isEqual [5, 9 .. 1] [Odd 5, Odd 9 .. Odd 1] "5, 9 .. 1",
        isEqual [9, 1 .. 5] [Odd 9, Odd 1 .. Odd 5] "9, 1 .. 5"
        -- negatives
    ]
    where
        isEqual expected actual msg = (take 99 expected == fromOdd (take 99 actual)) || error (
            "failed: " ++ msg ++ "; got " ++ show (take 99 actual) ++ "; expected " ++ show (take 99 expected)
            )
        fromOdd [] = []
        fromOdd ((Odd x):xs) = x : fromOdd xs

-- типы указывать не надо, там конфликт Int vs Integer
-- instance Enum Odd where
--   toEnum :: Int -> Odd
--   toEnum x
--     | odd x = Odd $ toInteger x
--     | otherwise = error ("Odd.toEnum: argument x must be an odd number, got " ++ show x)
--   fromEnum :: Odd -> Int
--   fromEnum (Odd x) = fromInteger x
--   succ :: Odd -> Odd
--   succ (Odd x) = Odd $ x + 2
--   pred :: Odd -> Odd
--   pred (Odd x) = Odd $ x - 2