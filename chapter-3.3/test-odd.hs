{-# OPTIONS_GHC -Wempty-enumerations #-}

module OddDemo where

-- Пусть задан тип `Odd` нечетных чисел следующим образом
data Odd = Odd Integer deriving (Eq, Show)

-- Сделайте этот тип представителем класса типов `Enum`

-- Вот тут надо было бы явно перечислить методы, реализацию которых надо переопределить.
-- Явно заявить, что поведение должно совпадать с Enum для Int.
-- Играть в угадайку по результатам тестов очень неинтересно.
-- Особенно, когда сообщения об ошибках не дают никакой информации, кроме номера теста.

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
  enumFrom x = x : xs where xs = enumFrom $ succ x -- stream
  enumFromTo (Odd start) (Odd stop) = map Odd [start, start + 2 .. stop] -- list
  enumFromThen (Odd start) (Odd second) = map Odd [start, second ..] -- stream-with-custom-step
  enumFromThenTo x0@(Odd start) x1@(Odd second) (Odd stop) = map Odd [start, second .. stop] -- list-with-custom-step

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
        isEqual [99, 91 .. 1] [Odd 99, Odd 91 .. Odd 1] "99, 91 .. 1",
        -- corner cases
        isEqual [1, 1 .. ] [Odd 1, Odd 1 .. ] "1, 1 .. ",
        isEqual [1 .. 1] [Odd 1 .. Odd 1] "1 .. 1",
        isEqual [1, 1 .. 1] [Odd 1, Odd 1 .. Odd 1] "1, 1 .. 1",
        -- invalid three points
        isEqual [1, 5 .. 9] [Odd 1, Odd 5 .. Odd 9] "1, 5 .. 9", -- good
        isEqual [9, 5 .. 1] [Odd 9, Odd 5 .. Odd 1] "9, 5 .. 1",
        isEqual [1, 9 .. 5] [Odd 1, Odd 9 .. Odd 5] "1, 9 .. 5", -- bad
        isEqual [5, 1 .. 9] [Odd 5, Odd 1 .. Odd 9] "5, 1 .. 9",
        isEqual [5, 9 .. 1] [Odd 5, Odd 9 .. Odd 1] "5, 9 .. 1",
        isEqual [9, 1 .. 5] [Odd 9, Odd 1 .. Odd 5] "9, 1 .. 5"
        -- negatives (WIP)
    ]
    where
        isEqual expected actual msg = (take 99 expected == fromOdd (take 99 actual)) || error (
            "failed: " ++ msg ++ "; got " ++ show (take 99 actual) ++ "; expected " ++ show (take 99 expected)
            )
        fromOdd [] = []
        fromOdd ((Odd x):xs) = x : fromOdd xs

{-- 
over-engineering, it's a habit
Wrong solution

  enumFromTo first@(Odd lower) (Odd upper) = takeWhile isInBounds stream
    where
      stream = enumFrom first
      isInBounds (Odd x)
        | dist == 0 = x == upper
        | otherwise = x <= upper
      dist = upper - lower

  enumFromThen x0@(Odd first) x1@(Odd second) = x0 : stream -- stream-with-custom-step
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
--}

{--
reference with `takeWhile`

instance Enum Odd where
    succ (Odd x) = Odd $ x + 2
    pred (Odd x) = Odd $ x - 2
    
    toEnum x = Odd $ toInteger x

    fromEnum (Odd x) = fromEnum x

    enumFrom x = x : enumFrom (succ x)

    enumFromThen ox@(Odd x) oy@(Odd y) =
        ox : enumFromThen oy (Odd $ y + (y - x))

    enumFromTo ox@(Odd x) oy@(Odd y)
        | x > y = []
        | x == y = [oy]
        | otherwise = ox : enumFromTo (succ ox) oy

    enumFromThenTo ox@(Odd x) oy@(Odd y) oz@(Odd z)
        | y > x =
            takeWhile (\(Odd v) -> v <= z) $ enumFromThen ox oy
        | y < x =
            takeWhile (\(Odd v) -> v >= z) $ enumFromThen ox oy
        | otherwise = takeWhile (\(Odd v) -> v <= z) $ enumFromThen ox oy
--}

{--
the best

instance Enum Odd where
  succ (Odd a) = Odd $ a + 2
  pred (Odd a) = Odd $ a - 2
  toEnum x = Odd $ toInteger x
  fromEnum (Odd a) = fromEnum a
  enumFrom a = [a,succ a..]
  enumFromTo a b = [a,succ a..b]
  enumFromThen (Odd a) (Odd b) = map Odd [a, b..]
  enumFromThenTo (Odd a) (Odd b) (Odd c) = map Odd [a, b..c]
--}