module Bitz where
{--
Целое число можно представить как список битов со знаком
Реализуйте функции сложения и умножения для таких целых чисел
считая, что младшие биты идут в начале списка.
Можно считать, что на вход не будут подаваться числа с ведущими нулями

реализуй это:

add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined
--}

-- original setup:
-- data Bit = Zero | One
-- data Sign = Minus | Plus
-- data Z = Z Sign [Bit]

data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

{-- test:
ghci> add (Z Plus [One]) (Z Plus [Zero, One])
Z Plus [One,One]
ghci> add (Z Plus [Zero, One]) (Z Plus [Zero, One])
Z Plus [Zero,Zero,One]
ghci> add (Z Minus [One]) (Z Plus [Zero, One])
Z Plus [One]
--}

-- решение

-- разбор: начнем с добавления чисел
-- add :: Z -> Z -> Z
-- add = undefined
-- на входе тип-произведение из типов Sign (сумма из двух вариантов) и списка Bit (сумма из двух вариантов)
-- попытаемся реализовать требуемое через кодек в целые числа и банальные операции с целыми числами

add :: Z -> Z -> Z
add = doOp (+) -- encode (decode a + decode b)

mul :: Z -> Z -> Z
mul = doOp (*) -- encode (decode a * decode b)

doOp :: (Int -> Int -> Int) -> Z -> Z -> Z
doOp _ (Z _ []) x = x
doOp _ x (Z _ []) = x
doOp op a b = encode (decode a `op` decode b)

decode :: Z -> Int
decode (Z _ []) = error "decode: have no bits"
decode (Z Minus bits) = (-1) * decodeUnsigned bits
decode (Z Plus bits)  = decodeUnsigned bits

encode :: Int -> Z
encode x = Z sign bits where
    sign = if x < 0 then Minus else Plus
    bits = encodeUnsigned . abs $ x

decodeUnsigned :: [Bit] -> Int
decodeUnsigned bits = sum bitsValues where
    bitsValues = zipWith pow bits positions
    positions = [0 ..]
    pow Zero idx    = 0
    pow One  idx    = 2 ^ idx

encodeUnsigned :: Int -> [Bit]
encodeUnsigned 0 = [Zero]
encodeUnsigned n = loop n where
    loop 0 = []
    loop x = bitFromInt (mod x 2) : loop (div x 2) where
        bitFromInt b = if b > 0 then One else Zero

-- reference

-- https://stackoverflow.com/questions/54378081/haskell-decimal-to-binary
fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = go n
    where go 0 = []
          go k = mod k 2 : go (div k 2)
