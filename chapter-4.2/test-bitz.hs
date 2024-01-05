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

дополнения к ТЗ

> Можете считать, что ведущих нулей нет, а 0 определяется как `Z Plus []`
это надо понимать так: `[0]` не будет, вместо него `[]`

> Вы избавились от ведущих нулей? Их не только нет на входе, но и не должно быть на выходе

> Можно было решать через fold/unfold, или предполагалось написать честные умножение/сложение? 
Предполагалось, что можно и так, и так

--}

-- original setup:
-- data Bit = Zero | One
-- data Sign = Minus | Plus
-- data Z = Z Sign [Bit]

data Bit = Zero | One deriving (Show, Eq)
data Sign = Minus | Plus deriving (Show, Eq)
data Z = Z Sign [Bit] deriving (Show, Eq)

{-- test:
0 + 0 = []
0 - 0 = []
1 + 0 = [1]
1 - 0 = [1]
0 + 1 = [1]
0 - 1 = -[1]
-5 + 16 = [1101]
-10 + 5 = [101]
1 * 0 = []
-20 + 5 = -[1111]

ghci> add (Z Minus [Zero, Zero, One, Zero, One]) (Z Plus [One, Zero, One])
Z Minus [One,One,One,One]

ghci> add (Z Plus [One]) (Z Plus [Zero, One])
Z Plus [One,One]
ghci> add (Z Plus [Zero, One]) (Z Plus [Zero, One])
Z Plus [Zero,Zero,One]
ghci> add (Z Minus [One]) (Z Plus [Zero, One])
Z Plus [One]
--}

-- решение

-- начнем с добавления чисел
-- add :: Z -> Z -> Z
-- add = undefined
-- на входе тип-произведение из типов Sign (сумма из двух вариантов) и списка Bit (сумма из двух вариантов)
-- попытаемся реализовать требуемое через кодек в целые числа и банальные операции с целыми числами

add :: Z -> Z -> Z
add = doOp (+)

mul :: Z -> Z -> Z
mul = doOp (*)

doOp :: (Int -> Int -> Int) -> Z -> Z -> Z
doOp op a b = encode (decode a `op` decode b)

decode :: Z -> Int
decode (Z _ []) = 0
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
encodeUnsigned 0 = []
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

-- tests

checkSum :: Int -> Int -> Bool
checkSum a b = a + b == decode (add (encode a) (encode b))

checkMul :: Int -> Int -> Bool
checkMul a b = a * b == decode (mul (encode a) (encode b))
{--
repl
and [checkMul x y | x <- [-100..100], y <- [-100..100]]
and [checkSum x y | x <- [-100..100], y <- [-100..100]]
--}

emptyZ  = Z Plus []

testList = [
    test001, test002, test003, test011, test012, test013, test021, test022,
    test023, test031, test032, test033, test041, test042, test043, test051,
    test052, test053, test054, test055, test056, test057, test058,test101,
    test102, test103, test104, test105, test111, test112, test113, test114,
    test121, test122, test131
    ]

testIndices = [1,2,3,11,12,13,21,22,23,31,32,33,41,42,43,51,52,53,54,55,56,57,58,101,102,103,104,105,111,112,113,114,121,122,131]

allTests = zip testIndices testList

-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul
