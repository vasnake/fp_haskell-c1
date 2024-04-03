# FP Haskell, chapter 3, lists

[Функциональное программирование на языке Haskell / Денис Москвин / stepik](https://stepik.org/course/75/syllabus?next=)

## chapter 3.1, Функции для работы со списками

https://stepik.org/lesson/8326/step/1?unit=1474

### 3.1.2 базовые операции над списком

Базовый контейнер, рекурсивное определение данных.

Конкатенация, добавление в голову, ... Базовые способы: пустой список, добавить элем. в голову.
```hs
-- конструктор пустого списка
[]
ghci> :i []
type [] :: * -> *
data [] a = [] | a : [a] -- всего два конструктора

-- добавление элемента в голову, оператор приклеивания головы
3 : []
5 : 3 : []
ghci> :i (:)
type [] :: * -> *
data [] a = ... | a : [a] -- один из конструкторов списка `a : [a]`, два параметра
infixr 5 : -- право-ассоциативный инфиксный оператор со средним приоритетом
-- (5 : (3 : [])) -- право-ассоциативный бинарный оператор

-- скобки и запятые
[5, 3] -- это сахар для конструктора (рекурсивного)
5 : 3 : []

-- пример использования конструктора списка в кастомных функциях
ghci> let cons42 = (42 :) -- получили ф. одного аргумента, используя левый срез оператора `:`
ghci> :t cons42
cons42 :: Num a => [a] -> [a] -- ф. из списка чисел в список чисел
ghci> cons42 [1,2,3]
[42,1,2,3]
```
repl

```hs
-- Реализуйте функцию `addTwoElements`
-- которая бы добавляла два переданных ей значения в голову переданного списка
GHCi> addTwoElements 2 12 [85,0,6]
[2,12,85,0,6]

addTwoElements :: a -> a -> [a] -> [a]

```
test

```hs
-- Реализуйте функцию `nTimes`
-- которая возвращает список, состоящий из повторяющихся значений ее первого аргумента
-- Количество повторов определяется значением второго аргумента этой функции

GHCi> nTimes 42 3
[42,42,42]
GHCi> nTimes 'z' 5
"zzzzz"
nTimes 'z' 0 = ""
nTimes 'z' (-1) = ""

nTimes:: a -> Int -> [a]

```
test [ntimes](./chapter-3.1/test-ntimes.hs)

### 3.1.5 де-конструкторы head, tail

```hs
-- не тотальная ф. [a] -> a
ghci> :t head
head :: GHC.Stack.Types.HasCallStack => [a] -> a
ghci> head [1,2,3]
1

-- тоже, не тотальная ф. [a] -> [a]
ghci> :t tail
tail :: GHC.Stack.Types.HasCallStack => [a] -> [a]
ghci> tail [1,2,3]
[2,3]

-- разобрали и собрали, получилось то же самое
ghci> (\lst -> head lst : tail lst) [1,2,3]
[1,2,3]

-- corner cases
ghci> head [1]
1

ghci> head []
*** Exception: Prelude.head: empty list
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/List.hs:1646:3 in base:GHC.List
  errorEmptyList, called at libraries/base/GHC/List.hs:85:11 in base:GHC.List
  badHead, called at libraries/base/GHC/List.hs:81:28 in base:GHC.List
  head, called at <interactive>:27:1 in interactive:Ghci9

ghci> tail [1]
[]

ghci> tail []
*** Exception: Prelude.tail: empty list
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/List.hs:1646:3 in base:GHC.List
  errorEmptyList, called at libraries/base/GHC/List.hs:128:28 in base:GHC.List
  tail, called at <interactive>:29:1 in interactive:Ghci10

-- examples
second :: [a] -> a
second = head . tail -- poinfree композиция двух ф.; eq to `second xs = head (tail xs)`

-- pattern matching
-- конструктор данных используется в качестве образца (паттерна)
let head ((:) x xs) = x
let head (x : _) = x
let tail (_ : xs) = xs

second (_ : xs) = head xs
second (_ : x : _) = x
```
repl

```hs
-- Исследуйте тип функции
sndHead = snd . head
-- разберитесь, каково ее поведение

-- Эту функцию можно реализовать, используя сопоставление с образцом
sndHead некоторый_образец = x
-- Отметьте те образцы, которые подходят для этой цели.

ghci> :t sndHead -- sndHead = snd . head
sndHead :: [(a, c)] -> c
-- композиция двух ф. первая возвращает голову списка (x from x:xs)
-- вторая возвращает второй элемент пары ((a, b) -> b)
-- следовательно, тут в pointfree стиле записана ф. которая возвращает второй элемент первой пары из списка пар.

-- sndHead некоторый_образец = x -- что сюда подходит? нужен второй элемент первой пары из списка пар
((,) x y : z) -- нет, возвращаемый х это первый элемент пары: ф.конструктора пары забирает два параметра и полученная пара встает в голову z
((_, x) : _) -- да, х второй элемент пары из головы списка
((:) ((,) _ x) y) -- да, х это второй элемент головы
((,) y z : x) -- нет, х это хвост
((,) y x : z) -- да, х это второй элемент пары из головы
((,) ((:) _ _) x) -- нет, паттерн описывает пару а не список
```
test

### 3.1.7 рекурсивная обработка списков

При терминирующем условии `[]` (пустой список) и рекурсии на хвосте: мы всегда получаем сходящуюся ф.
```hs
-- стандартный паттерн рекурсии на списках
length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

-- concat
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
-- xs ++ [] = xs -- пусть будет ленивая ф. по второму параметру, по первому будет форсить до WHNF
(x:xs) ++ ys = x : (xs ++ ys)
-- второй параметр вообще не вычисляется здесь, что позволяет пихать в него бесконечные списки
{--
программа "зависнет" в ходе вычисления следующего выражения
ghci> Data.List.foldr (++) [] (repeat [1])
--}

-- null
null :: [a] -> Bool
null [] = True
null _ = False
```
repl

```hs
{--
Сформируйте список целых чисел
содержащий только те элементы исходного списка, значение которых нечетно
(фильтр, оставить нечетные и отбрость четные)
--}
GHCi> oddsOnly [2,5,7,10,11,12]
[5,7,11]

-- Для анализа четности можно использовать функции `odd` и `even` стандартной библиотеки

oddsOnly :: Integral a => [a] -> [a]

```
test [odds_only](./chapter-3.1/test-odds_only.hs)

### 3.1.9 другие варианты рекурсии на списках

```hs
-- последний элемент списка
last :: [a] -> a
last (x : []) = x -- терминирующее условие "один элемент в списке"
last (_ : xs) = last xs

-- список с отброшенным последним элементом
init :: [a] -> [a]
init [x] = [] -- терминирующее условие "один элемент в списке", сахар для `x : []`
init (x : xs) = x : init xs

-- есть набор стандартных функций на списках (что они дают на пустом списке?)
ghci> :t sum
sum :: (Foldable t, Num a) => t a -> a
ghci> :t product
product :: (Foldable t, Num a) => t a -> a
ghci> :t maximum
maximum :: (Foldable t, Ord a) => t a -> a

ghci> sum []
0
ghci> product []
1
ghci> maximum []
*** Exception: Prelude.maximum: empty list

-- обращение списка, reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x] -- works
-- с аккумулятором, только на конструкторах
reverse lst = loop lst [] where -- data, accum
    loop [] acc     = acc               -- empty data -> accum
    loop (x:xs) acc = loop xs (x : acc) -- update accum and call next iteration on tail of data
```
repl

```hs
-- Реализуйте функцию `isPalindrome`
-- которая определяет, является ли переданный ей список палиндромом

GHCi> isPalindrome "saippuakivikauppias"
True
GHCi> isPalindrome [1]
True
GHCi> isPalindrome [1, 2]
False

isPalindrome :: Eq a => [a] -> Bool

```
test

### 3.1.11 рекурсия на нескольких списках

```hs
ghci> :t zip
zip :: [a] -> [b] -> [(a, b)]
ghci> zip [1,2,3] "Hello"
[(1,'H'),(2,'e'),(3,'l')]
-- implementation
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- зип на трех списках
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs -- не пустые списки в аргументах
zip3 _ _ _ = [] -- все другие случаи (хоть один список пустой)

-- список пар в пару списков
ghci> :t unzip
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x,y): t) = (x:xs, y:ys) where (xs, ys) = unzip t
```
repl

```hs
{--
Составьте список сумм соответствующих элементов трех заданных списков
Длина результирующего списка должна быть равна длине самого длинного из заданных списков
«закончившиеся» списки не должны давать вклада в суммы
--}
GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3]

```
test [sum3](./chapter-3.1/test-sum3.hs)

```hs
{--
Напишите функцию `groupElems`
которая группирует одинаковые элементы в списке (если они идут подряд)
и возвращает список таких групп.
Разрешается использовать только функции, доступные из библиотеки Prelude
--}

GHCi> groupElems []
[]

GHCi> groupElems [1,2]
[[1],[2]]

GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]

GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]

-- повторяющиеся элементы собирает во внутренний список
-- не-повторяющиеся элементы имеют свои собственные группы-списки

```
test [group_elems](./chapter-3.1/test-group_elems.hs)

### 3.1.14 take, drop, splitAt, `!!`

```hs
ghci> :t Data.List.take
Data.List.take :: Int -> [a] -> [a]

take n _ | n <= 0   = []
take _ []           = []
take n (x:xs)       = x : take (n - 1) xs

ghci> :t Data.List.drop
Data.List.drop :: Int -> [a] -> [a]

drop n xs | n <= 0  = xs
drop _ []           = []
drop n (_:xs)       = drop (n - 1) xs

ghci> :t Data.List.splitAt 
Data.List.splitAt :: Int -> [a] -> ([a], [a])

splitAt n xs = (take n xs, drop n xs)

-- элемент по индексу
ghci> :t (!!)
(!!) :: GHC.Stack.Types.HasCallStack => [a] -> Int -> a

xs !! n | n < 0 = error "idx must be >= 0"
[] !! _         = error "empty list"
(x:_) !! 0      = x -- found it
(_:xs) !! n     = xs !! (n - 1) -- next iter

```
repl

## chapter 3.2, Функции высших порядков над списками

https://stepik.org/lesson/12321/step/1?unit=2785

### 3.2.2 filter, takeWhile, dropWhile, span, break

```hs
-- два параметра: предикат и список, возвращает список с элементами одобренными предикатом
ghci> :i filter
filter :: (a -> Bool) -> [a] -> [a]     -- Defined in ‘GHC.List’
ghci> filter (< 3) [1,2,3,4,5,4,3,2,1]
[1,2,2,1]

filter _ [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

-- сигнатура как у фильтра
ghci> :i takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]  -- Defined in ‘GHC.List’
ghci> takeWhile (< 3) [1,2,3,4,5,4,3,2,1]
[1,2]

takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = [] -- останавливается на первом же обломе предиката и дропает остаток

ghci> :i dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]  -- Defined in ‘GHC.List’
ghci> dropWhile (< 3) [1,2,3,4,5,4,3,2,1]
[3,4,5,4,3,2,1]

dropWhile _ [] = []
dropWhile p lst@(x:xs) -- `lst`: локальный алиас для не-деконструированного аргумента
  | p x = dropWhile p xs
  | otherwise = lst -- дропает до первого облома и остаток не трогает

-- сигнатура отличается на выходе, возвращает пару списков
ghci> :i span
span :: (a -> Bool) -> [a] -> ([a], [a])        -- Defined in ‘GHC.List’
ghci> span (< 3) [1,2,3,4,5,4,3,2,1]
([1,2],[3,4,5,4,3,2,1])

span p xs = (takeWhile p xs, dropWhile p xs)

-- как и спан но переворачивает предикат
ghci> : break
break :: (a -> Bool) -> [a] -> ([a], [a])       -- Defined in ‘GHC.List’
ghci> break (< 3) [1,2,3,4,5,4,3,2,1]
([],[1,2,3,4,5,4,3,2,1])

break p = span (not . p) -- `not . p` композиция функций, предикат завернутый в переворот-булева-значения
```
repl

```hs
{--
Напишите функцию `readDigits`
принимающую строку и возвращающую пару строк
Первый элемент пары содержит цифровой префикс исходной строки
второй - ее оставшуюся часть
--}
GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","")

-- В решении вам поможет функция `isDigit` из модуля `Data.Char`

readDigits :: String -> (String, String)
readDigits = undefined

```
test [read_digits](./chapter-3.2/test-read_digits.hs)

```hs
{--
Реализуйте функцию `filterDisj`
принимающую два унарных предиката и список
возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов
--}
GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj = undefined

```
test [filter_disj](./chapter-3.2/test-filter_disj.hs)

```hs
{--
Напишите реализацию функции `qsort`
Функция qsort должная принимать на вход список элементов
сортировать его в порядке возрастания с помощью сортировки Хоара:
для какого-то элемента `x` изначального списка (обычно выбирают первый)
делить список на элементы меньше и не меньше `x`
потом запускаться рекурсивно на обеих частях.
Разрешается использовать только функции, доступные из библиотеки Prelude
--}
GHCi> qsort [1,3,2,5]
[1,2,3,5]

qsort :: Ord a => [a] -> [a]
qsort = undefined

```
test [qsort](./chapter-3.2/test-qsort.hs)

### 3.2.6 map, concat, concatMap

Трансформация элементов списка, HOF
```hs
ghci> :i map
map :: (a -> b) -> [a] -> [b]   -- Defined in ‘GHC.Base’
ghci> map (+10) [1,2,3,5]
[11,12,13,15]

map _ [] = []
map f (x:xs) = f x : map f xs

-- flatten aka concat, not HOF
ghci> :i concat
concat :: Foldable t => t [a] -> [a]    -- Defined in ‘Data.Foldable’
ghci> concat ["foo", "bar"]
"foobar"

concat [] = []
concat (x:xs) = x ++ concat xs -- x :: list

-- flatMap aka concatMap, функция возвращает список, примененная к списку - создает список списков, которые потом flatten
ghci> :i concatMap
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
        -- Defined in ‘Data.Foldable’
ghci> concatMap (\x -> [x, toUpper x]) "abc"
"aAbBcC"

concatMap _ [] = []
concatMap f (x:xs) = concat (map f xs) -- можно убрать "точки"
concatMap f = concat . map f -- маппинг завернут в конкат, композиция
-- > bind и (concatMap для списков) одно и тоже? Да

-- как можно "выводить" композицию функций
map :: (a -> x) -> ([a] -> [x]  )
(concat .) ::      (y   -> [[b]]) -> (y -> [b])
-- унифицируем (заменяем) y := [a], x:= [b]
map       :: (a -> [b]) -> ([a] -> [[b]])
(concat .) ::              ([a] -> [[b]]) -> ([a] -> [b])
-- итого:               - cut       cut -
(concat .) . map :: (a -> [b]) -> ([a] -> [b])

\f xs -> concat (map f xs) =   -- определение композиции
\f xs -> (concat . map f) xs = -- эта-редукция 
\f -> (concat .) (map f) =     -- определение композиции
\f -> ((concat .) . map) f =   -- эта-редукция 
(concat .) . map

```
repl

https://stepik.org/lesson/12321/step/7?unit=2785
```hs
{--
Напишите функцию `squares'n'cubes`
принимающую список чисел 
возвращающую список квадратов и кубов элементов исходного списка
--}
ghci> squares'n'cubes [3,4,5]
[9,27,16,64,25,125]

squares'n'cubes :: Num a => [a] -> [a]

```
test

```hs
{--
Воспользовавшись функциями `map` и `concatMap`
определите функцию `perms`
которая возвращает все перестановки
которые можно получить из данного списка
в любом порядке
Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка
--}
ghci> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

perms :: [a] -> [[a]]
perms = undefined

```
test [test-perms](./chapter-3.2/test-perms.hs)

### 3.2.9 and, or, all, any

```hs
-- свертка списка булевых
and, or :: [Bool] -> Bool

ghci> and (True : repeat False)
False
ghci> or (True : repeat False)
True

and [] = True
and (x:xs) = x && (and xs)

or [] = False
or (x:xs) = x || (or xs)

-- HOF, проверка предикатов
ghci> :i all
all :: Foldable t => (a -> Bool) -> t a -> Bool
        -- Defined in ‘Data.Foldable’
ghci> all odd [1,3,43]
True

all p = and . map p -- composition, `map p` wrapped in `and`, as a result we have "check predicat result for each item"
-- pointfree form, keep in mind hidden argument: list of items

any p = or . map p -- same pattern as for `all`

ghci> any even [1,2,3,43]
True

-- interactive example (HOF on lists)
ghci> words "abc is not ABC"
["abc","is","not","ABC"]

ghci> unwords $ words "abc is not ABC"
"abc is not ABC"

ghci> unwords . words $ "abc is not ABC"
"abc is not ABC"

ghci> unwords . map reverse . words $ "abc is not ABC"
"cba si ton CBA"

-- make it a named function
ghci> let revWords = unwords . map reverse . words
ghci> revWords "abc is not ABC"
"cba si ton CBA"
```
repl

```hs
{--
Реализуйте функцию `delAllUpper`
удаляющую из текста все слова
целиком состоящие из символов в верхнем регистре

Предполагается, что текст состоит только из символов алфавита и пробелов
знаки пунктуации, цифры и т.п. отсутствуют

Постарайтесь реализовать эту функцию как цепочку композиций, аналогично `revWords` из предыдущего видео
--}

GHCi> delAllUpper "Abc IS not ABC"
"Abc not"

delAllUpper :: String -> String
delAllUpper = undefined

-- оставить слово если хоть одна буква в нижнем регистре
ghci> unwords . filter (any (\c -> c >= 'a' && c <= 'z')) . words $ "Abc IS not ABC"
"Abc not"

```
test

### 3.2.11 zipWith, zipWith3

```hs
ghci> :i zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
        -- Defined in ‘GHC.List’
ghci> zipWith (+) [1,2] [3,4,5] -- функ. бинарный оператор сложения, складываем попарно элементы списков
[4,6]

zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith xs ys

-- ф.  zip это частный случай: передадим "конструктор пары" как бинарный оператор и получим список пар
ghci> zipWith (,) [1,2] [3,4,5]
[(1,3),(2,4)]

-- с другим колич. аргументов уже тривиально
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 xs ys zs
```
repl

```hs
{--
Напишите функцию `max3`
которой передаются три списка одинаковой длины
которая возвращает список той же длины
содержащий на k-ой позиции наибольшее значение
из величин на этой позиции в списках-аргументах
--}
GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]
GHCi> max3 "AXZ" "YDW" "MLK"
"YXZ"

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = undefined

```
test

## chapter 3.3, Генераторы списков

https://stepik.org/lesson/8328/step/1?unit=1476

### 3.3.2 Бесконечные структуры

Бесконечные структуры (списки), можно оперировать ими благодаря ленивости языка.
```hs
-- расходящиеся, бесконечные программы

-- бесконечный цикл, нет терминирующего условия
let bot = not bot

-- бесконечный список, продуктивная функция (продуцирует результат)
let ones = 1 : ones

let nats n = n : nats (n + 1)

-- использовать беск. список в конечных программах: терминирующее условие
ghci> take 10 $ nats 5
[5,6,7,8,9,10,11,12,13,14]

-- как это работает на примере head
ghci> head $ nats 42
42
-- методом подстановки
head (x:xs) = x -- реализация через пат.мат.
head (nats 42)
head (n : nats (n + 1)) -- WHNT, не надо вычислять далее
42

-- использование в более сложных выражениях
let squares = map (^2) $ nats 1
ghci> :t squares
squares :: Num b => [b]

ghci> take 10 squares
[1,4,9,16,25,36,49,64,81,100]
```
repl

```hs
{--
Реализуйте c использованием функции `zipWith`
функцию `fibStream`
возвращающую бесконечный список чисел Фибоначчи
--}
GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]

fibStream :: [Integer]
fibStream = undefined

```
test [fib_stream](./chapter-3.3/test-fib_stream.hs)

### 3.3.4 repeat, replicate, cycle, iterate

Рассмотрим полезные стандартные ф. генераторы списков
```hs
ghci> :i repeat
repeat :: a -> [a]      -- Defined in ‘GHC.List’
ghci> take 5 $ repeat 'c'
"ccccc"
repeat x = xs where xs = x : xs -- оптимизированная реализация

ghci> :i replicate
replicate :: Int -> a -> [a]    -- Defined in ‘GHC.List’
ghci> replicate 5 'c'
"ccccc"
replicate n x = take n (repeat x)

ghci> :i cycle
cycle :: GHC.Stack.Types.HasCallStack => [a] -> [a]
        -- Defined in ‘GHC.List’
ghci> take 5 $ cycle [1,2,3]
[1,2,3,1,2]
cycle [] = error "empty list"
cycle xs = ys where ys = xs ++ ys -- 

ghci> :i iterate
iterate :: (a -> a) -> a -> [a]         -- Defined in ‘GHC.List’
ghci> take 7 $ iterate (+ 3) 1 -- 1+3, 1+3+3, ...
[1,4,7,10,13,16,19]
ghci> take 6 $ iterate (^2) 2 -- срез бинарного оператора "возведение в степень" применить к 2, потом к 2^2, ...
[2,4,16,256,65536,4294967296]
iterate f x = x : iterate f (f x)

-- оптимизация рекурсии
ghci> :set +s

let repeat' x = x : repeat' x -- медленно и много памяти, не делай так
ghci> repeat' 42 !! 100000000
42
(6.19 secs, 7,200,065,304 bytes)

let repeat'' x = xs where xs = x:xs -- быстро и мало памяти, будь как xs
ghci> repeat'' 42 !! 100000000
42
(0.28 secs, 65,304 bytes)
```
repl

```hs
{--
Предположим, что функция `repeat`
была бы определена следующим образом
`repeat = iterate repeatHelper`
определите, как должна выглядеть функция repeatHelper
--}
repeatHelper = undefined

-- reference
let repeat x = xs where xs = x:xs -- repeat 1 = [1,1,1,1, ...]
let iterate f x = x : iterate f (f x) -- iterate (+ 3) 1 = [1,4,7,10,13,16,19, ...

```
test

### 3.3.6 арифметические последовательности enumFrom..

Специальные конструкции для генерации списков
```hs
ghci> [False .. ]
[False,True]
ghci> [False .. True]
[False,True]

-- сахал для enumFromTo
ghci> [1 .. 10]
[1,2,3,4,5,6,7,8,9,10]

ghci> :i enumFromTo
type Enum :: * -> Constraint
class Enum a where -- typeclass Enum
  ...
  enumFromTo :: a -> a -> [a]
  ...
        -- Defined in ‘GHC.Enum’
ghci> enumFromTo 1 10
[1,2,3,4,5,6,7,8,9,10]

-- Char have an instance of typeclass Enum
ghci> [' ' .. '~']
" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

-- последовательность с шагом
ghci> [1,3 .. 10] -- шаг = 2, получить все нечетные числа до 10
[1,3,5,7,9]
-- сахар для enumFromThenTo
ghci> :i enumFromThenTo
type Enum :: * -> Constraint
class Enum a where
  ...
  enumFromThenTo :: a -> a -> a -> [a]
        -- Defined in ‘GHC.Enum’
ghci> enumFromThenTo 1 3 10
[1,3,5,7,9]

-- можно не указыать закрывающее значение, получим бесконечную последовательность
ghci> take 100 $ [' ' ..]
" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL\128\129\130\131"
-- сахар для enumFrom
ghci> :i enumFrom
type Enum :: * -> Constraint
class Enum a where
  ...
  enumFrom :: a -> [a]
  ...
        -- Defined in ‘GHC.Enum’

-- бесконечность с шагом (числа кратные 7)
ghci> take 7 [7,14 ..]
[7,14,21,28,35,42,49]
-- сахар для enumFromThen
ghci> :i enumFromThen
type Enum :: * -> Constraint
class Enum a where
  ...
  enumFromThen :: a -> a -> [a]
  ...
        -- Defined in ‘GHC.Enum’

-- Можно поразмышлять над следующими конструкциями:
[1,1 .. 1]
[1,1 .. 2]
[1,1 .. 0]
```
repl

```hs
-- Пусть задан тип `Odd` нечетных чисел следующим образом

data Odd = Odd Integer 
  deriving (Eq, Show)

-- Сделайте этот тип представителем класса типов `Enum`

GHCi> succ $ Odd (-100000000000003)
Odd (-100000000000001)

-- Конструкции с четным аргументом, типа `Odd 2`, считаются недопустимыми и не тестируются
{--
Примечание. Мы еще не знакомились с объявлениями пользовательских типов данных, 
однако, скорее всего, приведенное объявление не вызовет сложностей. 
Здесь объявляется тип данных `Odd` с конструктором `Odd`. 
Фактически это простая упаковка для типа `Integer`. 
Часть `deriving (Eq, Show)` указывает компилятору, чтобы он автоматически сгенерировал представителей соответствующих классов типов 
для нашего типа (такая возможность имеется для ряда стандартных классов типов). 
Значения типа `Odd` можно конструировать следующим образом:
--}
GHCi> let x = Odd 33
GHCi> x
Odd 33

-- и использовать конструктор данных `Odd` в сопоставлении с образцом:

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where

```
test [odd](./chapter-3.3/test-odd.hs)

### 3.3.8 list comprehension

Генераторы списков (не надо в них совать бесконечные списки!)
```hs
ghci> let xs = [1 .. 20]
ghci> xs
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

-- [ | ] -- верт. черта разделяет две части
-- слева формирование списка, справа источник
[x^2 | x <- xs] -- для каждого х из xs, взять квадрат х
[1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400]

-- из теории множеств: множество х^2 где x принадлежит xs `{x^2 ...}`
-- из исходного множества "выделяется" результирующее множество

[x^2 | x <- xs, x^2 < 200] -- с наложением ограничения
[1,4,9,16,25,36,49,64,81,100,121,144,169,196]

-- исходных множеств можно дать несколько
[(x, y) | x <- [1, 2], y <- ['a', 'b']]
-- для каждого из х-ов взять каждый из у-ов, сформировать элемент резулт.множества
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- правило: чем правее генератор, тем чаще он "перебирается", вложенность циклов перебора слева-направо

-- найдем пифагоровы тройки: целочисленные кортежи длин прямоугольных треугольников
ghci> [(x, y, z) | x <- xs, y <- xs, z <- xs, z^2 == x^2 + y^2, x <= y]
-- второе условие для устранения повторов
[(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]

-- Гипотенуза всегда больше любого катета, поэтому ее можно сделать бесконечным списком, а катеты подогнать под ограничения:
take 200 [(c1, c2, h) | h <- [1..], c1 <- [1..h], c2 <- [c1..h], h^2 == c1^2 + c2^2]
-- уже заметно тормозит
```
repl

```hs
{--
https://stepik.org/lesson/8328/step/9?unit=1476
Пусть есть список положительных достоинств монет `coins`
отсортированный по возрастанию

Воспользовавшись механизмом генераторов списков
напишите функцию `change`

которая разбивает переданную ей положительную сумму денег 
на монеты из списка `coins`
всеми возможными способами

Примечание. Порядок монет в каждом разбиении имеет значение, 
то есть наборы `[2,2,3]` и `[2,3,2]` — различаются.
Список `coins` определять не надо
--}
-- Например, если `coins = [2, 3, 7]`
GHCi> change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]

change :: (Ord a, Num a) => a -> [[a]]
change = undefined

```
test [change](./chapter-3.3/test-change.hs)

## chapter 3.4, Правая свертка

https://stepik.org/lesson/4745/step/1?unit=1081

```hs
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ ini [] = ini
foldr' f ini (x:xs) = f x (foldr' f ini xs)

-- !!! not used !!!
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' _ acc [] = acc
-- foldl' f acc (x:xs) = foldl' f (f acc x) xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr' (\ x res -> p x || res) False xs

elem' :: Eq a => a -> [a] -> Bool
elem' a xs = any' (== a) xs

length' :: [a] -> Int
length' xs = foldr' (const succ) 0 xs

sum' :: Num a => [a] -> a
sum' xs = foldr' (+) 0 xs

product' :: Num a => [a] -> a
product' xs = foldr' (*) 1 xs

concat' :: [[a]] -> [a]
concat' xs = foldr' (++) [] xs

-- !!! canonical, not used !!!
-- map' :: (a -> b) -> [a] -> [b]
-- map' _ [] = []
-- map' f (x:xs) = f x : map' f xs

-- map using foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr' (\ x rest -> f x : rest) [] xs

fact' :: (Enum a, Num a) => a -> a
fact' n = product' [1..n]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr' (\ x rest -> if p x then x : rest else rest) [] xs
```
repl

### 3.4.2 foldr, паттерн рекурсивной обработки списка

Постоянно видим один и тот же паттерн обработки списка: применяем ф. к голове и рекурсивно вызываем себя на хвосте.
Обобщение шаблона
```hs
--Вот три показательные ф. Видно паттерн? инициализирующее значение и операция могут быть вынесены наружу
-- получаем редукцию, свертку списка
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * sum xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- разберем желаемую сигнатуру свертки
fold :: (a -> b -> b) -> b -> [a] -> b
-- удобно составлять (и разбирать) сигнатуру с хвоста:
-- возвращает б, на входе: список а, инициализирующее значение б, функция редуцирования (a,b) -> b

-- реализация
fold f ini [] = ini -- на пустом входе: вернуть инициализирующее значение (затравку свертки)
fold f ini (x:xs) = x `f` (fold f ini xs) -- ini это НЕ аккумулятор, это затравка свертки
-- видно, как реализован вышеупомянутый паттерн свертки? (голова - оп. - рекурсивный-хвост)
-- если подумать, можно заметить, что это ПРАВАЯ свертка, вычисления стартуют после исчерпания списка рекурсией, 
-- с последнего элемента и ini значения.
-- поэтому, это foldR

-- foldL можно реализовать применив фокус с аккумулятором и TCO (внутренний метод `helper`, рекурсивный, оптимизированный в цикл)
-- TCO: tail call optimization

-- перепишем показательные ф. через свертку
sum xs = fold (+) 0 xs
sum = fold (+) 0 -- pointfree
```
repl

```hs
-- Напишите реализацию функции `concatList` через `foldr`

GHCi> concatList [[1,2],[],[3]]
[1,2,3]

concatList :: [[a]] -> [a]
concatList = foldr undefined undefined

-- имеем
foldr :: (a -> b -> b) -> b -> [a] -> b
-- с хвоста: возвращает б, на входе: список а, инициализирующее значение б, функция редуцирования (a,b) -> b

```
test

### 3.4.4 сложные бинарные операторы для foldr

Имея `foldr` хотим сложное посчитать, как это сделать?
```hs
-- имеем свертку
foldr :: (a -> b -> b) -> b -> [a] -> b -- параметры: функ. a -> b -> b; затравка b; список [a]; результат: b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

-- задача: вывести сумму квадратов положительных элементов списка

-- sumSquaresOfPositives
sumPositiveSquares :: [Integer] -> Integer
sumPositiveSquares = _
-- ini = 0 ибо сумма на пустоте дает 0
-- ф. редукции будет как лямбда: первый аргумент типа a - очередной элемент списка, второй типа b - частично свернутый список:
-- (\ x s -> if x > 0 then x^2 + s else s)
sumPositiveSquares = foldr (\ x s -> if x > 0 then x^2 + s else s) 0 -- pointfree, actual list not mentioned
ghci> sumPositiveSquares [1,(-2),3]
10

-- альтернативная запись
sumPositiveSquares = foldr f 0 where
  f x s = if x > 0 then x^2 + s else s
-- или
sumPositiveSquares = foldr f 0 where
  f x s | x > 0     = x^2 + s
        | otherwise = s

-- extra
sumPositiveSquares = foldr (+) 0 . map (^2) . filter (> 0)
sumPositiveSquares = sum . map (^2) . filter (> 0)

```
repl

```hs
{--
Используя функцию `foldr`
напишите реализацию функции `lengthList`
вычисляющей количество элементов в списке

GHCi> lengthList [7,6,5]
3
--}
lengthList :: [a] -> Int
lengthList = foldr undefined undefined

-- имеем свертку
foldr :: (a -> b -> b) -> b -> [a] -> b -- параметры: функ. a -> b -> b; затравка b; список [a]; результат: b

```
test

```hs
{--
Реализуйте функцию `sumOdd`
которая суммирует элементы списка целых чисел
имеющие нечетные значения

GHCi> sumOdd [2,5,30,37]
42
--}
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> undefined) undefined

-- имеем свертку
foldr :: (a -> b -> b) -> b -> [a] -> b -- параметры: функ. a -> b -> b; затравка b; список [a]; результат: b

```
test

### 3.4.7 разбор: почему это "правая" свертка

Для демонстрации нам нужен не-ассоциативный бинарный оператор
```hs
-- имеем свертку
foldr :: (a -> b -> b) -> b -> [a] -> b -- параметры: функ. a -> b -> b; затравка b; список [a]; результат: b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

-- demo:
ghci> foldr (-) 5 [1,2,3]
-3

{--
выполним подстановки:
n.b: хотя в реале редуцироваться будет сначала оператор `f`, по правилам в Хаскел, для целей демо не будем этого делать,
благо для конечных списков результат тот-же.
Для бесконечных списков редуцирование оператора позволяет заканчивать вычисления до наступления конца вселенной.

foldr f ini 1:2:3:[] -- список в такой форме для наглядности
~> 1 `f` (foldr f ini 2:3:[])
~> 1 `f` (2 `f` (foldr f ini 3:[]))
~> 1 `f` (2 `f` (3 `f` (foldr f ini [])))
~> 1 `f` (2 `f` (3 `f` ini))

видно, что оператор будет применен с правой ассоциативностью
--}

Можно на это смотреть так: правая свертка просто пересобирает список, воткнув бинарный оператор между элементами (справа).
Сборка списка идет от хвоста, добавлением головы, как и положено в нормальном списке.

"foldr build fusion"
> часть механизма оптимизации, применяемого в GHC, и 
позволяющего избежать создания промежуточных (или вообще каких либо) структур для конструкций вроде 
foldr f z . map g . filter h
```
repl

```hs
-- Какой функции стандартной библиотеки, суженной на списки, эквивалентно выражение 
foldr (:) []

foldr (:) [] xs -- binOp ini list
-- к хвосту подклеиваем голову = сборка списка, получим идентичный входному списко
-- ответ

```
test

```hs
Какой функции стандартной библиотеки эквивалентно выражение 
foldr const undefined

foldr const undefined xs -- binOp ini list
-- const это ф. двух параметров, возвращающая первый аргумент
-- следовательно, при первой же подстановке, const вернет первый элемент списка
-- foldr const undefined [1 ..] ~> const 1 (not-interested) -- второй аргумент даже вычисляться не будет
-- ответ:

```
test

## chapter 3.5, Левая свертка и ее сравнение с правой, foldl

https://stepik.org/lesson/5790/step/1?unit=1136

### 3.5.2 тип (сигнатура) foldl

Разберем, что нам надо от левой свертки
```hs
-- если правая свертка это:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini 1:2:3:[] ~> 1 `f` (2 `f` (3 `f` ini))

-- то левая должна быть
((ini `f` 1) `f` 2) `f` 3
-- или в виде применения функции
f(f(f ini 1) 2) 3

-- тип функции чуть-чуть отличается от foldr (функция свертки флипнута)
foldl :: (b -> a -> b) -> b -> [a] -> b
-- редуцирующая функция: (b,a) -> b; затравка: b; список: [a]; результат: b
```
repl

```hs
{--
При каком значении переменной `x`
следующие два выражения примут одно и то же значение
(отличное от неопределенного)
--}
foldr (-) x [2,1,5]
foldl (-) x [2,1,5]

-- задачка на понимание правой и левой ассоциативности и на решение уравнения

```
test

### 3.5.4 реализация foldl

Напишем возможную реализацию левой свертки
```hs
-- для референса, правая свертка:
foldr :: (a -> b -> b) -> b -> [a] -> b -- параметры: функ. a -> b -> b; затравка b; список [a]; результат: b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs) -- рекурсивный вызов находится в вычислении аргумента оператора
-- для вычисления всего надо дойти до конца списка

-- левая свертка:

-- ((ini `f` 1) `f` 2) `f` 3
-- что эквивалентно f(f(f ini 1) 2) 3
foldl :: (b -> a -> b) -> b -> [a] -> b
-- редуцирующая функция: (b,a) -> b; затравка: b; список: [a]; результат: b

foldl f ini [] = ini -- на пустом списке результат = затравке
-- на непустом списке: применить операцию к паре (x, acc) и вызвать рекурсивно на хвосте
foldl f ini (x:xs) = foldl f acc xs where acc = f ini x -- рекурсивный вызов снаружи, аргументы оператора не содержат рекурсии
-- что эквивалентно более короткой записи:
foldl f ini (x:xs) = foldl f (f ini x) xs
-- для вычисления всего не надо сначала дойти до конца списка, вычисления накапливаются по ходу спуска, TCO в помощь.

-- осознай разницу в местоположении рекурсивного вызова в foldr vs foldl

{--
выполним упражнение по подстановке

foldl f ini 1:2:3:[]
~> foldl f (f ini 1) 2:3:[]
~> foldl f (f (f ini 1) 2) 3:[]
~> foldl f (f (f (f ini 1) 2) 3) []
~> f (f (f ini 1) 2) 3

что и требовалось.
--}

Видно, что на больших списках foldl не эффективен, ибо накапливает thunk-и по размеру списка.
Поэтому, ленивая foldl крайне не рекомендована к использованию на практике.
Есть версия "строгая", которая не копит thunk-и, оптимизированная.
Ее можно использовать, хотя на бесконечных списках будет зависон.
```
repl

>  thunk при правой свертке не создается, там просто будет большой стек рекурсии (который, конечно, таки занимает много памяти).
А вот в левой свертке дело именно в thunk'ах, и она будет работать намного эффективнее (в смысле памяти, конечно) при энергичной реализации.

### 3.5.5 строгая foldl

Строгая (энергичная) foldl, без накопления thunk-ов
```hs
-- ленивая верия
-- редуцирующая функция: (b,a) -> b; затравка: b; список: [a]; результат: b
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (f ini x) xs

-- перепишем немного
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f ini' xs where ini' = f ini x

ini' = f ini x
-- это именно то, что нам надо форсировать, у нас есть все данные для вычисления, нет причины лениться

-- форсируем (до WHNF)
foldl f ini (x:xs) = ini' `seq` foldl f ini' xs where ini' = f ini x

-- оператор форсирования (был разбор ранее)
ghci> :i seq
seq :: a -> b -> b      -- Defined in ‘GHC.Prim’
infixr 0 `seq`
```
repl

[foldl'](https://hoogle.haskell.org/?hoogle=foldl%27&scope=set%3Ahaskell-platform)

### 3.5.6 свертки на бесконечных списках

```hs
-- reference

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (f ini x) xs -- ленивая, не строгая f

-- поведение на бесконечных списках

foldl f ini (x:xs) = foldl f (f ini x) xs
-- первое же выражение это вызов себя, это редекс, который тут же будет заменен на его тело,
-- этот процесс будет продолжаться, пока не кончится список или память.
-- Редукция выражений не дает работать функции свертки до достижения конца списка.
-- Если редуцирующая f не строгая по второму аргументу, то далее в глубину можно не ходить.

foldr f ini (x:xs) = x `f` (foldr f ini xs)
-- в головном редексе находится сворачивающая функция, при замене которой на тело
-- может произойти завершение вычислений, не исчерпывая список (или память).
-- Редукция выражений дает поработать функции свертки.

-- пример на бесконечном списке

-- есть ли в списке элемент удовлетворяющий предикату
ghci> :i any
any :: Foldable t => (a -> Bool) -> t a -> Bool

-- реализуем any через foldr
any p = foldr f ini where
  ini = False
  f x res = p x || res

-- однострочник
any p = foldr f ini where {ini = False; f x res = p x || res}

-- здесь ключевой момент такой: `p x || res` в реализации ф. свертки
-- оператор || игнорирует правый параметр, если левый = True
-- и когда такое произойдет, рекурсия завершится

--reference: оператор или (||)
False (||) x = x
True (||) _ = True

-- для доказательства можно расписать подстановки при вычислении выражения:
any (== 2) [1 ..]
```
repl

### 3.5.7 несколько сверток за один проход

Задача: вычислить сумму и произведение элементов списка
```hs
-- reference
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

sumProd = foldr binOp ini [1,2,3,4]
sumProd = foldr binOp (0, 1) [1,2,3,4] -- ини для суммы = 0, ини для произведения = 1
sumProd = foldr (\ x (s, p) -> _) (0, 1) [1,2,3,4] -- используем пат.мат. в лямбде
sumProd = foldr (\ x (s, p) -> (x + s, x * p)) (0, 1) [1,2,3,4] -- бин.оп. возвращает пару: сумма, произведение

ghci> foldr (\ x (s, p) -> (x + s, x * p)) (0, 1) [1,2,3,4]
(10,24)
```
repl

```hs
{--
Реализуйте функцию `meanList`
которая находит среднее значение элементов списка
используя однократный вызов функции свертки

GHCi> meanList [1,2,3,4]
2.5

Постобработка считается допустимой,
то есть предполагаемая реализация функции `meanList` имеет вид
meanList = someFun . foldr someFoldingFun someIni
--}

-- не "среднее" а "медиану", значение находящееся в середине списка
-- https://www.google.com/search?q=mean+vs+avg
-- нет, это я перепутал среднее с медианой. Нужно среднее.

meanList :: [Double] -> Double
meanList = undefined

```
test

```hs
{--
Используя однократный вызов свертки
реализуйте функцию `evenOnly`
которая выбрасывает из списка элементы, стоящие на нечетных местах
оставляя только четные

GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"
--}

evenOnly :: [a] -> [a]
evenOnly = undefined

```
test

```hs
{--
Попробуйте добиться того, чтобы реализованная вами в прошлом задании функция `evenOnly`
позволяла работать и с бесконечными списками

То есть, например, 
запрос на первые три элемента бесконечного списка, возвращаемого этой функцией, 
примененной к списку всех натуральных чисел, должен завершаться

GHCi> take 3 (evenOnly [1..])
[2,4,6]
--}

evenOnly :: [a] -> [a]
evenOnly = undefined

-- reference
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)
-- в головном редексе находится сворачивающая функция, при замене которой на тело
-- может произойти завершение вычислений, не исчерпывая список (или память). См. пример с `any`.
-- Редукция выражений дает поработать функции свертки.

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (f ini x) xs -- ленивая, не строгая f
-- первое же выражение это вызов себя, это редекс, который тут же будет заменен на его тело,
-- этот процесс будет продолжаться, пока не кончится список или память.
-- Редукция выражений не дает работать функции свертки до достижения конца списка.
-- Важно: если редуцирующая f не строгая по второму аргументу, то далее в глубину можно не ходить.

```
test

## chapter 3.6, Родственные сверткам функции

https://stepik.org/lesson/6196/step/1?unit=1229

### 3.6.2 foldl1, foldr1

Существуют алгоритмы, поведение которых на пустом списке не определено.
Например: max, min, etc.
Для таких случаев существуют ф. высшего порядка `foldl1, foldr1`.

На пустом списке ломается, на непустом списке:
использует в качестве `ini` значения последний (foldr1) или первый (foldl1) элемент списка.

```hs
ghci> :i foldl1
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  ...
  foldl1 :: (a -> a -> a) -> t a -> a
  ...
        -- Defined in ‘Data.Foldable’

ghci> :i foldr1
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  ...
  foldr1 :: (a -> a -> a) -> t a -> a
  ...
        -- Defined in ‘Data.Foldable’

ghci> foldl1 (+) [1,2,3]
6
ghci> foldr1 (+) [1,2,3]
6
ghci> foldl1 (+) [1]
1
ghci> foldr1 (+) [1]
1

ghci> foldr1 (+) []
*** Exception: Prelude.foldr1: empty list

ghci> foldl1 (+) []
*** Exception: Prelude.foldl1: empty list

-- implementation

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = x `f` (foldr1 f xs)
foldr1 _ [] = error "Empty list"

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ [x] = x
foldl1 f (x:xs) = foldl f x xs -- using `foldl` with x as an ini value
foldl1 _ [] = error "Empty list"

-- reference, non-strict foldl
foldl f ini (x:xs) = foldl f (f ini x) xs

-- применение
maximum :: (Ord a) => [a] -> a
maximum = foldl1 max
```
repl

> правая свертка будет вычисляться лениво для всего списка и приведет к накапливанию большого количества thunk’ов и переполнению стека.
Левая свертка тоже не идеальна, поскольку она будет накапливать отложенные вычисления у себя в аккумуляторе, но это, всё-таки, лучше,
а вообще в определении `maximum` самое правильное это использовать строгую ленивую свертку `foldl1`

```hs
{--
Напишите реализацию функции
возвращающей последний элемент списка
через `foldl1`
--}

lastElem :: [a] -> a
lastElem = foldl1 undefined

```
test

### 3.6.4 scanl

Вычисление с накапливанием промежуточных результатов, scan
```hs
-- reference, foldl как основа scanl
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (ini `f` x) xs -- не строгая f

-- foldl списка [1,2,3] даст конструкцию
-- ((ini `f` 1) `f` 2) `f` 3

-- мы хотим получить список из промежуточных значений
-- [(ini), (ini `f` 1), ((ini `f` 1) `f` 2), (((ini `f` 1) `f` 2) `f` 3)]

ghci> scanl (+) 0 [1,2,3]
[0,1,3,6]

-- implementation
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f ini [] = [ini]
scanl f ini (x:xs) = ini : (scanl f acc xs) where acc = ini `f` x -- отличие от foldl небольшое: выращивание списка из результатов на каждом шаге

-- factorials
ghci> scanl (*) 1 [1 .. 10]
[1,1,2,6,24,120,720,5040,40320,362880,3628800]

-- extra
ghci> :i scanr
scanr :: (a -> b -> b) -> b -> [a] -> [b]       -- Defined in ‘GHC.List’
ghci> scanr (*) 1 [1 .. 10]
[3628800,3628800,1814400,604800,151200,30240,5040,720,90,10,1]
```
repl

### 3.6.5 scanl applications

scanl умеет в бесконечные списки (обладает свойством "продуктивности"),
ибо на каждом шаге строит результат рекурсивно, подклеивая новую голову в список.
Это открывает интересные применения для такого инструмента.
```hs
-- причина возросшей лени? Конструктор списка?
scanl f ini (x:xs) = ini : (scanl f acc xs) where acc = ini `f` x

-- стрим факториалов
facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1 ..]

ghci> facs !! 5
120
ghci> facs !! 50
30414093201713378043612608166064768844377641568960512000000000000

-- частичные (префиксные) суммы
partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

ghci> partialSums [1 .. 10]
[0,1,3,6,10,15,21,28,36,45,55]

-- можно оценить сходимость числовых рядов, к примеру
-- ряд обратных факториалов, сумма ряда сходится к числу е
map (**(-1)) facs -- бесконечный список обратных факториалов

ghci> take 7 (map (**(-1)) facs )
[1.0,1.0,0.5,0.16666666666666666,4.1666666666666664e-2,8.333333333333333e-3,1.388888888888889e-3]

ghci> take 15 . partialSums . map (**(-1)) $ facs
[0.0,1.0,2.0,2.5,2.6666666666666665,2.708333333333333,2.7166666666666663,2.7180555555555554,2.7182539682539684,2.71827876984127,2.7182815255731922,2.7182818011463845,2.718281826198493,2.7182818282861687,2.7182818284467594]

-- уже на 8 шаге стало похоже на правду
ghci> take 9 . partialSums . map (**(-1)) $ facs
[0.0,1.0,2.0,2.5,2.6666666666666665,2.708333333333333,2.7166666666666663,2.7180555555555554,2.7182539682539684]

```
repl

### 3.6.6 scanr

Правое сканирование (если есть левое, то есть и правое, не так ли).
Не работает с бесконечными потоками.
Иногда может, если заменить ф. `f` неким конструктором, чтобы спрятать продолжение за барьером.
```hs
-- возьмем foldr как референс
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)
-- в головном редексе находится сворачивающая функция, при замене которой на тело
-- может произойти завершение вычислений, не исчерпывая список (или память). См. пример с `any`.
-- Редукция выражений дает поработать функции свертки.

-- вычисление дает такую структуру выражения
(1 `f` (2 `f` (3 `f` ini)))

-- мы хотим получить частичные результаты
[(1 `f` (2 `f` (3 `f` ini))), (2 `f` (3 `f` ini)), (3 `f` ini), ini]

-- implementation
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f ini [] = [ini]
scanr f ini (x:xs) = (x `f` th) : tail where tail@(th : _) = scanr f ini xs
-- тут надо понять рекурсию `scanr f ini xs` и ее использование в построении списка

-- интересно, что будет, если пар.мат заменить на вызов `head`. Повышение ленивости приведет к поддержке стримов?
-- Нет, проблема в ф. `f` и ее рекурсивном вызове до дна списка
scanr f ini (x:xs) = (x `f` (head tail)) : tail where tail = scanr f ini xs

-- результат
ghci> scanr (+) 0 []
ghci> scanr (+) 0 [1,2,3]
[6,5,3,0]
-- сравните с левым сканированием
ghci> scanl (+) 0 [1,2,3]
[0,1,3,6]
```
repl

### 3.6.7 unfold

Как может быть сделана ф. обратная свертке.
Сделать список из значения.
```hs
-- построим сигнатуру
unfold :: (b -> (a, b)) -> b - > [a]
-- b это инициализирующее значение, [a] это результат.
-- b -> (a, b) это функция, берущая текущий (промежуточный) инициализатор b и возвращающая пару (a, b)
-- a надо подклеить к выходному списку, b передать на след. итерацию

-- implementation
unfold f ini = let (x, ini') = f ini in -- получаем первое значение и ...
  x : unfold f ini' -- строим список рекурсивно

-- где такое можно применить?
-- например, реализовать стандартную `iterate` через `unfold`
iterate :: (a -> a) -> a -> [a] -- берет ф. и затравку, рекурсивно применяет ф. к предыдущему результату и добавляет в список
[x, f x, f (f x), f (f (f x)), ...] -- ожидаемый список на выходе должен иметь такую структуру
-- через тривиальную рекурсию будет так
iterate f x =  x : iterate f (f x)

-- через анфолд будет тоже несложно
iterate f = unfold (\x -> (x, f x)) -- если смотреть на сигнатуры, реализация тривиальная

-- example, последовательное возведение в квадрат
ghci> take 8 . iterate (^2) $ 2
[2,4,16,256,65536,4294967296,18446744073709551616,340282366920938463463374607431768211456]

ghci> take 8 . iterate (/2) $ 10
[10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,7.8125e-2]
```
repl

### 3.6.8 Maybe, find, lookup

Чтобы цикл в unfold мог завершаться, воспользуемся типом данных Maybe
```hs
-- два конструктора
ghci> :t Nothing
Nothing :: Maybe a -- ничего не содержит, кроме возможности матчить по конструктору
ghci> :t Just
Just :: a -> Maybe a -- параметризован нужным значением

-- Расширяем тип Bool с двух значений до трех:
-- Just Bool (два значения) и Nothing.

-- Data.List содержит интересные ф.
ghci> import Data.List

ghci> :i find
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
        -- Defined in ‘Data.Foldable’
-- берет предикат, список, возвращает опциональный элемент

ghci> find odd [2,3,4]
Just 3
ghci> find odd [2,4]
Nothing

ghci> :i lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
        -- Defined in ‘GHC.List’
-- берет ключ, список пар и возвращает опциональное значение под ключом

ghci> lookup 2 [(2, "foo"), (3, "bar")]
Just "foo"
ghci> lookup 42 [(2, "foo"), (3, "bar")]
Nothing
```
repl

### 3.6.9 unfoldr, build

Реализуем конечный unfold с использованием Maybe
```hs
ghci> :i unfoldr
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
        -- Defined in ‘base-4.17.2.0:Data.OldList’
-- берем иниц. значение, ф. из него в опциональную пару и генерируем список

unfoldr f ini = helper (f ini) where
  helper Nothing = []
  helper (Just (x, ini')) = x : unfoldr f ini'

-- пример: взяли начало 0 и добавляем к нему число 2, складыавя в результат эти плюс-двойки
ghci> unfoldr (\x -> if x >= 10 || x < 0 then Nothing else Just (x, x+2)) 0
[0,2,4,6,8]

```
repl

`build` встроенный работает приблизительно так же.

Пара `fold`, `build` может сокращать друг-друга.
Не самая сложная оптимизация, устраняющая промоежуточные создания-удаления списков.
В итератор инжектить цепочку операций, не сложно, правда ведь.

> не нужно `build` использовать напрямую.
достаточно того, что для генерации списка вы используете одну из функций, использующих build (например ту же `unfoldr` или `[x..y]` ...

> если продюсеры (функции генерирующие списки) выражены через
`build :: (forall b. (a -> b -> b) -> b -> b) -> [a]` 
`build g = g (:) []` 
а обработчики выражены через foldr, то мы можем воспользоваться замечательным правилом short cut fusion,
а именно, эквивалентностью 
`foldr c n (build g) == g c n`

```hs
{--
Используя `unfoldr`, реализуйте функцию
которая возвращает в обратном алфавитном порядке
список символов, попадающих в заданный парой диапазон

Попадание символа `x` в диапазон пары `(a,b)`
означает, что `x >= a` и `x <= b`

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g where g = undefined

GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"
--}
revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = undefined

-- reference
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- берем иниц. значение b, ф. из него в опциональную пару (a,b) и генерируем список [a]
unfoldr f ini = helper (f ini) where
  helper Nothing = []
  helper (Just (x, ini')) = x : unfoldr f ini'

-- пример: взяли начало 0 и добавляем к нему число 2, складыавя в результат эти плюс-двойки
ghci> unfoldr (\x -> if x >= 10 || x < 0 then Nothing else Just (x, x+2)) 0
[0,2,4,6,8]

-- решение: нужно от второго значения пары двигать вниз, до достижения первого значения

```
test
