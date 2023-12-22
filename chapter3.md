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
-- addTwoElements x y lst = x : y : lst
-- addTwoElements x y = (x :) . (y :)
-- addTwoElements = (. (:)) . (.) . (:)
addTwoElements = (.) `on` (:)

-- pointfree https://pointfree.io/
addTwoElements x y l = x : y : l
addTwoElements x y l = (:) x ((:) y l)
addTwoElements x y l = ((:) x . (:) y) l
addTwoElements x y = (:) x . (:) y
addTwoElements x y = ((:) x .) . (:)) y
addTwoElements x = ((:) x .) . (:)
...
-- Использовал 2 преобразования:
\x -> f x <=> f 
f (g x) <=> (f . g) x
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
nTimes _ 0 = [] -- eager recursion
nTimes elem n
    | n < 0     = [] -- just in case, make it total function
    | otherwise = elem : nTimes elem (n - 1)
-- replicate 3 10
-- take 10 (repeat 5)
-- а можно с аккумулятором, через рекурсивного хелпера
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
oddsOnly [] = []
oddsOnly (x:xs)
    | odd x = x : oddsOnly xs
    | otherwise = oddsOnly xs

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
isPalindrome xs = reverse xs == xs
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

-- редуцирование до sum2
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = xs `sum2` ys `sum2` zs
  where
    sum2 [] bs = bs
    sum2 as [] = as
    sum2 (a : as) (b : bs) = (a + b) : sum2 as bs

-- добивка 0 пустых списков
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] ys zs = sum3 [0] ys zs
sum3 xs [] zs = sum3 xs [0] zs
sum3 xs ys [] = sum3 xs ys [0]
sum3 (x:xs) (y:ys) (z:zs) = x+y+z : sum3 xs ys zs
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

-- простая рекурсия с гарантированно непустым результатом внутри
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] =[[x]]
groupElems (x : xs) =   if x /= head (head next)
                        then [x] : next
                        else (x : head next) : tail next where
                            next = groupElems xs

-- с аккумулятором и "забеганием вперед"
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = take' [x] xs where
    take' curr [] = [curr]
    take' (x:xs) (y:ys) = if x == y
                          then take' (x:y:xs) ys
                          else (x:xs) : take' [y] ys
-- то же самое но сбоку
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = groupElems' [x] xs
    where groupElems' eqs [] = [eqs]
          groupElems' eqs (x:xs) 
                      | (head eqs == x) = groupElems' (x : eqs) xs
                      | otherwise = eqs : groupElems' [x] xs

-- фокус с функцией `span`
groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems (x:xs) = (x:ys) : groupElems zs
    where (ys,zs) = span (== x) xs

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

readDigits :: String -> (String, String)
readDigits = span (<= '9')

readDigits :: String -> (String, String)
readDigits = span (\x -> (>= '0') x && (<= '9') x)

readDigits :: String -> (String, String)
readDigits = break (not . isDigit)
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

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)
-- filterDisj = (filter .) . liftM2 (||) -- pointfree.io
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

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = let (l, r) = splitBy (< x) xs where
  splitBy pred = foldr f ([], []) where
    f x ~(yes, no) | pred x = (x : yes, no) 
                   | otherwise = (yes, x : no)
  in qsort l ++ x : qsort r

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

concanMap _ [] = []
concatMap f (x:xs) = concat (map f xs) -- можно убрать "точки"
concatMap f = concat . map f -- маппинг завернут в конкат, композиция
-- > bind и (concatMap для списков) одно и тоже? Да

-- как можно "посчитать" композицию функций
map :: (a -> x) -> ([a] -> [x]  )
(concat .) ::      (y   -> [[b]]) -> (y -> [b])
-- унифицируем (заменяем) y := [a], x:= [b]
map       :: (a -> [b]) -> ([a] -> [[b]])
(concat .) ::              ([a] -> [[b]]) -> ([a] -> [b])
-- итого:               - cut       cut -
(concat .) . map :: (a -> [b]) -> ([a] -> [b])

\f xs -> concat (map f xs) =   -- определение композиции
\f xs -> (concat . map f) xs = -- эта-редукция 
\f -> concat . map f =         -- определение сечения
\f -> (concat .) (map f) =     -- определение композиции -- ИСПРАВИЛ ОПЕЧАТКУ
\f -> ((concat .) . map) f =   -- эта-редукция 
(concat .) . map

```
repl

```hs
https://stepik.org/lesson/12321/step/7?unit=2785
```
test
