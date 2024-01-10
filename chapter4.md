# FP Haskell, chapter 4, data types

[Функциональное программирование на языке Haskell / Денис Москвин / stepik](https://stepik.org/course/75/syllabus?next=)

## chapter 4.1, Sum types

https://stepik.org/lesson/4916/step/1?unit=1082

Типы перечислений (перечисляемые типы)

Пользовательские типы данных без параметров, с перечислимым количеством конструкторов: sum types.
Почему сумма? Мощность множества типа определяется количеством его конструкторов

### 4.1.2 объявление типа данных (data TypeName)

Пользовательские (не встроенные) типы данных.
Как можно определить свой тип данных
```hs
-- конструктор типа = конструкторы данных (тут два, без параметров)
data Bool = True | False

-- Конструктор типа и конструктор данных: две разные вещи.
-- Конструктор данных встречается в выражениях, тип используется в определении сигнатур (типов) выражений.

-- пример: в описании типа ф. использовали тип, в выражении реализации использовали значение (конструктор данных)
alwaysTrue :: Int -> Bool
alwaysTrue _ = True

-- Конструкторы и типы всегда начинаются с Большой Буквы.
ghci> data b = T | F
<interactive>:1:6: error:    Malformed head of type or class declaration: b
ghci> data B = t | f
<interactive>:2:10: error: Not a data constructor: ‘t’
```
repl

Когда говорят "конструктор" имеют в виду "конструктор данных".

```hs
-- Выберите корректные определения типов данных

data T = Con1 | Con2    -- да, с большой буквы, два конструктора через дождик
data myType = A | B | C -- нет, с маленькой буквы
data T = Con1 | myCon2  -- нет, с маленькой
data Type = A | B | C   -- да? слово `Type` не сломает? не сломает.
data T = A              -- да
data Data = A | B       -- да? слово `Data` не сломает? не сломает.
```
test

### 4.1.4 pattern matching on `data` types

Как работает пат.мат. на пользовательских типах данных
```hs
-- тип Б с двумя конструкторами, Т и Ф
data B = T | F

-- хотим функцию отрицания: на двух значениях банально, через пат.мат.
not :: B -> B
not T = F
not F = T

-- попытка использовать в репл: он не знает как отобразить результат на консоль (тайпкласс Show метод print)
ghci> not T
<interactive>:11:1: error:
    • No instance for (Show B) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it

-- используем автоматический вывод инстансов тайпклассов:
data B = T | F deriving Show
...
-- теперь норм
ghci> not T
F

-- в автовыводе можно генерировать довольно много тайпклассов
data B = T | F deriving (Show, Eq, Read, Enum)
-- точно, работает
ghci> F == T
False
ghci> succ T
F
ghci> pred F
T

-- что будет, если в пат.мат. испльзовать не все значения? ошибка не-исчерпывающего пат.мат.
not :: B -> B
not T = F
-- функция стала частичной (не полной) partial, not total

ghci> not F
*** Exception: <interactive>:30:1-9: Non-exhaustive patterns in function not

-- есть ключ компилятора для раннего обнаружения partial обьявлений
ghci> :set -fwarn-incomplete-patterns
```
repl: `:{\n multiline snippet \n:}`

```hs
{--
Тип данных `Color` определен следующим образом
data Color = Red | Green | Blue
Определите экземпляр класса `Show` для типа `Color`
сопоставляющий каждому из трех цветов его текстовое представление

GHCi> show Red
"Red"
--}

instance Show Color where
    show = undefined

-- reference
ghci> :t show
show :: Show a => a -> String

-- решение: надо определить, какой из трех конструкторов пришел, выдать соотв. строку
:{
instance Show Color where
    show Red    = "Red"
    show Green  = "Green"
    show Blue   = "Blue"
    show _      = error "unknown color"
:}
-- Добавил избыточный паттерн-дырку, на случай если кто-то добавит цвет и забудет поправить инстанс. 
-- Хотя правильнее было бы использовать флаг warn-incomplete-patterns

-- альтернатива
data Color = Red | Green | Blue deriving Show
```
test

### 4.1.6 pattern matching on literals

Литералы стандартных типов как образцы (для пат.мат.).
Литерал `42` как один из конструкторов типа данных `Int`, к примеру.
Значения типов как перечисления, Enum
```hs
-- example, pat.mat. on Int literals
intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
-- это частичная (расходящаяся ф.)
-- можем сделать ее полной
intToChar _ = 'N'

-- аналогично с символами (total)
isz :: Char -> Bool
isz 'z' = True
isz _ = False

-- и со строками (partial); n.b. строка как список символов
stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False
```
repl

```hs
{--
Определите частичную (определенную на значениях от '0' до '9') функцию `charToInt`

GHCi> charToInt '0'
0
GHCi> charToInt '9'
9
--}
charToInt :: Char -> Int
charToInt = undefined

-- reference
ghci> fromEnum '0'
48
ghci> fromEnum '9'
57

-- решение: предполагаю, задачка на написание пачки пат.матов.
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

-- альтернатива
charToInt :: Char -> Int
charToInt c = if x >= 0 && x <= 9 then x else undefined where x = fromEnum c - 48

```
test

```hs
{--
Определите (частичную) функцию `stringToColor`
которая по строковому представлению цвета как в прошлой задаче возвращает исходный цвет

GHCi> stringToColor "Red"
Red
--}
data Color = Red | Green | Blue
stringToColor :: String -> Color
stringToColor = undefined

-- решение: упражнение на написание пат.мат.-ов
data Color = Red | Green | Blue
stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

-- альтернатива
data Color = Red | Green | Blue deriving (Read)
stringToColor :: String -> Color
stringToColor = read
```
test

### 4.1.9 порядок пат.мат.

Пат.мат. в функции с несколькими параметрами: сверху-вниз, слева-направо.
Есть три варианта результата пат.мат: success, fail, undefined.
Расходимость может возникнуть при форсировании вычислений до WHNF в пат.мат.
```hs
-- пример: второй параметр 2 или любой, первый либо 1 либо 0
-- в остальных случаях неопределено (partial)
foo 1 2 = 3
foo 0 _ = 5

-- форсирования второго параметра нет, поэтому вычисление расходимости отброшено
ghci> foo 0 undefined
5
-- первое сопоставление зафейлилось на первом параметре, второй не вычислялся.
-- второй паттерн игнорирует второй параметр, по первому сукес.

-- даже так не будет форсирования, просто связали `х` и не использовали его
foo 0 x = 5

-- а вот если первым параметром дать расхождение, то при форсировании undefined на первом же паттерне мы отвалимся
ghci> foo undefined 0
*** Exception: Prelude.undefined

-- еще пример: форсирование вычисления во втором параметре на первом паттерне
-- первый параметр сматчился, чтобы сматчить второй параметр надо форсировать вычисление выражения
ghci> foo 1 (5 - 3)
3

-- принцип сверху-вниз и слева-направо справедлив и для более сложных структур
-- возьмем конструкторы типа данных пары
foo (1, 2) = 3
foo (0, _) = 5
```
repl

Следствие: порядок параметров имеет значение, влияет на сходимость-расходимость программы.

`_`: wildcard, joker, hole (дырка).

```hs
{--
Пусть определены следующие функции:

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

Выберите варианты вызовов этих функций, 
при которых сопоставление с образцом будет осуществлено успешно
-}
emptyOrSingleton undefined 5        -- сломается на первом параметре (форсирование андеф до булеан)
emptyOrSingleton True undefined     -- да, форсирования второго параметра нет
emptyOrSingleton False undefined    -- да, второй параметр игнорируется

isEqual undefined undefined                             -- сломается на форсировании первого параметра (это пара или нет?)
isEqual undefined (undefined, undefined)                -- аналогично
isEqual (undefined, undefined) (undefined, undefined)   -- да, форсирование будет только в правой части
isEqual (undefined, undefined) undefined                -- сломается на второй паре
```
test

```hs
{--
Тип `LogLevel` описывает различные уровни логирования

data LogLevel = Error | Warning | Info

Определите функцию `cmp`, сравнивающую элементы типа `LogLevel`
так, чтобы было верно, что `Error > Warning > Info`

GHCi> cmp Error Warning
GT
GHCi> cmp Info Warning
LT
GHCi> cmp Warning Warning
EQ
--}
cmp :: LogLevel -> LogLevel -> Ordering
cmp = undefined

-- решение, предполагаю требуется использование пат.мат.
:{
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Info Info = EQ
cmp Warning Warning = EQ
cmp Error _ = GT
cmp Info _ = LT
cmp _ Error = LT
cmp _ Info = GT
:}

-- alternative
import Data.Function
instance Enum LogLevel where
    fromEnum Error   = 2
    fromEnum Warning = 1
    fromEnum Info    = 0
cmp :: LogLevel -> LogLevel -> Ordering
cmp = on compare fromEnum

ghci> :i on
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```
test

### 4.1.12 `case of` и пат.мат.

Пат.мат. в правой части определения функции.
Не обязательно пат.мат.-у находится в левой части, есть разные варианты использования пат.мат.
`case of`
```hs
-- рассмотрим пример: сравнивание уровней логирования
-- пусть у нас есть три уровня и есть ф. сравнения двух уровней
data LogLevel = Error | Warning | Info
cmp :: LogLevel -> LogLevel -> Ordering

-- допустим, мы хотим функцию сравнения уровня с уровнем ошибки
lessThanError :: LogLevel -> Bool
lessThanError lvl = 
    case cmp lvl Error of
        LT -> True
        _  -> False

```
repl

`case of` сам по себе используется редко в пользовательских программах. Традиционный пат.мат. заметно удобнее и читабельнее.
Однако, компилятор при трансляции кода рассахаривает традиционный пат.мат. как раз в конструкции `case of`.
Это более низкоуровневый конструкт языка. Выражения пат.мат. это сахар для `case of`.

```hs
{--
Пусть объявлен следующий тип данных

data Result = Fail | Success

И допустим определен некоторый тип данных `SomeData`
и некоторая функция

doSomeWork :: SomeData -> (Result, Int)

возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха

Определите функцию `processData`
которая вызывает `doSomeWork`
возвращает строку "Success" в случае ее успешного завершения
либо строку "Fail: N" в случае неудачи, где N — код ошибки.
--}
processData :: SomeData -> String
processData = undefined

-- решение: задачка на пат.мат. и/или `case of`
processData :: SomeData -> String
processData x = case doSomeWork x of
    (Success, _) -> "Success"
    (_, errCode) -> "Fail: " ++ show errCode

-- alternative
processData :: SomeData -> String
processData = (\p -> case p of { (Success, 0) -> "Success"; (Fail, n) -> "Fail: " ++ show n }) . doSomeWork

processData :: SomeData -> String
processData = m . doSomeWork
    where m (r, err) = case r of
                        Fail -> "Fail: " ++ show err
                        Success -> "Success"

```
test

## chapter 4.2, Product types

4.2 Типы произведений и сумм произведений
https://stepik.org/lesson/4985/step/1?unit=1083

- Типы произведения
- Типы сумм произведений
- Функции, заменяющие конструкторы
- Ленивые образцы

### 4.2.2 product types

Типы данных с аргументами - тип "произведение".
Почему? Декартово произведение, вот почему.
Область определения вычисляется как `x * y` если `x` и `y` это параметры конструктора.
Тип продакт соответствует туплам (кортежам) такого же размера.
```hs
-- тип данных = конструктор, один но с двумя параметрами типа дабл
data Point = Pt Double Double

-- конструктор выглядит как функция с двумя параметрами
ghci> :t Pt
Pt :: Double -> Double -> Point

-- для экспериментов в консоли выведем инстанс тайпкласса Show
data Point = Pt Double Double deriving Show

ghci> Pt 3.0 4.0
Pt 3.0 4.0

-- пример конструирования точки через вспомогательную ф.
origin :: Point
origin = Pt 0.0 0.0

-- использовать конструктор в пат.мат. просто: группируем в скобках выражение конструктора
distanceToOrigin :: Point -> Double
distanceToOrigin (Pt x y) = sqrt (x^2 + y^2)

-- обычно (удобно) конструктор называть так же как тип данных
data Point = Point Double Double
-- напомню: конфликтов имен не будет, конструктор используется в выражениях, тип используется в типах (сигнатурах)
```
repl

```hs
{--
Реализуйте функцию `distance`
возвращающую расстояние между двумя точками
--}
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance = undefined

-- решение: квадрат гипотенузы равен сумме квадратов катетов
data Point = Point Double Double
origin :: Point
origin = Point 0.0 0.0
distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt (lenX ^ 2 + lenY ^ 2) where
    lenX = x2 - x1
    lenY = y2 - y1

ghci> distance (Point 1 2) (Point 3 4)
2.8284271247461903

-- notes
> distanceToOrigin - норма https://ru.wikipedia.org/wiki/Нормированное_пространство#Метрика_нормированного_пространства_и_связь_с_нормой

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = distanceToOrigin $ Point (x2 - x1) (y2 - y1)
```
test

### 4.2.4 Суммы произведений, sum of products

Посмотрели на типы сумм, на типы произведений.
Теперь посмотрим на типы сумм произведений
```hs
-- мотивация: корни квадратного уравнения
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = (x1, x2) where
    x1 = helper (-d)
    x2 = helper d
    helper x = (-b + x) / (2 * a)
    d = sqrt discr
    discr = b ^ 2 - 4 * a * c

-- что будет при отрицвтельном дискриминанте? будет (NaN, NaN)
-- мы хотим отразить такую возможность на уровне типов

-- определим корни как сумму двух множеств, где одно множество это None а второе: тип-произведение для пары чисел - корней
data Roots = Roots Double Double | None
    deriving Show
-- уже тип данных говорит нам о том, что корни могут быть а могут не быть;
-- сравните с (Double, Double) который не содержит этой информации

-- тогда ф. вычисления корней будет:
roots :: Double -> Double -> Double -> Roots
roots a b c
    | discr >= 0    = Roots x1 x2
    | otherwise     = None
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c
-- отобразили в типах поведение фунции
```
repl

```hs
{--
Определим тип фигур `Shape`
У него два конструктора: `Circle r` — окружность радиуса `r`
`Rectangle a b` — прямоугольник с размерами сторон `a` и `b`

data Shape = Circle Double | Rectangle Double Double

Реализуйте функцию `area`, возвращающую площадь фигуры
Константа `pi` уже определена в стандартной библиотеке
--}
data Shape = Circle Double | Rectangle Double Double
area :: Shape -> Double
area = undefined

-- решение: площадь круга пи-эр-квадрат, площадь прямоугольника а-умножить-б
-- тип данных: сумма, поэтому для каждого конструктора делаем отдельный паттерн
data Shape = Circle Double | Rectangle Double Double
area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b

ghci> area (Circle 1)
3.141592653589793
ghci> area (Rectangle 2 3)
6.0

-- notes
-- приоритет возведения в степень выше умножения
ghci> :i ^
(^) :: (Num a, Integral b) => a -> b -> a       -- Defined in ‘GHC.Real’
infixr 8 ^
ghci> :i *
type Num :: * -> Constraint
class Num a where
  ...
  (*) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 7 *
```
test

```hs
{--
В одном из прошлых заданий мы встречали тип `Result` и функцию `doSomeWork`
Функция `doSomeWork` возвращала результат своей работы и либо код ошибки в случае неудачи, либо `0` в случае успеха

data Result = Fail | Success
doSomeWork :: SomeData -> (Result, Int)

в случае успеха мы вынуждены возвращать некоторое значение, которое не несет никакой смысловой нагрузки

Используя функцию `doSomeWork`, определите функцию `doSomeWork'`
чтобы она возвращала код ошибки только в случае неудачи

Для этого необходимо определить тип `Result'`

определите instance `Show` для `Result'`
чтобы show возвращал "Success" в случае успеха
и "Fail: N" в случае неудачи, где `N` — код ошибки
--}
data Result' = ?
instance Show Result' where
    show = undefined
doSomeWork' :: SomeData -> Result'
doSomeWork' = undefined

-- reference
data Result = Fail | Success
doSomeWork :: SomeData -> (Result, Int)
processData :: SomeData -> String
processData = m . doSomeWork
    where m (r, err) = case r of
                        Fail -> "Fail: " ++ show err
                        Success -> "Success"

-- решение

-- при успехе кода нет, при ошибке есть код (сумма). Имена со штрихом во избежание коллизий
data Result' = Success' | Fail' Int

-- "Success" в случае успеха | "Fail: N" в случае неудачи, где `N` — код ошибки
instance Show Result' where
    show Success' = "Success"
    show (Fail' x) = "Fail: " ++ show x

doSomeWork' :: SomeData -> Result' -- при успехе кода нет, при ошибке есть код (сумма)
doSomeWork' = decode . doSomeWork where
    decode (tag, code) = case tag of
        Success -> Success'
        Fail -> Fail' code
```
test

### 4.2.7 constructor functions

Для удобства бывает полезно определить вспомогательные функции конструирующие некоторые значения типов
```hs
-- пример
data Shape = Circle Double | Rectangle Double Double
    deriving Show

-- вспомогательная ф. создающая значение типа, для удобства
square :: Double -> Shape
square x = Rectangle x x

-- есть проблема: такую ф. невозможно использовать в пат.мат.
isSquare (square _) = True -- так работать не будет
-- в пат.мат. можно использовать только конструкторы (вспомогательная ф. не является конструктором)

-- такая особенность (экспорта хелперов и прятания конструкторов) используется в случаях,
-- когда надо спрятать конструкторы (детали реализации) и дать удобные хелперы, часто с расширенной функциональностью.
-- для этого из модуля экспортируются только вспомогательные функции

-- example
ghci> import Data.Ratio
ghci> 2 % 3
2 % 3
ghci> 2 % 3 + 1 % 6
5 % 6
-- настоящий конструктор рациональных чисел выглядит по другому
-- а хелпер делает ряд проверок перед созданием значения
```
repl

```hs
-- Реализуйте функцию `isSquare`
-- проверяющую является ли фигура квадратом

data Shape = Circle Double | Rectangle Double Double
square :: Double -> Shape
square a = Rectangle a a
isSquare :: Shape -> Bool
isSquare = undefined

-- решение
-- шейп надо проверить на "квадратность"
-- шейп это сумма круга и прямоугольника, квадрат это прямоугольник с одинаковыми сторонами
data Shape = Circle Double | Rectangle Double Double
square :: Double -> Shape
square a = Rectangle a a
isSquare :: Shape -> Bool
isSquare (Rectangle a b) = abs (a - b) <= delta where delta = 0.000001
isSquare _ = False

ghci> isSquare $ square 3.2
True
```
test

```hs
{--
Целое число можно представить как список битов со знаком
Реализуйте функции сложения и умножения для таких целых чисел
считая, что младшие биты идут в начале списка.
Можно считать, что на вход не будут подаваться числа с ведущими нулями
--}
data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined

-- разбор
add :: Z -> Z -> Z
add = undefined
-- на входе тип-произведение из типов Sign (сумма из двух вариантов) и списка Bit (сумма из двух вариантов)
-- попытаемся реализовать требуемое через кодек в целые числа и банальные операции с целыми числами

-- особенности
{--
Пустой список бит = 0
"ведущих нулей не будет" = [0] не валидно.
--}

-- решение (альтернативное)
import Data.List
data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

add :: Z -> Z -> Z
add a b = toBinarySystem $ toDecimalSystem a + toDecimalSystem b

mul :: Z -> Z -> Z
mul a b = toBinarySystem $ toDecimalSystem a * toDecimalSystem b

bitToInt :: Bit -> Int
bitToInt One = 1
bitToInt Zero = 0

boolToBit :: Bool -> Bit
boolToBit True = One
boolToBit False = Zero

toDecimalSystem :: Z -> Int
toDecimalSystem (Z s bits) =
    case s of
        Plus -> x
        Minus -> -x
    where
        x :: Int
        x = foldr convert 0 (zip [0..] bits)

        convert :: (Int, Bit) -> Int -> Int
        convert (n, b) acc =
            acc + bitToInt b * 2^n

toBinarySystem :: Int -> Z
toBinarySystem v = Z sign bits
    where
        sign :: Sign
        sign = if v >= 0 then Plus else Minus
        
        bits :: [Bit]
        bits = [ boolToBit v | v <- bitsRes ]

        bitsRes :: [Bool]
        bitsRes = unfoldr (\x -> if x == 0 then Nothing else Just (odd x, x `div` 2)) (abs v)
```
test [test-bitz](./chapter-4.2/test-bitz.hs)

### 4.2.10 ленивый пат.мат., `~` (irrefutable pattern)

Неопровержимые образцы (irrefutable pattern): сравнение с ними всегда успешно.
Это подчерк (дырка) или связывание переменной (с именем) без разбора структуры или сравнения значений.
А есть ленивые образцы, со значком `~`.
Что означает: это неопровержимый образец, переходи сразу в правую часть уравнения.
```hs
-- example, работает норм
fromMaybe (Just x) = x
fromMaybe Nothing = error "nothing"

-- с ленивым образцом:
fromMaybe ~(Just x) = x -- выполняется всегда (падает в правой части)
fromMaybe Nothing = error "nothing" -- сюда никогда не попадает

-- разберем полезное применение тильды (ранее была задачка, на обработку стримов через фолд, там это было полезно)
(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- две функции и пара. Первый эл. обработать первой ф., второй - второй. Вернуть пару из результатов работы функций.
(***) f g p = (f $ fst p, g $ snd p) -- собрали результ. пару из выходов переданных функций
-- тут иррефутабл паттерн, выполняется всегда

-- работает ок
ghci> succ *** pred $ (5,5)
(6,4)

-- возьмем другие функции
ghci> const 1 *** const 2 $ (5,5)
(1,2)

-- видно, что обработка переданной пары нам не нужна
-- реализация оператора `***` использует irrefutable pattern, поэтому пара и не форсируется (она просто не используется, спасибо ф. const)
ghci> (const 1) *** (const 2) $ (undefined, undefined)
(1,2)
-- т.е. функция "хорошо определена", она "нестрогая"
ghci> const 1 *** const 2 $ undefined
(1,2)

-- сравните с таким вариантом (пат.мат. пары -- проверка что это действительно пара)
(***) f g (x, y) = (f x, g y)

ghci> const 1 *** const 2 $ (5,5)
(1,2)
ghci> const 1 *** const 2 $ (undefined, undefined)
(1,2)
ghci> const 1 *** const 2 $ undefined -- а вот тут сломалось: пат.мат. форсировал вычисление `undefined` и упал
*** Exception: Prelude.undefined

-- это можно исправить добавив тильду, объявим "не форсируй этот пат.мат"
ghci> :{
ghci| (***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
ghci| (***) f g ~(x, y) = (f x, g y)
ghci| :}
-- и, поскольку фунции const не требуют второго аргумента, форсирования вычисления `undefined` не происходит
ghci> const 1 *** const 2 $ undefined
(1,2)
```
repl

> функция становится лучше определена. То есть в случае, когда зависимости от входных данных нет, 
функция на расходящемся входе возвращает разумный результат, а не расходится

https://wiki.haskell.org/Lazy_pattern_match

```hs
-- Пусть определена следующая функция

foo :: Bool -> Int
foo ~True = 1
foo False = 0

-- Что произойдет при вызове foo False?

-- ответ:
1
-- почему? irrefutable pattern, созданный при помощи тильды `~`.
-- вычисление пойдет по ветке на этом паттерне и редуцируется до `1`
```
test

## chapter 4.3, Синтаксис записей (record syntax)

https://stepik.org/lesson/5431/step/1?unit=1132

- Метки полей
- Использование синтаксиса записей
- Синтаксис записей и сопоставление с образцом

### 4.3.2 тип product, метки полей (tag), оператор амперсанд

Как в Xaskell описать структуру данных с полями (как в ООП: `struct Person {name: String, age: Int, ...}`).
Tuple? Неудобно.
Как произведение? Да
```hs
-- пример 1, неудачный
data Person = Person String String Int -- произведение имени, фамилии, возраста
-- тип = конструктор данных

-- зададим проекции типа, для удобства доступа
firstName (Person x _ _) = x
lastName (Person _ x _) = x
age (Person _ _ x) = x

-- пример 2, правильный, со специальным синтаксисом
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)
-- для каждого поля задана специальная метка поля, по которой будет сгенерирован геттер (проекция)

-- конструктор данных это функция трех параметров
ghci> :t Person
Person :: String -> String -> Int -> Person

-- проекция (геттер) это функция одного параметра
ghci> :t firstName
firstName :: Person -> String

ghci> john = Person "John" "Smith" 33
ghci> age john
33

-- тайпкласс Show по умолчанию выводит запись конструктора данных, пример:
ghci> john
Person {firstName = "John", lastName = "Smith", age = 33}

ghci> j = Person {firstName = "John", lastName = "Smith", age = 33}
ghci> j
Person {firstName = "John", lastName = "Smith", age = 33}

ghci> j == john
True

-- т.е. создание структуры можно записать как
john = Person "John" "Smith" 33
-- так и
john = Person {firstName = "John", lastName = "Smith", age = 33}
-- при этом порядок следования полей не важен (в отличие от безымянного конструирования)
john = Person {firstName = "John", age = 33, lastName = "Smith"}

-- есть оператор "амперсанд" для имитации доступа в стиле ООП
ghci> import Data.Function
ghci> :i (&)
(&) :: a -> (a -> b) -> b       -- Defined in ‘Data.Function’
infixl 1 & -- левая ассоциативность означает
-- возможность писать цепочки `so & si & field` для доступа к нижним уровням вложенности

ghci> john & age
33

-- забавно, раньше этот оператор называли "оператор евро" в пику "оператор доллар"
-- доллар: оператор "апплай" с пониженным приоритетом и правой ассоц.
ghci> :i $
($) :: (a -> b) -> a -> b       -- Defined in ‘GHC.Base’
infixr 0 $
-- понятно почему? ибо делает "наоборот", вернее делает flip $
-- кроме того, ассоциативность у доллара правая а у "евро" левая

{--
цепочка применения
x & h & g & f
эквивалентена
f $ g $ h $ x
Что тут происходит: делает применение h-к-x, к полученному применяет g, к полученному применяет f
Что в амперсандовой нотации можно прочитать как:
скармливает х в аш, полученное скармливает в же, полученное скармливает в эф. Как пайплайн
--}
```
repl

```hs
{--
Определите тип записи, который хранит элементы лога
Имя конструктора должно совпадать с именем типа,
и запись должна содержать три поля
- timestamp — время, когда произошло событие (типа UTCTime)
- logLevel — уровень события (типа LogLevel)
- message — сообщение об ошибке (типа String)

Определите функцию `logLevelToString`
возвращающую текстуальное представление типа `LogLevel`
и функцию `logEntryToString`
возвращающую текстуальное представление записи в виде
`<время>: <уровень>: <сообщение>`
Для преобразование типа `UTCTime` в строку используйте функцию `timeToString`
--}
import Data.Time.Clock
import Data.Time.Format
import System.Locale
timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"
data LogLevel = Error | Warning | Info

data LogEntry = undefined

logLevelToString :: LogLevel -> String
logLevelToString = undefined

logEntryToString :: LogEntry -> String
logEntryToString = undefined

-- решение
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.List (intercalate)

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString le = intercalate ": " [t,l,m]
                        where
                            t = timeToString $ timestamp le
                            l = logLevelToString $ logLevel le
                            m = message le

-- ------------------------------------------

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    logLevel  :: LogLevel,
    message   :: String
}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString log = concatMap ($ log) pattern where
    pattern = [timeToString . timestamp, sep, logLevelToString . logLevel, sep, message]
    sep = const ": "

-- ------------------------------------------------

import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.List (intersperse)

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString log =
    concat $ intersperse ": " [ f log | f <- [time, level, message]]
    where
        time = timeToString . timestamp
        level = logLevelToString . logLevel

```
test [logentry](./chapter-4.3/test-logentry.hs)

### 4.3.4 операции с именованными полями продуктов

Что нам доступно при записи продукта с тегами:
разные формы конструкторов, не полная инициализация, создание копии с модифицированным полем
```hs
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)

-- два разных вида создания переменной такого типа
john = Person "John" "Smith" 33 -- только в таком порядке
xavier = Person { age = 40, firstName = "Phideaux", lastName = "Xavier" } -- в любом порядке

-- допустимо указывать не все поля при создании
unknownBill = Person { firstName = "Bill" }
-- в таком случае будет предупреждение
ghci> unknownBill = Person { firstName = "Bill" }
<interactive>:33:15: warning: [-Wmissing-fields]
    • Fields of ‘Person’ not initialised:
        lastName :: String
        age :: Int
    • In the expression: Person {firstName = "Bill"}
      In an equation for ‘unknownBill’:
          unknownBill = Person {firstName = "Bill"}

-- переменная создалась, но работать с ней можно только обращаясь к инициализированному полю
ghci> unknownBill 
Person {firstName = "Bill", lastName = "*** Exception: <interactive>:33:15-43: Missing field in record construction lastName "

ghci> unknownBill & firstName -- так нормально
"Bill"

-- создадим копию билла и сравним с оригиналом (спойлер: не сможем)
ghci> unknownBill' = Person { firstName = "Bill" }
<interactive>:36:16: warning: [-Wmissing-fields] ...
-- облом, хотя они одинаковые. Доступ к неинициализированным полям роняет программу
ghci> unknownBill == unknownBill'
*** Exception: <interactive>:33:15-43: Missing field in record construction lastName

-- как модифицировать такие записи
updateAge :: Int -> Person -> Person
updateAge newAge person = person { age = newAge } -- copy с новым значением поля
-- будет создана копия персоны с новым значением возраста

ghci> xavier & age
40
ghci> updateAge 42 xavier 
Person {firstName = "Phideaux", lastName = "Xavier", age = 42}
ghci> xavier & age
40
```
repl

```hs
{--
Определите функцию `updateLastName person1 person2`
которая меняет фамилию `person2` на фамилию `person1`
--}
data Person = Person { firstName :: String, lastName :: String, age :: Int }
updateLastName :: Person -> Person -> Person
updateLastName = undefined

-- решение
data Person = Person { firstName :: String, lastName :: String, age :: Int }
updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName p1 }

-- alternative
data Person = Person { firstName :: String, lastName :: String, age :: Int }
updateLastName :: Person -> Person -> Person
updateLastName (Person {lastName = ln}) p2 = p2 {lastName = ln}
```
test

### 4.3.6 пат.мат. для записей с тегами

```hs
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)

-- как написать ф. получения полного имени персоны
name :: Person -> String
name p =
    firstName p ++ " " ++ lastName p

-- можно это сделать через пат.мат.
name (Person fn ln _) =
    fn ++ " " ++ ln -- надо знать, на каких позициях какие поля. Надо перечислить все поля

-- а можно выборочно по именам меток сделать пат.мат.
name (Person { lastName = ln, firstName = fn }) =
    fn ++ " " ++ ln -- перечисляем только то, что надо
```
repl

```hs
-- Допустим мы объявили тип
data Shape = Circle Double | Rectangle Double Double

-- Что произойдет при объявлении такой функции:
isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

-- решение: нормально скомпиляется и будет работать как ожидается (отличать прямоуг. от круга). Очевидно, параметры конструктора в
-- фигурных скобках опциональны, вплоть до полного отсутствия параметров.
-- Она компилируется и возвращает True, если на вход передается Rectangle, иначе она возвращает False
:{
data Shape = Circle Double | Rectangle Double Double
isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False
:}
ghci> isRectangle $ Rectangle 0 0
True
ghci> isRectangle $ Circle 0
False

ghci> isRectangle $ Rectangle{}
<interactive>:20:15: warning: [-Wmissing-fields]     • Fields of ‘Rectangle’ not initialised ...
True
```
test

```hs
{--
Определить функцию `abbrFirstName`
которая сокращает имя до первой буквы с точкой
то есть, если имя было "Ivan", то после применения этой функции оно превратится в "I."
Однако, если имя было короче двух символов, то оно не меняется
--}
data Person = Person { firstName :: String, lastName :: String, age :: Int }
abbrFirstName :: Person -> Person
abbrFirstName p = undefined

-- решение
data Person = Person { firstName :: String, lastName :: String, age :: Int }
abbrFirstName :: Person -> Person
abbrFirstName p = p { firstName = abbr } where
    abbr = if length fn < 2 then fn else head fn : "."
    fn = firstName p

-- можно оптимальнее, как-то так (не тестировал)
data Person = Person { firstName :: String, lastName :: String, age :: Int }
abbrFirstName :: Person -> Person
abbrFirstName p@Person {firstName = fn} = if length fn < 2 then p else p { firstName = head fn : "." }
```
test

## chapter 4.4, Типы с параметрами

https://stepik.org/lesson/5746/step/1?unit=1256
- Типы, параметризованные переменной типа
- Стандартные параметризованные типы
- Виды (кайнды)
- Строгий конструктор данных

### 4.4.2 конструктор типа

Допустим, есть потребность в типе данных, параметризованном другими типами.
Пример: в картографии есть Координата, ее параметры это Широта, Долгота.
Широта и долгота могут быть выражены через Double а могут через Int, если у нас сетка ячеек.
```hs
-- coords examples
data CoordD = CoordD Double Double
data CoordI = CoordI Int Int

{--
Вместо конкретики (инт, дабл), зададим параметр-полиморфный-тип aka переменная типа.
Реализация будет через некоторые полиморфные интерфейсы (вероятно, тайпкласса Num) и нам без разницы, на этом уровне абстракции,
что там "под капотом".

Тип данных (параметризован переменной типа а) = конструктор данных (параметризован двумя значениями "типа а")
Правильно так: конструктор типов, параметризован `a` = конструктор данных, параметризован двумя значениями типа `a`
--}
data Coord a = Coord a a
{--
Coord (слева) это не тип, это конструктор типов.
Конструктор типа: порождает тип, применяясь к типам.
Конструктор данных (справа): порождает выражения, применяясь к выражениям.
--}
ghci> :t Coord (3::Int) (4::Int)
Coord (3::Int) (4::Int) :: Coord Int -- сконструировано выражение типа `Coord Int`

ghci> :t Coord
Coord :: a -> a -> Coord a -- полиморфный конструктор, принимает два параметра типа `a`
-- конструирует выражение типа `Coord a`
-- `Coord a` это параметризованный тип, `Coord` 'это конструктор типов, еще говорят "функция над типами.
```
repl

```hs
{--
Реализуйте функции: `distance`
считающую расстояние между двумя точками с вещественными координатами
и `manhDistance`
считающую манхэттенское расстояние между двумя точками с целочисленными координатами
--}
data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance = undefined

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance = undefined

-- решение

-- расстояние между точками а и б на плоскости: let dist^2 = abx^2 + aby^2 in sqrt dist^2 where abx = a.x - b.x; aby = a.y - b.y
-- манх. расстояние:  is a metric in which the distance between two points is the sum of the absolute differences of their Cartesian coordinates
-- abs abx + abs aby where abx = a.x - b.x; aby = a.y - b.y

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord ax ay) (Coord bx by) = sqrt (abx^2 + aby^2) where
    abx = ax - bx
    aby = ay - by

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord ax ay) (Coord bx by) = abs abx + abs aby where
    abx = ax - bx
    aby = ay - by

```
test

```hs
{--
Плоскость разбита на квадратные ячейки
Стороны ячеек параллельны осям координат
Координаты углов ячейки с координатой `(0,0)` имеют неотрицательные координаты (т.е. ячейка 0,0 находится в положительном квадранте плоскости)
Один из углов этой ячейки имеет координату `(0,0)` (т.е. сетка начинается с 0)
С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек. (т.е. рассматриваем положительный квадрант)

Реализуйте функции `getCenter`
которая принимает координату ячейки и возвращает координату ее центра

и функцию `getCell`
которая принимает координату точки и возвращает номер ячейки в которой находится данная точка

В качестве первого аргумента обе эти функции принимают ширину ячейки
--}
data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter = undefined

getCell :: Double -> Coord Double -> Coord Int
getCell = undefined

-- решение
-- Раз у нас положительный квадрант, все числа положительные и счет начинается с 0.
-- Центр ячейки это два числа, х и у: (количество-ячеек * размер-ячейки - половина-размера-ячейки).
-- Номер (координаты) ячейки это два числа, х и у: (целая-часть размер-проекции-вектора / размер-ячейки)

data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (center x) (center y) where
    center n = (size * fromIntegral (n + 1)) - (size / 2) -- first cell has number 0

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (cell x) (cell y) where
    cell proj = floor (proj / size)

-- alternative

import Data.Function
data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x y) = (Coord `on` ((*w) . (+0.5) . fromIntegral)) x y

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = (Coord `on` (floor . (/ w))) x y

-- пояснения
ghci> :i on
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c -- Defined in ‘Data.Function’
infixl 0 `on`
-- левоассоциативный оператор с низшим приоритетом
-- x on y -- означает: ф. b -> b -> c 'on' ф. a -> b применить к двум a, чтобы получить один c
-- справа трансформер из a -> b, он обрабатывает входные аргументы функции x 'on' y.
-- слева трансформер из двух b в один c, он дает финальный результат.
(Coord `on` (floor . (/ w))) x y -- означает: к входным х и у применить деление на дабл-ю и округление-вниз
-- (оператор . это композиция ака декоратор).
-- потом НА этом применить конструктор Coord. Получим выходной Coord на координатах разделенных-на-ширину-и-округленных-вниз
```
test

### 4.4.5 Стандартные параметризованные типы, конструкторы

Некоторые стандартные типы тоже параметризованы.
Например, список.
При этом, используются разные виды записи конструкторов (инфиксные, префиксные).
Слева: конструктор типа, принимает тип и выдает тип.
Справа: конструктор данных, принимает значение (выражение) и выдает значение (выражение).
```hs
twice :: a -> [a] -- конструктор (типов) списков с значениями произвольного типа
twice x = [x, x] -- конструктор данных

twice :: a -> [] a -- конструктор записан в "аппликативном" стиле (конструктор `[]` применить к выражению `a`)
-- так бывает удобно в случаях, когда надо использовать "частичное применение" конструктора
twice x = [x, x]

ghci> :t twice 3 -- создание списка чисел
twice 3 :: Num a => [a]

ghci> :t twice 'x' -- создание списка чаров
twice 'x' :: [Char]

-- с кортежами очень похоже
thrice :: a -> (,,) a a a -- конструктор типов, в префиксной форме
thrice x = (,,) x x x -- конструктор данных в префиксной форме

ghci> :t thrice -- репл печатает тип в обычном (инфиксном) стиле
thrice :: a -> (a, a, a)

-- функциональная стрелка, это тоже (параметризованный) конструктор, можно записать в префиксном виде
a -> b
(->) a b

-- example
id :: (->) a a -- двух-параметрический конструктор типа
id x = x

-- другой пример функц. стрелки, ф. const может быть записана так:
k :: a -> b -> a
k x y = x
-- или так:
k :: a -> (b -> a) -- ассоциативность стрелки правая
k x y = x

k :: (->) a (b -> a) -- вытащили в префиксный стиль первую стрелку
k x y = x

k :: (->) a ((->) b a) -- вытащили в префиксный стиль вторую стрелку
k x y = x

-- еще два стандартных типа

data Maybe a = Nothing | Just a -- сумма "ничего" и `a`
-- одно-параметрический конструктор типа, имеет два конструктора данных
-- Maybe Int (конструктор типа) дает тип для выражения Just 42 (конструктор данных)

data Either a b = Left a | Right b -- сумма левого (ошибка) и правого (данных)
-- двух-параметрический тип (конструктор типов) Either, с двумя конструкторами данных.
-- Зачем такой? Конструкторы данных используются как маркеры (в пат.мат.) для определения ветвлений.

ghci> :t Left "abc"
Left "abc" :: Either String b
-- второй параметр остался не определен, что не важно, в "левой" части туда доступа нет

ghci> :t Right True
Right True :: Either a Bool
-- аналогично и для "правой части", правый параметр связался типом бул; левый параметр не связан

-- Either это, по сути, расширенный Maybe.
-- Левая часть используется для поддержки обработки ошибок.

-- использование Either на примере ф. roots
-- ранее было c использованием кастомного типа Roots
data Roots = Roots Double Double | None deriving Show
roots :: Double -> Double -> Double -> Roots
roots a b c
    | discr >= 0    = Roots x1 x2
    | otherwise     = None
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c

-- теперь это можно сделать через встроенный Either
roots :: Double -> Double -> Double -> Either String (Double, Double)
roots a b c
    | discr >= 0    = Right (x1, x2)
    | otherwise     = Left "Negative discriminant"
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c
```
repl

```hs
{--
Реализуйте функцию, которая ищет в строке первое вхождение символа
который является цифрой
возвращает `Nothing`, если в строке нет цифр
--}
import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit = undefined

-- решение

import Data.Char(isDigit)
findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs
-- findDigit = Data.List.find isDigit
```
test

```hs
{--
Реализуйте функцию `findDigitOrX`
использующую функцию `findDigit` -- реализовывать не нужно

`findDigitOrX` должна находить цифру в строке
а если в строке цифр нет, то она должна возвращать символ 'X'

Используйте конструкцию `case`
--}
import Data.Char(isDigit)
findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX = undefined

-- решение: нужна конвертация Maybe в символ

import Data.Char(isDigit)
findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX = decode . findDigit where -- без case of
    decode Nothing = 'X'
    decode (Just c) = c

-- case of

import Data.Char(isDigit)
findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX s = case findDigit s of
    Nothing     -> 'X'
    (Just c)    -> c

```
test

```hs
{--
`Maybe` можно рассматривать как простой контейнер
например, как список длины 0 или 1

Реализовать функции `maybeToList` и `listToMaybe`
преобразующие `Maybe a` в `[a]`

и наоборот (вторая функция отбрасывает все элементы списка, кроме первого
--}
maybeToList :: Maybe a -> [a]
maybeToList = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

-- решение

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- альтернатива

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] (:[]) -- default value = [], function f=(:[]), parameter Maybe a: if Nothing -> default, otherwise -> f a

listToMaybe :: [a] -> Maybe a
listToMaybe = foldr ((Just .) . const) Nothing -- на пустом = Ничего, на списке = Только первый-элемент -- работает на бесконечности

```
test

```hs
{--
Реализуйте функцию `parsePerson`
которая разбирает строки и возвращает либо результат типа `Person`, либо ошибку типа `Error`

`firstName = John\nlastName = Connor\nage = 30`

Строка, которая подается на вход, должна разбивать по символу '\n' на список строк каждая из которых имеет вид `X = Y`.
Если входная строка не имеет указанный вид, то функция должна возвращать `ParsingError`

Если указаны не все поля, то возвращается `IncompleteDataError`

Если в поле `age` указано не число, то возвращается `IncorrectDataError str`, где `str` — содержимое поля `age`

Если в строке присутствуют лишние поля, то они игнорируются
--}
data Error = ParsingError | IncompleteDataError | IncorrectDataError String
data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson = undefined

-- решение
-- лишние поля: не ошибка, недостаточно полей: ошибка
-- неправильный тип возраста: ошибка
-- строка разбивается по `\n` в список строк. Каждая строка в списке разбивается по `=` на пару (имя, значение)
-- если строка не разбивается по ` = ` то это ошибка
-- из трех пар по именам (firstName, lastName, age) собирается запись Person
-- необходимо учесть возможный порядок возникновения ошибок: сначала возможна ParsingError, если прошли, то возможна IncompleteDataError, ...

import Data.List (stripPrefix)
import Control.Arrow (first)
import Data.Maybe (isNothing, fromJust)
import Text.Read ( readMaybe )

-- https://hackage.haskell.org/package/extra-1.7.14/docs/src/Data.List.Extra.html#stripInfix
stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix needle haystack | Just rest <- stripPrefix needle haystack = Just ([], rest)
stripInfix needle [] = Nothing
stripInfix needle (x:xs) = case stripInfix needle xs of
  Just (prefix, next) -> Just (x:prefix, next)
  Nothing -> Nothing

getKv :: [(String, String)] -> String -> Maybe String
getKv [] _ = Nothing
getKv ((k,v):kvs) key = if k == key then Just v else getKv kvs key

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson s = let
    mkvs = map (stripInfix " = ") $ lines s
    kvs = map fromJust mkvs
    get = getKv kvs
  in if any isNothing mkvs then Left ParsingError
     else case (get "firstName", get "lastName", get "age") of
       (Just fn, Just ln, Just ageStr) ->
         case readMaybe ageStr of
           Nothing -> Left $ IncorrectDataError ageStr
           Just ageVal -> Right Person {firstName=fn, lastName=ln, age=ageVal}
       _ -> Left IncompleteDataError

----------------------------------------------------------------

import Data.List
import Data.Maybe
import Text.Read ( readMaybe )

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

trim :: String -> String
trim = f . f
   where f str = reverse $ Data.List.dropWhile (==' ') str

splitToPairs :: [String] -> [(String, String)]
splitToPairs [] = []
splitToPairs (x:xs) = (trim name, trim (if "" == value then [] else tail value) ): splitToPairs xs
    where (name, value) = break (=='=') x

parsePerson :: String -> Either Error Person
parsePerson txt 
    | values == []                = Left IncompleteDataError
    | any ((=="").snd) values     = Left ParsingError
    | any (==Nothing) [fn, ln, a] = Left IncompleteDataError
    | age' == Nothing             = Left (IncorrectDataError $ fromJust a)
    | otherwise         = Right (Person {firstName = fromJust fn, lastName = fromJust ln, age = fromJust age'})
    where
        values = splitToPairs $ lines txt
        fn     = lookup "firstName" values
        ln     = lookup "lastName" values
        a      = lookup "age" values
        age'   = readMaybe (fromJust a) :: Maybe Int

```
test [parse_person.hs](./chapter-4.4/test-parse_person.hs)

### 4.4.10 kind (vs type)

При появлении конструкторов типов мы получили "применение" функций к типам.
В отличие от "применения" функций к выражениям, обладающим типами, мы здесь переходим на уровень выше,
применение ф. к типам.
Как (в языке) контролировать (проверять) правильность (корректность) такого "применения"?

На уровне применения ф. к выражению, мы пользуетмся типами, для проверки корректности.
А на уровне выше (применить ф. к типу)?
Над системой типов создать еще одну систему типов.

Входят "вид" aka "kind" выражения (типа).
Над выражениями есть типы, над типами есть кайнды.

Правильный (годный к использованию) кайнд: одна звезда `*`.
Если кайнд это стрелочка (стрелочки) между звездами, это кайнд параметризованного типа и параметры надо указать,
перед тем как использовать полученный (из конструктора) тип в дальнейших упражнениях с типами
```hs
-- по аналогии с командой :type, есть команда :kind
ghci> :type 'c' -- покажет тип выражения
'c' :: Char
ghci> :type succ 'c'
succ 'c' :: Char
ghci> :t succ
succ :: Enum a => a -> a -- стрелочный тип, ф. одного аргумента

ghci> :kind Char -- покажет кайнд типа
Char :: * -- простой кайнд, без стрелочек, одна звезда, годен к использованию в выражениях

ghci> :k Char -> Int
Char -> Int :: * -- простой кайнд, без стрелочки, годен

ghci> :kind 'c' -- кайнда у выражения (не описывающего типы) нет
error:    Illegal type: ‘'c'’ Perhaps you intended to use DataKinds

-- простые, встроенные типы имеют кайнд "звездочка": `*`

-- У параметризованных типов (с конструкторами типов) кайнд уже сложнее
-- содержит функциональные стрелки
-- Конструктор типа (мэйби) ожидает параметр с кайндом `*` и возвращает тип с кайндом `*`.
ghci> :k Maybe -- один параметр
Maybe :: * -> *

ghci> :k [] -- один параметр
[] :: * -> *

ghci> :k Either -- два параметра, оба с кайндом `*`, т.е. "простых"
Either :: * -> * -> *

-- конструктор типа вернул тип с кайндом `*`, т.е. сделал тип годный к использованию в выражениях
ghci> :k Maybe Int
Maybe Int :: *

-- проверка кайндов позволяет ловить ошибки (попытка дать два параметра в конструктор типов)
ghci> :k Maybe Int Char
error:     • Expected kind ‘* -> k0’, but ‘Maybe Int’ has kind ‘*’
-- kind система дает контроль арности конструкторов типов
ghci> :k Either Int Char
Either Int Char :: *

-- пример: у конструктора типа списка кайнд звездочка-в-звездочку,
-- убираем одну звездочку задав параметр конструктору списка, получаем кайнд одна звезда.
ghci> :k []
[] :: * -> *
ghci> :k [] Int -- префиксная запись конструктора типа
[] Int :: *
ghci> :k [Int] -- инфиксная запись конструктора типа
[Int] :: *

-- Теперь полученный тип можно использовать при построении другх типов, где есть кайнд звезда-в-зведду, в качестве параметра
ghci> :k Maybe [Int]
Maybe [Int] :: *
-- сравните с
ghci> :k Maybe []
error:    • Expecting one more argument to ‘[]’        Expected a type, but ‘[]’ has kind ‘* -> *’

-- функциональная стрелка тоже имеет кайнд, два параметра (две звезды на входе)
ghci> :k (->)
(->) :: * -> * -> *
-- соответственно
ghci> :k (->) Int [] -- такой стрелочный тип невозможен, список требует параметр
error:    • Expecting one more argument to ‘[]’      Expected a type, but ‘[]’ has kind ‘* -> *’
-- но закрыв лишнюю звездочку (указав параметр списка) мы получим валидный кайнд стрелочного типа, с одной звездой, как у встроенных типов
ghci> :k (->) Int [Char] -- тип описывает множество функций из инта в строку и имеет кайнд `*`
(->) Int [Char] :: *

-- другие примеры
ghci> :k (,) -- кайнд пары, пара требует два параметра (также как и ф. стрелка)
(,) :: * -> * -> *
-- тройка - три параметра
ghci> :k (,,)
(,,) :: * -> * -> * -> *
```
repl

```hs
-- Укажите вид конструктора типов `Either (Maybe Int)`

-- решение
-- ийзер: хочет два параметра, т.е. это будет две звезды на входе, одна на выходе
-- одну звезду мы закрыли `Maybe Int`, остается еще один параметр (кайнд звезда-в-звезду)

ghci> :kind Either (Maybe Int)
Either (Maybe Int) :: * -> *
```
test

```hs
-- Исправьте ошибку в приведенном коде

eitherToMaybe :: Either a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

-- решение: Either хочет два параметра, указан один.
-- Логика подсказывает, что мы интересуемся "эффектом", т.е. ошибкой (Left), отбрасывая результат (Right)
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing
```
test

```hs
-- Укажите все выражения, имеющие вид `*`
-- т.е. типы, не требующие параметра в конструктор типа
Either (Int -> Int) Maybe -- нет, мэйби хочет параметр
(Maybe Int, Either (Int -> (Char, Char)) Int) -- да, это пара где все типы определены
Maybe -> Int -- нет, мейби не определен (или стрелочка не определена, хз)
Either (Int -> (,)) Int -- нет, пара не определена
Maybe (Int -> Either Int Int) -- да, мейби для функции
Int -> Int -- да, функция
Nothing -- нет, это не тип
Maybe Int -> Int -- да, мейби функции
Either True False  -- нет, тру фолс это не типы
```
test

### 4.4.14 строгие конструкторы (лень, форсирование вычислений)

Конструкторы: это барьеры вычислений, обеспечивают "лень", являются WHNF.
При создании объекта (в конструкторе данных) вычисления параметров не происходят.

Можно форсировать вычисления (дальнейшую нормализацию).
Для этого есть флаги "строгости".

Строгие конструкторы ведут к повышению эффективности (хотя и снижают определенность функций).
Поэтому, если есть уверенность в сходимости параметров конструктора (нет преимуществ в ленивости конструктора),
можно (нужно) делать его строгим.

Заодно мы уточнили соглашения Хаскел про имена конструкторов:
что конструктор должен начинаться с большой буквы. Или с двоеточия `:`.

```hs
data CoordLazy a = CoordLazy a a deriving Show -- обычный, ленивый конструктор
-- тут нет вычисления выражений, переданных в параметрах `a`

-- чтобы форсировать вычисления выражений в `a`, надо задать флаги строгости
data CoordStrict a = CoordStrict !a !a deriving Show -- строгий конструктор, форсирование выражений в `a`

-- посмотрим, как это работает
getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x

getXStrict :: CoordStrict a -> a
getXStrict (CoordStrict x _) = x

-- нормальная работа геттера, не видим разницы между ленивым и строгим конструкторами
ghci> getXLazy $ CoordLazy 3 5
3
ghci> getXStrict  $ CoordStrict  3 5
3
-- демонстрируем разницу: вторая координата лениво не вычисляется, строго - вычисляется
ghci> getXLazy $ CoordLazy 3 undefined
3
ghci> getXStrict  $ CoordStrict  3 undefined
*** Exception: Prelude.undefined

-- Data.Complex, Data.Ratio
-- у них строгие конструкторы
2 :+ 5 -- конструктор комплексного числа

data Complex a = !a :+ !a
data Ratio a = !a :% !a
-- инфиксный конструктор, должен начинаться с `:`
```
repl

```hs
-- Допустим тип `Coord` определен следующим образом
data Coord a = Coord a !a -- n.b. ленивый первый; строгий второй параметр, нормализуется в конструкторе (WHNF)

-- Пусть определены следующие функции
getX :: Coord a -> a
getX (Coord x _) = x -- второй параметр игнор, возвращ. первый, форсированный до WHNF

getY :: Coord a -> a
getY (Coord _ y) = y -- второй параметр пат.мат. до WHNF. возвр. второй

-- Какие из следующих вызовов  вернут число 3?
getY (Coord 3 7)                -- нет, второй 7
getX (Coord 3 3)                -- да, первый 3
getX (Coord undefined 3)        -- нет, сломается на пат.мат.
getX (Coord undefined undefined) -- нет, сломается на пат.мат.
getY (Coord undefined 3)        -- да, второй 3
getY undefined                  -- нет, полустрогий конструктор сломается
getY (Coord 3 undefined)        -- нет, строгий второй сломается
getX (Coord 3 undefined)        -- нет, строгий второй сломается
```
test

## chapter 4.5, Рекурсивные типы данных

https://stepik.org/lesson/7009/step/1?unit=1472

- Рекурсия в определении типа
- Представление выражений

### 4.5.2 рекурсивный конструктор данных

По аналогии с рекурсивным определением функции (вызов ее самой в правой части уравнения),
могут быть определены типы данных.
В конструкторе данных можно вызывать конструктор этого типа. Рекурсивное определение конструктора (это просто функция).

Список: это рекурсивный тип данных, два констуктора.
```hs
-- встроенный тип, без Большой буквы в названии
-- два конструктора, второй использует рекурсию для указания, что в конструктор надо дать два параметра: голову и список
data [] a = [] | a : ([] a) -- в префиксном стиле
data [a] = [] | a : [a] -- в инфиксном стиле

-- более традиционная запись, не встроенный тип а пользовательский
data List a = Nil | Cons a (List a) -- пустой список или голова и список

Cons 'c' Nil -- список Char из одного элемента
let bc = Cons 'b' (Cons 'c' Nil) -- список Char из двух элементов
Cons 'a' bc -- список из трех элементов
```
repl

```hs
https://stepik.org/lesson/7009/step/3?unit=1472
TODO
{--
Тип `List`, определенный ниже, эквивалентен определению списков из стандартной библиотеки
в том смысле, что существуют взаимно обратные функции, преобразующие `List` a в `[a]` и обратно

Реализуйте эти функции
--}
data List a = Nil | Cons a (List a)
fromList :: List a -> [a]
fromList = undefined
toList :: [a] -> List a
toList = undefined
```
test

```hs
https://stepik.org/lesson/7009/step/4?unit=1472
TODO
{--
Рассмотрим еще один пример рекурсивного типа данных
data Nat = Zero | Suc Nat

Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее
мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления

Мы можем написать функцию, которая преобразует Nat в Integer следующим образом
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал
--}
data Nat = Zero | Suc Nat
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
add :: Nat -> Nat -> Nat
add = undefined
mul :: Nat -> Nat -> Nat
mul = undefined
fac :: Nat -> Nat
fac = undefined
```
test

```hs
https://stepik.org/lesson/7009/step/5?unit=1472
TODO
{--
Тип бинарных деревьев можно описать следующим образом:
data Tree a = Leaf a | Node (Tree a) (Tree a)

Реализуйте функцию `height`
возвращающую высоту дерева

и функцию `size`,
возвращающую количество узлов в дереве (и внутренних, и листьев)

Считается, что дерево, состоящее из одного листа, имеет высоту 0
--}
data Tree a = Leaf a | Node (Tree a) (Tree a)
height :: Tree a -> Int
height = undefined
size :: Tree a -> Int
size = undefined
```
test

```hs
https://stepik.org/lesson/7009/step/6?unit=1472
TODO
{--
Теперь нам нужно написать функцию `avg`
которая считает среднее арифметическое всех значений в дереве
И мы хотим, чтобы эта функция осуществляла только один проход по дереву

Это можно сделать при помощи вспомогательной функции,
возвращающей количество листьев и сумму значений в них

Реализуйте эту функцию
--}
data Tree a = Leaf a | Node (Tree a) (Tree a)
avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go = undefined
```
test

### 4.5.7 рекурсивный тип "арифметические выражения"

Expression type
```hs
-- инфиксные конструкторы, ибо начинаются с двоеточия `:`
infixl 6 :+:
infixl 7 :*:
-- тип-сумма, не-параметризованный, три конструктора: значение, сумма-двух-выражений, произведение-двух-выражений
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr -- | Expr :-: Expr ...
    deriving (Show, Eq)

expr1 = Val 2 :+: Val 3 :*: Val 4
expr2 = (Val 2 :+: Val 3) :*: Val 4

-- равенство (бай дефолт) реализовано через ставнение структур
(Val 2 :+: Val 2) == Val 4 -- False

-- чтобы получить ожидаемый (арифметика) результат, надо сначала вычислить выражение (редуцировать до финального значения)
eval :: Expr -> Int -- тип-сумма, надо пат.мат. на три конструктора
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

-- Однако, структурная запись выражений используется обычно для другого.
-- Например, для описания некоторой алгебры, скажем "дистрибутивность умножения относительно сложения"
-- для раскрытия скобок, например
expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e
-- увы, эта ф. не раскрывает все возможные подвыражения, не все скобки будут раскрыты
-- видимо, нет (a + b) + c; (a * b) * c; (a* b) + c и их перевертыши
```
repl

В исходниках GHC
- https://github.com/ghc/ghc/blob/58bbb40ba23860df2ede1275493ef32ba69a2083/compiler/prelude/primops.txt.pp#L184
- https://github.com/ghc/ghc/blob/58bbb40ba23860df2ede1275493ef32ba69a2083/libraries/base/GHC/Num.hs#L67

```hs
https://stepik.org/lesson/7009/step/8?unit=1472
TODO
{--
Исправьте определение функции `expand`
так, чтобы она, используя дистрибутивность
(а также, возможно, ассоциативность и коммутативность)
всегда возвращала значение, эквивалентное данному
и являющееся суммой произведений числовых значений

Например
expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))

Примечание. Скобки в ответе могут быть расставлены по-другому или вообще отсутствовать,
поскольку сложение ассоциативно. Слагаемые могут идти в другом порядке, поскольку сложение коммутативно
--}
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e

```
test



Grep `TODO` and fix it, before moving to the next step.
