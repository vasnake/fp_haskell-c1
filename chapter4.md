# FP Haskell, chapter 4, data types

[Функциональное программирование на языке Haskell / Денис Москвин / stepik](https://stepik.org/course/75/syllabus?next=)

## chapter 4.1, Типы перечислений (перечисляемые типы)

https://stepik.org/lesson/4916/step/1?unit=1082

Пользовательские типы данных без параметров, с перечислимым количеством конструкторов.

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

## chapter 4.2 Sum, Product types


Grep `TODO` and fix it, before moving to the next step.
