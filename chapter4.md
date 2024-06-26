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

-- решение:

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

```
test

```hs
-- Исправьте ошибку в приведенном коде

eitherToMaybe :: Either a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

-- решение:

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
В конструкторе данных можно сослаться на конструктор типа (правая часть может ссылаться на левую).

Пример: список, это рекурсивный тип данных, два констуктора.
```hs
-- как можно было бы самому определить список: голова и хвост
-- не встроенный тип а пользовательский
data List a = Nil | Cons a (List a) -- тип-данных-сумма: пустой список или голова и список
-- `Cons a (List a)` -- это конструктор данных, принимающий два параметра, первый типа `a` и второй типа `List a`

Cons 'c' Nil -- список Char из одного элемента (два параметра, чар и список)
let bc = Cons 'b' (Cons 'c' Nil) -- список Char из двух элементов
Cons 'a' bc -- список из трех элементов

-- встроенный тип, без Большой буквы в названии, выглядит так же
-- два конструктора, второй использует рекурсию
data [] a = [] | a : ([] a) -- в префиксном стиле
data [a] = [] | a : [a] -- в инфиксном стиле
```
repl

```hs
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

-- решение:

```
test

```hs
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

-- решение: подозреваю, что ожидается реализация `toNat :: Integer -> Nat` и уже через нее операции с натами.

```
test

```hs
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

-- решение

```
test

```hs
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

-- решение

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

-- решение

```
test [test-expr](./chapter-4.5/test-expr.hs)

## chapter 4.6, Синонимы и обертки для типов

https://stepik.org/lesson/7602/step/1?unit=1473

- Синонимы типа
- Объявление newtype
- Класс типов Monoid
- Прочие моноиды
- Эндоморфизмы как моноиды

### 4.6.2 type keyword, алиас типа

Для удобства,
как задать краткий синоним сложного конструктора типов: ключевое слово `type`.
Важно: для алиаса остаются доступны инстансы тайпклассов от оригинала.
```hs
-- Пример: объявлен синоним `String` для типа (конструктора типа) `[Char]`
type String = [Char]

-- теперь алиас можно использовать вместо оригинала
allUpper :: String -> Bool
allUpper = all isUpper

-- кастомные синонимы делаются также
type IntegerList = [Integer]

-- пример использования
sumSquares :: IntegerList -> Integer
sumSquares = foldr1 (+) . map (^2)

-- все существующие реализации тайпклассов для оригинального типа остаются доступны для алиаса

-- параметризованные синонимы типов: возможно, пример: двух-параметрический оператор над типами
type AssocList k v = [(k, v)] -- тип: список пар, параметризован двумя параметрами
-- отдельно задан тип "ключа" и отдельно тип "значения"

-- пример использования
lookup :: (Eq k) => k -> AssocList k v -> Maybe v
lookup _ [] = Nothing
lookup key ((k, v) : pairs)
    | key == k  = Just v
    | otherwise = lookup key pairs

-- рассмотрим частичное применение параметризованных синонимов
-- partial apply

-- для примера возьмем коллекцию-мапку
import qualified Data.Map as Map -- упорядоченная по ключам коллекция пар 
-- https://hoogle.haskell.org/?hoogle=Data.Map&scope=set%3Ahaskell-platform

-- частично примененный конструктор типа, получили "тип" (алиас) с одним параметром (второй связан с Int)
type IntMap = Map.Map Int -- сократили три слова в одно, удобно
-- мапка, где ключи всегда Int

ghci> :k Map.Map
Map.Map :: * -> * -> * -- две стрелочи: два параметра конструктора типов

ghci> :k IntMap
IntMap :: * -> * -- одна стрелочка: один параметр конструктора типов
```
repl

extra (comments)
```hs
newtype Name = Name String
newtype Email = Email String

showNameEmail :: Name -> Email -> String
showNameEmail (Name name) (Email email) = "name: " ++ name ++ ", email: " ++ email

n = Name "Andrey"
e = Email "someemail@somewhere.org"
showNameEmail n e

-- https://wiki.haskell.org/Phantom_type
-- вариант с фантомными типами выглядит лучше из за возможности добавить информацию о валидации на уровне типов

newtype TextFld a = TextFld String
data Name
data Email

showNameEmail :: TextFld Name -> TextFld Email -> String
showNameEmail (TextFld name) (TextFld email) = "name: " ++ name ++ ", email: " ++ email

n = TextFld "Andrey" :: TextFld Name
e = TextFld "someemail@somewhere.org" :: TextFld Email
showNameEmail n e

{--
 экспортируя только функцию-конструктор textFld, 
 мы получаем ситуацию в которой пользователь не может создать валидированный тип без обращения к функциям библиотеки
--}

newtype TextFld a b = TextFld { getText::String } deriving Show

data Validated
data Unvalidated

data Name
data Email

textFld :: String -> TextFld a Unvalidated
textFld text = TextFld text

showNameEmail :: TextFld Name a -> TextFld Email a -> String
showNameEmail (TextFld name) (TextFld email) = "name: " ++ name ++ ", email: " ++ email

sanitize :: TextFld a Unvalidated -> TextFld a Validated
sanitize (TextFld text) = TextFld (filter isAlphaNum text)

n = textFld "Andrey" :: TextFld Name Unvalidated
e = TextFld "someemail@somewhere.org" :: TextFld Email Unvalidated
```
extra

```hs
-- Пусть синоним типа `Endo` определен следующим образом
type Endo a = a -> a -- эндоморфизм

-- Выберите из списка типы, эквивалентные
Endo (Endo Int) -- эндоморфизм эндоморфизмов

-- решение

```
test

### 4.6.4 newtype keyword

Если `type` это алиас типа (алиас типа `=` конструктор типа),
то `newtype` это враппер "дизайн-тайм" над типом с единственным конструктором данных (враппер `=` конструктор данных).
При этом, конструктор данных валидный `iff` у него один параметр.

В рантайме все `newtype` развернуты в свои определения.
Если у типа более одного конструктора, то в `newtype` его не завернуть.
Если у конструктора более одного параметра или нет параметров, то в `newtype` его не завернуть.

`newtype` это псевдо-тип (эфемерный тип) для единственного конструктора данных с единственным параметром.

Для `newtype` отбрасываются все существующие инстансы тайпклассов для завернутого типа (для `type` сохраняются).
Т.е. мы можем (хотим) определить свою реализацию тайпкласса для некоторого типа, поэтому используем `newtype` для этого типа.

Это как взять существующий тип и сказать: нет, вот новый тип.
Не смотрите, что под капотом китайский мотор, это русский "москвич".

Поведние похоже на таковое при определении типа с `data`, но есть отличия:
ньютайп более ленив и гарантирует (ровно один конструктор с ровно одним параметром)

```hs
-- обертка = конструктор
newtype IntList = IList [Int] -- определили список интов как новый тип, с потерей всех тайпклассов для списка интов
let example = IList [1,2,3]
-- теперь нам надо самостоятельно определять реализацию (инстансы) тайпклассов, например: Show
ghci> example 
<interactive>:7:1: error:    • No instance for (Show IntList) arising from a use of ‘print’

-- определили реализацию тайпкласса
newtype IntList = IList [Int] deriving Show
ghci> example
IList [1,2,3]

-- newtype vs data, с первого взгляда одинаковое поведение
data IntList = IList [Int] deriving Show
{--
1) `newtype` гарантированно имеет ровно один конструктор с ровно одним параметром:
в рантайме можно избавиться от обертки.
`data` конструктор может быть не один, с разным количеством параметров.
Конструкторы нужны в пат.мат. и они нужны в рантайм.

2) newtype типы более ленивы чем data типы.
--}

-- иллюстрация большей ленивости
data IntList = IList [Int] deriving Show
ignore :: IntList -> String
ignore (IList _) = "Hello" -- форсирование вычисления аргумента

ghci> ignore undefined -- дата форсировала вычисление андефайнед в пат.мат.
"*** Exception: Prelude.undefined ... "

newtype IntList = IList [Int] deriving Show
ignore :: IntList -> String
ignore (IList _) = "Hello" -- нет форсирования вычисления аргумента, компилятор знает про гарантию одного одно-параметрического конструктора

ghci> ignore undefined -- а ньютайп выкинул обертку и не стал форсировать андефайнед
"Hello"

-- примеры

-- параметризованный ньютайп Id: прозрачный враппер
-- можем использовать параметризацю ньютайпа, можем использовать метки полей
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Ord)

ghci> :kind Identity 
Identity :: * -> * -- кайнд конструктора типа: одно-параметрический тип

ghci> :type Identity 
Identity :: a -> Identity a -- тип конструктора типа: одно-параметрическая ф.

ghci> :t runIdentity 
runIdentity :: Identity a -> a -- тип геттера: из "коробочки-с-а" в "просто а"
```
repl

```hs
-- Выберите корректные объявления типов

newtype A a b = A a b -- нет, два параметра
newtype A a = A a -- да один параметр
newtype A = A A A -- нет, два параметра
newtype A a b = A a -- да, один параметр
newtype A = A -- нет, нет параметра
newtype A a = A a a -- нет, два параметра
newtype A = A A -- да, один параметр
newtype A a b = A b -- да, один параметр
newtype A a = A -- нет, нет параметров
newtype A = A a  -- нет, свободный параметр (нет в скоупе)

-- ньютайп это эфемерный враппер с одним конструктором данных у которого один параметр

```
test

### 4.6.6 type-class Monoid

Есть тайп-класс, для реализаций которого часто используется keyword `newtype`:
это моноид (нейтральный элемент и операция склеивания двух значений в моноиде).

Множество и заданная на нём Ассоциативная Бинарная Операция (с нейтральным элементом).
```hs
class Monoid a where -- одно-параметрический интерфейс: тайпкласс
    mempty :: a -- zero or neutral element
    mappend :: a -> a -> a -- binary op, associative
    -- helpers:
    mconcat :: [a] -> a -- fold (flatMap)
    mconcat = foldr mappend mempty
{--
для моноида должны быть валидны законы: правая и левая единица (нейтральность операции); ассоциативность
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

нет требования коммутативности
x `mappend` y (не должен быть =) y `mappend` x
--}

-- посмотрим на типы в этом тайпклассе

-- список
instance Monoid [a] where
    mempty = []
    mappend = (++)

-- вернемся к `newtype`
-- очевидно, для чисел можно выделить два моноида: на операции сложения и операции умножения
-- чтобы эффективно определить эти два моноида, удобно воспользоваться именно `newtype`
-- это отбрасываемый в рантайме (бесплатный) враппер, он отбрасывает (не тянет за собой) имеющиеся в базовом типе реализации тайпклассов

newtype Sum a = Sum { getSum :: a } -- упаковка числа в конструкторе Sum, распаковка в геттере getSum
    deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Sum a) where -- для моноида мы требуем, чтобы а был числом
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum $ x + y
    -- (Sum x) `mappend` (Sum y) = Sum $ x + y

ghci> Sum 2 `mappend` Sum 3
Sum {getSum = 5}

-- аналогично для моноида чисел по умножению
newtype Product a = Product { getProduct :: a } -- упаковка числа в конструкторе Sum, распаковка в геттере getSum
    deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1
    (Product x) `mappend` (Product y) = Product $ x * y

ghci> Product 2 `mappend` Product 3
Product {getProduct = 6}
```
repl

https://cdsmithus.medium.com/monoids-are-composable-list-summarizers-77d2baf23ffc

```hs
{--
Реализуйте представителя класса типов `Monoid`
для типа `Xor`
в котором `mappend` выполняет операцию `xor`
--}
newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = undefined
    mappend = undefined

-- решение

```
test

### 4.6.8 стандартные моноиды

Пары (кортежи, `tuple`) - являются моноидами, если их элементы тоже моноиды.
Вообще, полезно сперва определить семантику Бинарной Ассоциативной Операции (`mappend`) для рассматриваемого типа данных.

Maybe. Сложение двух опций: возможны варианты (вытаскивать первый же Nothing), (протаскивать валидное значение Just)

```hs
instance (Monoid a, Monoid b) => Monoid (a, b) where -- требование (контекст): оба элемента есть моноиды
    mempty = (mempty, mempty) -- нейтраль из а, нейтраль из б (они оба моноиды ведь)
    (x1, y1) `mappend` (x2, y2) = (x, y) where
        x = x1 `mappend` x2 -- аппенд из моноида а
        y = y1 `mappend` y2 -- аппенд из моноида б

ghci> ("abc", Product 2) `mappend` ("def", Product 3)
("abcdef",Product {getProduct = 6})

-- вариант буквального следования монадическим законам, с делегированием операции на внутренний элемент типа моноид
instance (Monoid a) => Monoid (Maybe a) where -- требуем от элемента быть моноидом
    mempty = Nothing
    Nothing `mappend` x = x -- протаскиваем валидное значение, пока оно есть. Четыре варианта комбинаций (Nothing, Just)
    x `mappend Nothing` = x
    Just x `mappend` Just y = Just $ x `mappend` y

-- есть полезный вариант без делегирования операции во внутренний элемент
-- всегда возвращаем первый элемент из пары, если он не пустой (первый не-нулевой)
newtype First a = First { getFirst :: Maybe a } -- воспользуется newtype для создания еще одного моноида для Maybe
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    (First Nothing) `mappend` x = x
    x `mappend` _ = x -- если первый элемент не пуст, то всегда его и возвращаем

ghci> mconcat [First Nothing, First (Just 3), First (Just 5)]
First {getFirst = Just 3}
-- вернул "первый не-нулевой"

-- поговорим про "упаковку", вот так набирать утомительно и это неправда
[First Nothing, First (Just 3), First (Just 5)]
-- более правдоподобно список значений может выглядеть так
[Nothing, Just 3, Just 5]
-- упакуем
map First [Nothing, Just 3, Just 5]
-- и используем
mconcat $ map First [Nothing, Just 3, Just 5]
-- и распакуем
ghci> getFirst $ mconcat $ map First [Nothing, Just 3, Just 5]
Just 3

-- Доллар это аппликация, а точка - композиция

-- упаковка-операция-распаковка в виде композиции функций
ghci> let firstConcat = getFirst . mconcat . map First
ghci> firstConcat [Nothing, Just 3, Just 5]
Just 3

-- extra

-- Как правильно делать в современных версиях библиотеки (но не заработает в старой)
instance Monoid (First a) where
    mempty = First Nothing
instance Semigroup (First a) where
    First Nothing <> x = x
    x <> _ = x
```
repl

```hs
{--
Реализуйте представителя класса типов `Monoid`
для `Maybe' a`
чтобы `mempty` не был равен `Maybe' Nothing`
Нельзя накладывать никаких дополнительных ограничений на тип `a`
кроме указанных в условии
--}
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = undefined
    mappend = undefined

-- решение

```
test

```hs
{--
Ниже приведено определение класса `MapLike` типов, похожих на тип `Map`
Определите представителя `MapLike` для типа `ListMap`
определенного ниже как список пар ключ-значение

Для каждого ключа должно храниться не больше одного значения
Функция `insert` заменяет старое значение новым если ключ уже содержался в структуре
--}
import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

-- решение: обеспечить уникальность ключей удалением записи перед вставкой

```
test [maplike](./chapter-4.6/test-maplike.hs)

### 4.6.11 эндоморфизм, newtype Endo, Monoid Endo

Эндоморфизмы это функции где область определения = области значений.
Есть моноид эндоморфизмов, где Бинарная Ассоциативная Операция это композиция двух функций.
Нейтральный элемент это функция `id`
```hs
-- Посмотрим на список функций
[(*2), (+5), (^2)]

-- тип: список стрелок из числа-в-число
ghci> :t [(*2), (+5), (^2)]
[(*2), (+5), (^2)] :: Num a => [a -> a]

ghci> zipWith ($) [(*2), (+5), (^2)] [1 ..] -- два списка зипованы на операторе применения
[2,7,9] -- 1*2, 2+5, 3^2

-- функции из типа А в тип А называются эндоморфизмами
-- a -> a -- это эндоморфизм

newtype Endo a = Endo { appEndo :: a -> a }
instance Monoid (Endo a) where
    mempty = Endo id
    (Endo f) `mappend` (Endo g) = Endo (f . g) -- эф как декоратор над же, сначала применяется же, потом к результату применяется эф

-- использование
map Endo [(*2), (+5), (^2)]

ghci> :t map Endo [(*2), (+5), (^2)]
map Endo [(*2), (+5), (^2)] :: Num a => [Endo a] -- список эндоморфизмов

ghci> :t mconcat $ map Endo [(*2), (+5), (^2)]
mconcat $ map Endo [(*2), (+5), (^2)] :: Num a => Endo a -- сложили в один эндоморфизм

ghci> :t appEndo $ mconcat $ map Endo [(*2), (+5), (^2)]
appEndo $ mconcat $ map Endo [(*2), (+5), (^2)] :: Num a => a -> a -- вынули результат: одна ф. как комбинация списка ф.

ghci> (appEndo $ mconcat $ map Endo [(*2), (+5), (^2)]) 4
42 -- 4 ^2 +5 *2 = 16 +5 *2 = 21 *2 = 42
```
repl

Тайпкласс моноид это не просто мат.абстракция, это интерфейс, API.
Интерфейс дает средство свертки списка через бинарную ассоциативную операцию, с нейтральным элементом.
Соответственно, если операции над вашими типами данных могут быть выражены через этот интерфейс,
можно эти операции скормить разным видам вычислителей, умеющих в моноиды.
Например: бустинг через параллелизацию. И/или использование GPU, ...

> Точная формулировка была бы такой: cписок, тип элементов которого является представителем класса Monoid

```hs
-- Реализуйте представителя `MapLike` для типа `ArrowMap`, определенного ниже.

import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

-- решение
-- апи "мапки" реализуем через однопараметрические функции
-- функции можно можно "заворачивать" (оператор композиции `.`) одну-в-другую, но проще через лямбды

import Prelude hiding (lookup)
class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }
-- под капотом стрелка (маппинг) k -> Maybe v

instance MapLike ArrowMap where
    -- empty :: ArrowMap k v
    empty = ArrowMap $ const Nothing

    -- lookup :: Ord k => k -> ArrowMap k v -> Maybe v
    lookup key f = getArrowMap f key

    -- insert :: Ord k => k -> v -> ArrowMap k v -> ArrowMap k v
    insert key val (ArrowMap f) = ArrowMap (\ k -> if k == key then Just val else f k)

    -- delete :: Ord k => k -> ArrowMap k v -> ArrowMap k v
    delete key (ArrowMap f) = ArrowMap (\ k -> if k == key then Nothing else f k)

    -- fromList :: Ord k => [(k, v)] -> ArrowMap k v
    fromList lst = ArrowMap $ find lst where
        find [] _ = Nothing
        find ((k, v) : pairs) key = if k == key then Just v else find pairs key
    -- fromList [] = empty
    -- fromList ((k, v) : xs) = insert k v (fromList xs)

-- альтернативы

import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ const Nothing
    lookup k (ArrowMap f) = f k
    insert k v (ArrowMap f) = ArrowMap $ \x -> if x == k then return v else f x
    delete k (ArrowMap f) = ArrowMap $ \x -> if x == k then Nothing else f x
    fromList = foldr (uncurry insert) empty

```
test
