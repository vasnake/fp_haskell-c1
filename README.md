# FP Haskell

[Функциональное программирование на языке Haskell / Денис Москвин / stepik](https://stepik.org/course/75/syllabus?next=)

## Intro

Язык чувствителен к таб/пробел.
Таб интерпретируется как 8 пробелов.
В редакторе заменить табы на пробелы (рекомендовано).

### setup dev env

> На момент разработки курса последней версией была Haskell Platform 2014.2.0.0, на которой курс и оттестирован.
Версия компилятора GHC 7.8.2, входящая в Haskell Platform 2014.2.0.0, используется и для проверки домашних заданий
https://downloads.haskell.org/~platform/2014.2.0.0/

- https://hoogle.haskell.org/
- https://www.haskell.org/ghcup/#

I'm using WSL2, Ubuntu
```s
# https://www.haskell.org/ghcup/#
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# System requirements 
#  Please ensure the following distro packages are installed before continuing (you can exit ghcup and return at any time):
# build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

sudo apt update && sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

cat > ghcup-install-result.log << EOT
OK! ~/.bashrc has been modified. Restart your terminal for the changes to take effect,
or type "source ~/.ghcup/env" to apply them in your current terminal session.

===============================================================================

All done!

To start a simple repl, run:
  ghci

To start a new haskell project in the current directory, run:
  cabal init --interactive

To install other GHC versions and tools, run:
  ghcup tui

If you are new to Haskell, check out https://www.haskell.org/ghcup/steps/
EOT
```
install ghcup (WSL).

win-setup
```s
In order to run ghc and cabal, you need to adjust your PATH variable.
To do so, you may want to run 'source /c/ghcup/env' in your current terminal
session as well as your shell configuration (e.g. ~/.bashrc).

===============================================================================

All done!

In a new powershell or cmd.exe session, now you can...

Start a simple repl via:
  ghci

Start a new haskell project in the current directory via:
  cabal init --interactive

To install other GHC versions and tools, run:
  ghcup tui

To install system libraries and update msys2/mingw64,
open the "Mingw haskell shell"
and the "Mingw package management docs"
desktop shortcuts.

If you are new to Haskell, check out https://www.haskell.org/ghcup/steps/
```
install ghcup (windows).

```s
# wsl
ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci> :q

# win
C:\ghcup\msys64\msys2_shell.cmd -mingw64
source /c/ghcup/env
ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci> :q

# docker run -it --rm haskell:7.8
wsl$ docker run -it --rm haskell:slim
slim: Pulling from library/haskell
...
GHCi, version 9.8.1: https://www.haskell.org/ghc/  :? for help
ghci> :q
```
play a little.

### ghci REPL

```s
ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci>
# no Prelude: https://wiki.haskell.org/Prelude
ghci> import Prelude
ghci> 33 + 3 * 3
42
ghci> pi
3.141592653589793
ghci> "ABC" ++ "DE"
"ABCDE"
ghci> :set prompt "GHCi> "
GHCi> 
```
знакомство с интерпретатором.

Установка prompt скрывает имя загруженного модуля.

#### 1.1.6

load (Main) module
```s
# step 6 chapter 1.1
Запустите ваш текстовой редактор и создайте файл Hello.hs, содержащий следующую строку кода:
main = putStrLn "Hello, world!"

Вызовите теперь с помощью средств вашей ОС интерпретатор GHCi c параметром — именем файла исходного кода:
ghci Hello.hs

ghci hello.hs 
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( hello.hs, interpreted )
Ok, one module loaded.
ghci> 

Проверьте, что загрузка модуля прошла успешно, вызвав в интерпретаторе определенную вами функцию main:
GHCi> main
Hello, world!

Какое приглашение на самом деле выдает командная строка интерпретатора?
ghci>
# должно быть *Main>
```
1.1.6

#### 1.1.7

load, reload module
https://stepik.org/lesson/8119/step/7?next=&unit=1375
```s
# test.hs
module Test where
sayHello = putStrLn "Hello from module Test!"

ghci> :load Test
[1 of 1] Compiling Test             ( Test.hs, interpreted )
Ok, one module loaded.
ghci> sayHello 
Hello from module Test!

ghci> :reload Test
[1 of 1] Compiling Test             ( Test.hs, interpreted ) [Source file changed]
Ok, one module loaded.
ghci> sayHello
Hello World from module Test!
```
1.1.7

### chapter 1.2, functions

Императивные языки: программа это набор инструкций, мутирует именованные ячейки памяти, переменные.
Функциональные языки: программа это составное выражение, редуцирует до получения результата.
```s
(5 + 4 * 3) ^ 2
~> (5 + 12) ^ 2
~> 17 ^ 2
~> 289
```
expression reduction example.

#### 1.2.3 Вызов фуцнкции, роль скобок

Скобки нужны для группировки (задания приоритета) операций
```s
acos cos pi # учитываю левую ассоциативность вызова функ. получаем (acos apply cos) apply (pi) что выглядит как херня.
acos (cos pi) # дает ожидаемый результат

ghci> acos (cos pi)
3.141592653589793

ghci> acos cos pi
<interactive>:8:1: error:
    • No instance for (Floating (Double -> Double))
        arising from a use of ‘it’
        (maybe you haven''t applied a function to enough arguments?)
    • In the first argument of ‘print’, namely ‘it’
      In a stmt of an interactive GHCi command: print it

# • No instance for (Floating (Double -> Double))      
говорит нам о том, что ghci не знает, что делать с функцией Double -> Double там, где ожидается Floating.
```
про скобки.

Что делать с вызовом функции с несколькими аргументами?
Ничего, каждая функция, это функция одного аргумента, благодаря каррированию.
Поэтому `f x y` это `(f apply x) apply y`, первый вызов вернет "частично примененную функцию".
```s
ghci> max 5 42
42
# the same as
ghci> (max 5) 42
42
```
два аргумента.

#### 1.2.4 операция применения функции ассоциативна влево

`(max 5)`: частичное применение функции.
```s
ghci> 3 + sin 42
2.0834784520843663
ghci> 3 + (max 5) 42
45
# (max 5) это функ. одного аргумента, как и sin
```
examples.

#### 1.2.6 обьявление функции

`sumSquares x y = x ^ 2 + y ^ 2`
Требование: имя функ. и имена параметров должны начинаться с буквы в нижнем регистре.
Буквы в верхнем регистре используются для обьявления типов данных.
Допустимо использовать одинарные кавычки в именах: `rock'n'roll = 42`

Раньше в репл нельзя было так `sumSquares x y = x ^ 2 + y ^ 2`, надо было так
`let sumSquares x y = x ^ 2 + y ^ 2`

```s
ghci> lenVec3 x y z =  sqrt (x^2 + y^2 + z^2)
ghci> lenVec3 2 3 6
7.0
```
test.

#### 1.2.8 pure functions

Зависимость только от параметров.
Функция без параметров = константа.

Наружение чистоты помещает функцию в "гетто" `IO` (монада).
Из гетто выбраться нельзя, все выражения downstream будут давать результат IO-что-то.

#### 1.2.9 condition expression if-then-else

Условное выражение: `f x = if x > 0 then 1 else (-1)` обе ветки должны быть.
Важно: литерал `-1` необходимо взять в скобки, ибо есть проблема унарного оператора `-`.

Поскольку это полноценное выражение, можно использовать его как часть другого выражения:
`g x = (if x > 0 then 1 else (-1)) + 3`
без скобок `+3` играет только в `else`.

```s
sign x = if x == 0 then 0 else if x < 0 then (-1) else 1
sign x = if x > 0 then 1 else if x < 0 then (-1) else 0
```
test.

#### 1.2.11 partial apply

Определение функции через другую функ. частично примененную: бесточечный стиль.
```s
max5 x = max 5 x
max5' = max 5

ghci> max5 4
5
ghci> max5 42
42
ghci> max5' 4
5
ghci> max5' 42
42 '

discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
# параметры limit, proc меняются редко, поэтому поставим их впереди, чтобы удобнее было делать частичное применение.
# стандартная скидка: при сумме от 1000 скидываем 5%
standardDiscount = discount 1000 5

ghci> standardDiscount 2000
1900.0

ghci> standardDiscount 999
999.0
```
repl

From https://wiki.haskell.org/Pointfree

> Pointfree Style
It is very common for functional programmers to write functions as a composition of other functions, never mentioning the actual arguments they will be applied to.

> The term originated in topology, a branch of mathematics which works with spaces composed of points, and functions between those spaces.
So a 'points-free' definition of a function is one which does not explicitly mention the points (values) of the space on which the function acts. 
In Haskell, our 'space' is some type, and 'points' are values.

### chapter 1.3, operators

https://stepik.org/lesson/8411/step/1?next=&unit=1550

#### 1.3.2 infix operators

Функции вызываются в префиксном стиле, операторы в инфиксном. Но это можно менять.
```s
max 6 7 # prefix
6 + 7 # infix

6 `max` 7 # infix
(+) 6 7 # prefix
```
snippet.

Все операторы бинарные. За одним "досадным" исключением: унарным `(-5)`.
Есть и бинарный минус: `(-) 5 3`
```s
ghci> max 5 -5

<interactive>:66:1: error:
    • No instance for (Show (Integer -> Integer))
        arising from a use of ‘print’
        (maybe you haven''t applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
ghci> max 5 (-5)
5
```
`-`

#### 1.3.3 приоритеты и ассоциативность (fixity)

Приоритет операторов: число 0..9
Чем выше приоритет, тем раньше выполнится оператор.

Ассоциативность: `infix, infixl, infixr`.
Отсутствие ассоциативности у оператора говорит о невозможности использовать его в цепочке,
e.g. `a == b < c` не работает.

По умолчанию `infixl 9`.

Применение функций (оператор ` `): приоритет 10.

Где посмотреть приоритеты: https://www.haskell.org/onlinereport/decls.html#prelude-fixities
или в репл `:info op`
```s
ghci> :info *
type Num :: * -> Constraint
class Num a where
  ...
  (*) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 7 *

ghci> :i +
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +

ghci> :i -
type Num :: * -> Constraint
class Num a where
  ...
  (-) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 -
```
repl

https://wiki.haskell.org/Keywords#infix.2C_infixl.2C_infixr

```s
ghci> 2 ^ 3 ^ 2 # infixr
512

ghci> (*) 2 ((+) 1 4) ^ 2 # operators as functions, apply have priority 10, left assoc.
100
```
test

#### 1.3.6 определение операторов

Все операторы определены в либе.
Допустимо использовать символы
`! # $ % & * + . / < = > ? @ \ ^ | - ~`

Символ `:` играет особую роль, в инфиксных конструкторах данных.
Начинать свой оператор с него не надо.

```s
module Demo where
infixl 6 *+*
a *+* b = a^2 + b^2
(*+*) a b = a^2 + b^2 # alternative definition

ghci> :load operator-def.hs 
ghci> 2 *+* 3
13
ghci> (*+*) 2 3
13
```
свой оператор, сумма квадратов.

```s
ghci> 1 + 3 *+* 2 * 2
32
# (1 + 3) *+* (2 * 2)

ghci> :i +
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +

ghci> :i *
type Num :: * -> Constraint
class Num a where
  ...
  (*) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 7 *

# Реализуйте оператор |-|, который возвращает модуль разности переданных ему аргументов
x |-| y = max (x - y) (-(x - y))
ghci> x |-| y = abs (x - y)
```
test

#### 1.3.9 частичное применение операторов (сечение, section)

```s
(/) 2
(2 /)

ghci> x = (2 /) # left section
ghci> x 3
0.6666666666666666

ghci> x = (/ 3) # right section
ghci> x 2
0.6666666666666666

ghci> (- 2) # у - нет сечения, это унарный оператор
-2
```
https://wiki.haskell.org/Section_of_an_infix_operator

```s
# Попробуйте вычислить значение выражения 
ghci> (`mod` 14) ((+ 5) 10)
1

ghci> mod 15 14
1

# Почему? функ. mod-как-оператор с правым сечением применяется к сумме.
```
test

#### 1.3.11 применение функции как оператор

Пробел между функцией и аргументом - оператор.
Такой оператор можно переопределить.
```s
f $ x = f x

# такой оператор есть, с наименьшим приоритетом и правой ассоциативностью.
ghci> :i $
($) :: (a -> b) -> a -> b       -- Defined in ‘GHC.Base’
infixr 0 $
```
Оператор `$` используется для устранения скобочек (сомнительная польза IMHO).
```s
# f (g x (h y)) == f $ g x (h y) == f $ g x $ h y

ghci> sin pi / 2
6.123233995736766e-17
ghci> sin $ pi / 2
1.0

ghci> sin 0 + pi
3.141592653589793

ghci> sin $ 0 + pi
1.2246467991473532e-16

ghci> sin (0 + pi)
1.2246467991473532e-16
```
изменение приоритета и ассоциативности применения функции.

```s
# Используя оператор $, перепишите выражение logBase 4 (min 20 (9 + 7)) без скобок
logBase 4 $ min 20 $ 9 + 7
```
test

### chapter 1.4, базовые типы

https://stepik.org/lesson/8412/step/1?next=&unit=1551

## links

- https://github.com/bitemyapp/learnhaskell/blob/master/guide-ru.md
- https://hub.docker.com/_/haskell/tags
