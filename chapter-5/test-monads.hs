{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module TestMonads where
-- import Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails(NoAnnot))
-- import Data.Char (toUpper)
import Data.Char ( toUpper )
import Data.Monoid ( Sum(..) )
import Control.Monad.State ( ap, liftM, State, runState, evalState, execState, get, put, modify, state, replicateM )
import Control.Monad.Writer ( Writer, writer, runWriter, execWriter, tell )
-- import Control.Monad.Reader (Reader, reader, runReader, asks, ask, local)
-- import Control.Monad (liftM, ap)
import Text.Read (readMaybe)
-- import Text.Parsec.Prim (putState)
-- import Data.List (isInfixOf)
import Data.List as L ( isInfixOf )
import Control.Applicative ()

{--
Ideas testing sandbox, various snippets, tests, problems
-}

{--
Определите представителя класса `Functor`
для следующего типа данных
представляющего точку в трёхмерном пространстве

data Point3D a = Point3D a a a deriving Show

GHCi> fmap (+ 1) (Point3D 5 6 7)
Point3D 6 7 8
--}

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)


{--
Определите представителя класса `Functor`
для типа данных `GeomPrimitive`

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

При определении, воспользуйтесь тем, что `Point3D` уже является представителем класса `Functor`

GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)

GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)
--}

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor GeomPrimitive where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

-- test = fmap (+ 1) $ (Point3D 5 6 7) -- Point3D 6 7 8
-- test2 = fmap (+ 1) $ Point (Point3D 0 0 0) -- Point (Point3D 1 1 1)
-- test3 = fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1) -- LineSegment (Point3D 1 1 1) (Point3D 2 2 2)
test = (+ 1) <$> Point3D 5 6 7 -- Point3D 6 7 8
test2 = (+ 1) <$> Point (Point3D 0 0 0) -- Point (Point3D 1 1 1)
test3 = (+ 1) <$> LineSegment (Point3D 0 0 0) (Point3D 1 1 1) -- LineSegment (Point3D 1 1 1) (Point3D 2 2 2)

{--
Определите представителя класса `Functor` для бинарного дерева,
в каждом узле которого хранятся элементы типа `Maybe`

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

GHCi> words <$> Leaf Nothing
Leaf Nothing

GHCi> words <$> Leaf (Just "a b")
Leaf (Just ["a","b"])
--}
{--
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
-- import Data.Functor
instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Leaf maybeA) = Leaf (f <$> maybeA)
    fmap f (Branch left maybeA right) = Branch (f <$> left) (f <$> maybeA) (f <$> right)

test4 = words <$> Leaf Nothing -- Leaf Nothing
test5 = words <$> Leaf (Just "a b") -- Leaf (Just ["a","b"])
--}

{--
Определите представителя класса `Functor`
для типов данных `Entry` и `Map`

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

Тип Map представляет словарь, ключами которого являются пары

В результате должно обеспечиваться следующее поведение:
`fmap` применяет функцию к значениям в словаре, не изменяя при этом ключи

GHCi> fmap (map toUpper) $ Map []
Map []

GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]
--}

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show
-- import Data.Functor
-- `fmap` применяет функцию к значениям в словаре, не изменяя при этом ключи
instance Functor (Entry k1 k2) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Entry k v) = Entry k $ f v

instance Functor (Map k1 k2) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Map lst) = Map $ fmap (fmap f) lst

test6 = fmap (map toUpper) $ Map [] -- Map []
test7 = fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"] -- Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]

{--
Введём следующий тип:

data Log a = Log [String] a

Реализуйте вычисление с логированием, используя `Log`

Для начала определите функцию `toLogger`
которая превращает обычную функцию, в функцию с логированием

toLogger :: (a -> b) -> String -> (a -> Log b)

GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6

Далее, определите функцию `execLoggers`
Которая принимает некоторый элемент и две функции с логированием.
`execLoggers` возвращает результат последовательного применения функций к элементу и
список сообщений, которые были выданы при применении каждой из функций

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c

GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
--}
data Log a = Log [String] a deriving Show

-- превращает обычную функцию, в функцию с логированием
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg a = Log [msg] (f a)

-- принимает некоторый элемент и две функции с логированием,
-- возвращает результат последовательного применения функций к элементу и список сообщений
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (lst1 ++ lst2) c where
    Log lst1 b = f x
    Log lst2 c = g b

add1Log = toLogger (+1) "added one"
test8 = add1Log 3 -- Log ["added one"] 4
mult2Log = toLogger (* 2) "multiplied by 2"
test9 = mult2Log 3 -- Log ["multiplied by 2"] 6
test10 = execLoggers 3 add1Log mult2Log -- Log ["added one","multiplied by 2"] 8


{--
Функции с логированием из предыдущего задания возвращают в качестве результата
значение с некоторой дополнительной информацией в виде списка сообщений
Этот список является контекстом.

Реализуйте функцию `returnLog`
которая является аналогом функции `return` для контекста `Log`
Данная функция должна возвращать переданное ей значение с пустым контекстом

returnLog :: a -> Log a
--}
-- data Log a = Log [String] a deriving Show
returnLog :: a -> Log a
returnLog = Log []


{--
Реализуйте фукцию `bindLog`
которая работает подобно оператору `>>=` для контекста `Log`

bindLog :: Log a -> (a -> Log b) -> Log b

GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
--}
-- data Log a = Log [String] a deriving Show
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log lstA a) f = Log (lstA ++ lstB) b where
    Log lstB b = f a
-- тема: bind (>>=) infixl и его свойства
test11 = Log ["nothing done yet"] 0 `bindLog` add1Log -- Log ["nothing done yet","added one"] 1
test12 = Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log -- Log ["nothing done yet","added one","multiplied by 2"] 8



{--
Реализованные ранее `returnLog` и `bindLog` позволяют объявить тип `Log` представителем класса `Monad`

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

Используя `return` и `>>=`, определите функцию `execLoggersList`
которая принимает некоторый элемент, 
список функций с логированием и 
возвращает результат последовательного применения всех функций в списке к переданному элементу 
вместе со списком сообщений, которые возвращались данными функциями

execLoggersList :: a -> [a -> Log a] -> Log a

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
--}

-- data Log a = Log [String] a deriving Show
instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
    -- return :: a -> m a -- pure
    return = returnLog
    -- (>>=) :: m a -> (a -> m b) -> m b --  bind, infixl
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x = foldl (>>=) (return x)
-- execLoggersList = foldl (>>=) . return
-- foldl :: (b -> a -> b) -> b -> [a] -> b

test13 = execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)] -- Log ["added one","multiplied by 2","multiplied by 100"] 800




{--
Рассмотрим язык арифметических выражений, которые состоят из чисел, скобок, операций сложения и вычитания
Конструкции данного языка можно представить следующим типом данных

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

Реализуйте лексер арифметических выражений.

Для начала реализуйте функцию `asToken`:
Она проверяет, является ли переданная строка числом (используйте функцию `isDigit` из модуля `Data.Char`), 
знаком "+" или "-", открывающейся или закрывающейся скобкой. 
Если является, то она возвращает нужное значение обёрнутое в `Just`, в противном случае - `Nothing`

asToken :: String -> Maybe Token

GHCi> asToken "123"
Just (Number 123)
GHCi> asToken "abc"
Nothing

Далее, реализуйте функцию `tokenize`
Функция принимает на вход строку и если каждое слово является корректным токеном, 
то она возвращает список этих токенов, завёрнутый в `Just`. 
В противном случае возвращается `Nothing`
Функция должна разбивать входную строку на отдельные слова по пробелам (используйте библиотечную функцию `words`). 
Далее, полученный список строк должен быть свёрнут с использованием функции `asToken` и свойств монады `Maybe`

tokenize :: String -> Maybe [Token]

GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]
GHCi> tokenize "1 + ( 7 - 2 )" -- Обратите внимание, что скобки отделяются пробелами от остальных выражений!
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
GHCi> tokenize "1 + abc"
Nothing
--}

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

-- import Text.Read (readMaybe)
asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken x = readMaybe x >>= (Just . Number)
-- asToken x = if all Data.Char.isDigit x then Just $ Number (read x) else Nothing
-- readMaybe :: Read a => String -> Maybe a        -- Defined in ‘Text.Read’

tokenize :: String -> Maybe [Token]
tokenize str = sequence listMaybeTokens where
    wordList = words str
    listMaybeTokens = map asToken wordList
-- sequence :: Monad m => [m a] -> m [a] -- Defined in ‘Data.Traversable’

test14 = asToken "123" -- Just (Number 123)
test15 = asToken "abc" -- Nothing

test16 = tokenize "1 + 2" -- Just [Number 1,Plus,Number 2]
test17 = tokenize "1 + ( 7 - 2 )" -- Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
-- Обратите внимание, что скобки отделяются пробелами от остальных выражений!
test18 = tokenize "1 + abc" -- Nothing


{--
Пусть имеется тип данных, который описывает конфигурацию шахматной доски

data Board = ...

Кроме того, пусть задана функция `nextPositions`
которая получает на вход некоторую конфигурацию доски и 
возвращает все возможные конфигурации, которые могут получиться, если какая-либо фигура сделает один ход

nextPositions :: Board -> [Board]

Напишите функцию `nextPositionsN`
которая принимает конфигурацию доски, число ходов `n`, предикат `p` и 
возвращает все возможные конфигурации досок, которые могут получиться, 
если фигуры сделают `n` ходов и которые удовлетворяют заданному предикату. 
При `n < 0` функция возвращает пустой список. 

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
--}

data Board = String
nextPositions :: Board -> [Board]
nextPositions x = [x, x]

--Тип Board и функция nextPositions заданы, реализовывать их не нужно
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
    | n < 0 = []
    | n == 0 = [b | pred b] -- терминальное условие рекурсии, эти значения пойдут в результат
    | otherwise = do
        board <- nextPositions b -- first loop
        nextPositionsN board (n - 1) pred -- inner loop, рекурсия
{--
логика:
на каждом ходе происходит ветвление предыдущих позиций
каждая позиция порождает эм новых

> фильтруйте только конечный набор состояний,  и не проверяйте промежуточные результаты
предикат - это оценочная функция, она отбрасывает слабые варианты
функция nextPositions рассматривает все легальные продолжения

тестов нет, поэтому перебором нащупано такое решение
--}


{--
Используя монаду списка и do-нотацию, реализуйте функцию `pythagoreanTriple`
которая принимает на вход некоторое число `x` и возвращает список троек `(a,b,c)`
таких что

a^2 + b^2 = c^2,
a > 0,  b > 0,  c > 0,  c ≤ x,  a < b

pythagoreanTriple :: Int -> [(Int, Int, Int)]

Число `x` может быть `≤ 0` , на таком входе должен возвращаться пустой список

GHCi> pythagoreanTriple 5
[(3,4,5)]

GHCi> pythagoreanTriple 0
[]

GHCi> pythagoreanTriple 10
[(3,4,5),(6,8,10)]
--}

-- принимает на вход некоторое число `x` и возвращает список троек `(a,b,c)`
-- a^2 + b^2 = c^2, -- катет всегда меньше гипотенузы, которая в свою очередь ограничена сверху
-- a > 0,  b > 0,  c > 0,  c ≤ x,  a < b
-- `x` может быть `≤ 0` , на таком входе должен возвращаться пустой список
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    c <- allC
    a <- allA
    b <- allB
    [42 | x > 0 && allConditionsTrue a b c]
    return (a, b, c) where
        allA = [1 .. x]
        allB = [1 .. x]
        allC = [1 .. x]
        allConditionsTrue a b c = a < b && (a^2 + b^2) == c^2 && a < c && b < c
-- задачка на предыдущую тему "if then else внутри цепочки монад.вычислений"

test19 = pythagoreanTriple 5 -- [(3,4,5)]
test20 = pythagoreanTriple 0 -- []
test21 = pythagoreanTriple 10 -- [(3,4,5),(6,8,10)]


{--
На этом шаге вы будете работать с монадой `IO`
значит, ваша программа будет взаимодействовать с операционной системой.
Чтобы тестирующая система смогла оценить вашу программу, пожалуйста, 
используйте только функции, осуществляющие ввод/вывод на терминал

getChar, putChar, putStr, putStrLn, getLine

Все эти функции уже будут находиться в области видимости, так что вам не следует их импортировать.

По той же причине, главная функция вашей программы будет называться не `main`, а `main'` (со штрихом)

Напишите программу, которая будет спрашивать имя пользователя
а затем приветствовать его по имени
если пользователь не ввёл имя, программа должна спросить его повторно, и 
продолжать спрашивать, до тех пор, пока пользователь не представится

Итак, первым делом, программа спрашивает имя:

What is your name?
Name: 

Пользователь вводит имя и программа приветствует его:

What is your name?
Name: Valera
Hi, Valera!

Если же пользователь не ввёл имя, необходимо отобразить точно такое же приглашение ещё раз:

What is your name?
Name: 
What is your name?
Name: 
What is your name?
Name: Valera
Hi, Valera!

строго соблюдайте приведенный в примере формат вывода. 
Особое внимание уделите пробелам и переводам строк! 
Не забудьте про пробел после `Name:`, а также про перевод строки в самом конце 
ожидается, что вы будете использовать `putStrLn` для вывода приветствия пользователя
--}
{-- solution
main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if null name then main' else putStrLn $ "Hi, " ++ name ++ "!"
--}


{--
На этом шаге вы будете работать с монадой IO, а значит, ваша программа будет взаимодействовать с операционной системой
Чтобы тестирующая система смогла оценить вашу программу, пожалуйста, используйте только функции, 
работающие с файлами и директориями

getDirectoryContents, removeFile

Все эти функции уже будут находиться в области видимости,
так что вам не следует их импортировать. 
По той же причине, главная функция вашей программы будет называться не `main`, а `main'` (со штрихом).

В этом задании ваша программа должна попросить пользователя ввести любую строку, 
а затем удалить все файлы в текущей директории, в именах которых содержится эта строка,
выдавая при этом соответствующие сообщения

Substring: 

Пользователь вводит любую строку:

Substring: hell

Затем программа удаляет из текущей директории файлы с введенной подстрокой в названии.

К примеру, если в текущей директории находились файлы 
thesis.txt, kitten.jpg, hello.world, linux_in_nutshell.pdf,
то вывод будет таким:

Substring: hell
Removing file: hello.world
Removing file: linux_in_nutshell.pdf

Если же пользователь ничего не ввёл (просто нажал Enter), следует ничего не удалять и сообщить об этом

Substring: 
Canceled

Для получения списка файлов в текущей директории используйте функцию `getDirectoryContents`
https://hackage.haskell.org/package/directory-1.2.3.1/docs/System-Directory.html#v:getDirectoryContents
передавая ей в качестве аргумента строку, состоящую из одной точки  ("."), что означает «текущая директория»

Для удаления файлов используйте функцию `removeFile`
https://hackage.haskell.org/package/directory-1.2.3.1/docs/System-Directory.html#v:removeFile
считайте, что в текущей директории нет поддиректорий — только простые файлы

В выводимых сообщениях удаленные файлы должны быть перечислены в том же порядке, в котором их возвращает функция `getDirectoryContents`

Пожалуйста, строго соблюдайте приведенный в примере формат вывода.
Особое внимание уделите пробелам и переводам строк!
Не забудьте про пробел после Substring:, а также про перевод строки в конце.
ожидается, что вы будете использовать putStrLn для вывода сообщений об удалении
--}
main' :: IO ()
main' = do
    putStr "Substring: "
    substr <- getLine
    if null substr then putStrLn "Canceled" else removeFiles substr

removeFiles :: String -> IO ()
removeFiles substr = do
    names <- getFiles
    mapM_ (removeMatchingFile substr) names

-- getDirectoryContents :: FilePath -> IO [FilePath]
-- type FilePath = String
getFiles =
    return ["thesis.txt", "kitten.jpg", "hello.world", "linux_in_nutshell.pdf"]
    -- getDirectoryContents "."

-- removeFile :: FilePath -> IO ()
-- removeMatchingFile :: String -> String -> IO ()
removeMatchingFile substr fileName = do
    if fileName `contains` substr then doRemoveFile else skipRemoval where
        skipRemoval = return ()
        doRemoveFile = do
            putStrLn $ "Removing file: " ++ fileName
            return ()
            -- removeFile fileName

-- import Data.List (isInfixOf)
-- isInfixOf :: Eq a => [a] -> [a] -> Bool
contains fileName substr = substr `L.isInfixOf` fileName


{--
Вспомним пример с базой пользователей и паролей

type User = String
type Password = String
type UsersTable = [(User, Password)]

Реализуйте функцию, принимающую в качестве окружения `UsersTable`
и возвращающую список пользователей, использующих пароль "123456"
в том же порядке, в котором они перечислены в базе

GHCi> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
["user","root"]
--}

newtype Reader r a = Reader { runReader :: (r -> a) } -- завернули стрелку в тип `Reader r a`, двухпараметрический

instance Functor (Reader r) where
    fmap = liftM
instance Applicative (Reader r) where
    pure  = return
    (<*>) = ap
instance Monad (Reader r) where
    return x = Reader (\ e -> x)
    m >>= k  = Reader (\ e ->
        let x = runReader m e
        in runReader (k x) e)

ask :: Reader r r -- `r -> r`, фактически и есть `id`
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader -- конструктор ридера из аргумента-функции

local :: (r -> r) -> Reader r a -> Reader r a
local f mr = Reader (runReader mr . f)

reader :: (r -> a) -> Reader r a
reader f = do
    r <- ask
    return (f r)

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks (map fst . selected) where
    selected = filter (\ (_, pwd) -> pwd == "123456")

-- Реализуйте функцию, принимающую в качестве окружения `UsersTable`
-- и возвращающую список пользователей, использующих пароль "123456"
-- в том же порядке, в котором они перечислены в базе

test22 = runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")] -- ["user","root"]


{--
Давайте разработаем программное обеспечение для кассовых аппаратов одного исландского магазина
Заказчик собирается описывать товары, купленные покупателем, с помощью типа `Shopping` следующим образом

type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

Последовательность приобретенных товаров записывается с помощью `do`-нотации
Для этого используется функция `purchase`
которую вам предстоит реализовать

Эта функция принимает наименование товара, а также его стоимость в исландских кронах 
(исландскую крону не принято делить на меньшие единицы, потому используется целочисленный тип Integer)
Кроме того, вы должны реализовать функцию `total`:

GHCi> total shopping1 
19708
--}

-- import Data.Monoid
-- import Control.Monad
-- import Control.Monad.Writer ( Writer, writer, runWriter, tell )

{--
type Shopping = Writer (Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = do
    tell $ Sum cost

total :: Shopping -> Integer
-- total = getSum . snd . runWriter
total = getSum . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

test23 = total shopping1 -- 19708
--}


{--
Измените определение типа `Shopping` и доработайте функцию `purchase`
из предыдущего задания таким образом, чтобы можно было реализовать функцию `items`
возвращающую список купленных товаров 
(в том же порядке, в котором они были перечислены при покупке):

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

GHCi> total shopping1 
19708
GHCi> items shopping1
["Jeans","Water","Lettuce"]

Реализуйте функцию `items` и исправьте функцию `total`, чтобы она работала как и прежде
--}
type Shopping = Writer (Sum Integer, [String]) ()

purchase :: String -> Integer -> Shopping
purchase item cost = do
    tell (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

test23 = total shopping1 -- 19708
test24 = items shopping1 -- ["Jeans","Water","Lettuce"]


{--
Давайте убедимся, что с помощью монады `State` можно эмулировать монаду `Reader`
Напишите функцию `readerToState`, «поднимающую» вычисление из монады `Reader` в монаду `State`

GHCi> evalState (readerToState $ asks (+2)) 4
6
GHCi> runState (readerToState $ asks (+2)) 4
(6,4) 
--}

readerToState :: Reader r a -> State r a
readerToState rm = state valStatePair where
    valStatePair givenState = (runReader rm givenState, givenState)
-- newtype State s a  = State  { runState :: s -> (a, s) }
-- newtype Reader r a = Reader { runReader :: (r -> a) }
-- return a = State (\st -> (a, st)) -- Monad State pure

test25 = evalState (readerToState $ asks (+2)) 4 -- 6
test26 = runState (readerToState $ asks (+2)) 4 -- (6,4) 


{--
Теперь убедимся, что с помощью монады `State` можно эмулировать монаду `Writer`
Напишите функцию `writerToState`, «поднимающую» вычисление из монады `Writer` в монаду `State`:

GHCi> runState (writerToState $ tell "world") "hello,"
((),"hello,world")
GHCi> runState (writerToState $ tell "world") mempty
((),"world")

Обратите внимание на то, что при работе с монадой `Writer` предполагается, что изначально лог пуст
(точнее, что в нём лежит нейтральный элемент моноида), 
поскольку интерфейс монады просто не позволяет задать стартовое значение.
Монада `State` же начальное состояние (оно же стартовое значение в логе) задать позволяет
--}

writerToState :: Monoid w => Writer w a -> State w a
writerToState wa = state valStatePair where
    valStatePair givenState = (a, givenState `mappend` w)
    (a, w) = runWriter wa
-- newtype State s a  = State  { runState :: s -> (a, s) }
-- newtype Writer w a = Writer { runWriter :: (a, w) }
-- import Control.Monad.Writer ( Writer, writer, runWriter, execWriter, tell )

test27 = runState (writerToState $ tell "world") "hello," -- ((),"hello,world")
test28 = runState (writerToState $ tell "world") mempty -- ((),"world")



{--
Если бы мы хотели вычислить n-е число Фибоначчи на императивном языке программирования
мы бы делали это с помощью двух переменных и цикла, обновляющего эти переменные

def fib(n):
  a, b = 0, 1
  for i in [1 .. n]:
    a, b = b, a + b
  return a

С точки зрения Хаскеля, такую конструкцию удобно представлять себе как вычисление с состоянием. 
Состояние в данном случае — это два целочисленных значения

Императивный алгоритм действует очень просто: он совершает n шагов, каждый из которых некоторым образом изменяет текущее состояние. 
Первым делом, реализуйте функцию `fibStep`
изменяющую состояние таким же образом, как и один шаг цикла в императивном алгоритме

GHCi> execState fibStep (0,1)
(1,1)
GHCi> execState fibStep (1,1)
(1,2)
GHCi> execState fibStep (1,2)
(2,3)

После этого останется лишь применить этот шаг n раз к правильному стартовому состоянию и выдать ответ
Реализуйте вспомогательную функцию `execStateN`, которая принимает число шагов n,
вычисление с состоянием и начальное состояние, 
запускает вычисление n раз и выдает получившееся состояние (игнорируя сами результаты вычислений)

Применяя эту функцию к `fibStep`, мы сможем вычислять числа Фибоначчи:

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
--}

-- newtype State s a  = State  { runState :: s -> (a, s) }
fibStep :: State (Integer, Integer) () -- type state value -- runState (value, state)
fibStep = do
    (n1, n2) <- get
    put (n2, n1 + n2)
    return ()

execStateN :: Int -> State s a -> s -> s
execStateN n ms = execState (replicateM n ms)

-- tests

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

test29 = execState fibStep (0,1) -- (1,1)
test30 = execState fibStep (1,1) -- (1,2)
test31 = execState fibStep (1,2) --(2,3)
test32 = fib 7 -- 13
test33 = fib 12 -- 144


{--
Некоторое время назад мы определили тип двоичных деревьев, содержащих значения в узлах

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

В этой задаче вам дано значение типа `Tree ()`, иными словами, вам задана форма дерева.

Требуется пронумеровать вершины дерева данной формы, обойдя их `in-order` 
то есть, сначала обходим левое поддерево, затем текущую вершину, затем правое поддерево

GHCi> numberTree (Leaf ())
Leaf 1
GHCi> numberTree (Fork (Leaf ()) () (Leaf ()))
Fork (Leaf 1) 2 (Leaf 3)
--}
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree :: Tree () -> Tree Integer -- replace values `()` with ordering numbers
numberTree t = evalState (numberTreeS t) 1
-- обойдем дерево с протаскиванием стейта
numberTreeS :: Tree () -> State Integer (Tree Integer) -- state value
-- Tree is a sum type, pat.mat. for 2 cases
numberTreeS (Leaf _) = do
    n <- get
    put (n + 1)
    return (Leaf n) -- set current number
numberTreeS (Fork left _ right) = do
    lt <- numberTreeS left -- in-order: left, node, right
    n <- get
    put (n + 1)
    rt <- numberTreeS right
    return $ Fork lt n rt -- construct current node

test34 = numberTree (Leaf ()) -- Leaf 1
test35 = numberTree (Fork (Leaf ()) () (Leaf ())) -- Fork (Leaf 1) 2 (Leaf 3)

-- reference
{--
tick :: State Int Int -- тут стрелки не видно, но она есть, стейт это монада над стрелочным типом
tick = do
    n <- get
    put (n + 1) -- new state
    return n -- new value

ghci> runState tick 5
(5,6) -- (value, state) предыдущее значение в качестве "значения"

ghci> execState tick 5
6

succ :: Int -> Int
succ n = execState tick n -- only state

plus :: Int -> Int -> Int
plus n x = execState (sequence (replicate n tick)) x

ghci> runState (sequence (replicate 4 tick)) 5
([5,6,7,8],9) -- список значений цепочки монад.вычислений, эффект (стейт) = 9

replicateM n = sequence . replicate n

ghci> :i replicateM
replicateM :: Applicative m => Int -> m a -> m [a] -- Defined in ‘Control.Monad’

plus n x = execState (replicateM n tick) x

ghci> runState (replicateM 4 tick) 5
([5,6,7,8],9)

instance Monad (State s) where
    return a = State (\ st -> (a, st)) -- заворачиваем стрелку -- pure, как лямбда завернутая в newtype
    m >>= k = 
        State $ \st1 -> -- заворачиваем стрелку
            let
                (a, st2) = runState m st1 -- первое вычисление, левая часть bind
                m2 = k a -- создадим второе вычисление, очередность правильная, правая часть bind
            in runState m2 st2 -- и запускаем второе вычисление на (возможно модифицированном) стейте

newtype Reader r a = Reader { runReader :: (r -> a) } -- завернули стрелку в тип `Reader r a`, двухпараметрический
instance Functor (Reader r) where
    fmap = liftM
instance Applicative (Reader r) where
    pure  = return
    (<*>) = ap
instance Monad (Reader r) where
    return x = Reader (\ e -> x)
    m >>= k  = Reader (\ e ->
        let x = runReader m e
        in runReader (k x) e)
ask :: Reader r r -- `r -> r`, фактически и есть `id`
ask = Reader id
asks :: (r -> a) -> Reader r a
asks = Reader -- конструктор ридера из аргумента-функции
local :: (r -> r) -> Reader r a -> Reader r a
local f mr = Reader (runReader mr . f)
reader :: (r -> a) -> Reader r a
reader f = do
    r <- ask
    return (f r)

put :: s -> State s ()
put st = State $ \ _ -> ((), st) -- игнорирует внешний стейт, кладет в монаду новый

newtype State s a  = State  { runState :: s -> (a, s) }
newtype Reader r a = Reader { runReader :: (r -> a) }
newtype Writer w a = Writer { runWriter :: (a, w) }

newtype Writer w a = Writer { runWriter :: (a, w) } -- n.b. флипнуты типы
runWriter :: Writer w a -> (a, w)
writer :: (a, w) -> Writer w a

-- type-class, полностью полиморфный, без ограничений на типы (переменные типов) a, b, f
-- класс типов параметризован переменной (типа) `f`
class Functor f where
    -- fmap "поднимает" функцию `a -> b` в "контекст" `f`: принимает значение `f a`, на выходе значение `f b`
    -- где `f a`, `f b` это два типа (конструктора), параметризованных а и бе, соответственно.
    -- при этом сам контекст не меняется, нет "эффекта"
    fmap :: (a -> b) -> f a -> f b -- видно, что эф это конструктор типов, ибо "применяется" к а и к бе
-- переменная `f` используется как функция над типами, у нее должен быть "стрелочный кайнд": `* -> *`
-- <$> -- infix synonim for fmap

закоры функтора
-- 1) fmap id = id
-- 2) fmap (f . g) = (fmap f) . (fmap g) -- композиция лифтов заменяется на 1 лифт композиции функций, типа оптимизация
второй закон опционален в Хаскел, он следует из первого при условии полиморфности функтора

-- для реализации монады нужно реализовать функтор и аппликатив
instance Functor Identity where
  fmap  f (Identity x) = Identity (f x) -- fmap :: (a -> b) -> f a -> f b
instance Applicative Identity where
  pure x = Identity x -- Monad return  
  (Identity f) <*> (Identity v) = Identity (f v) -- "applied over", infixl 4 <*>, <*> :: f (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a -- pure
    (>>=) :: m a -> (a -> m b) -> m b --  bind, infixl
    (>>) :: m a -> m b -> m b -- then, sequence

-- байнд может менять структуру "контейнера"

-- первый закон (левый pure)
(return a) >>= k    =    k a
-- второй закон (правый pure)
m >>= return        =    m
-- третий закон, ассоциативность bind (скобки опциональны, и без них корректно)
(m >>= k) >>= k'    =   m >>= (\ x -> k x >>= k')

-- первый и второй законы отражают "тривиальную" природу оператора `return`
-- он не выполняет эффектов и не меняет значение

-- третий закон (ассоциативность), несколько хитрее: порядок операций не меняется, но лямбды влияют на "накопление" эффектов.
-- Накопление эффектов должно быть ассоциативно. 
-- Если эффект - это список строк лога, то надо понимать, что конкатенация списков должна быть ассоциативна.

-- оператор амперсанд (евро) очень похож на монадический байнд -- лево-ассоциативный и порядок аргументов такой же
ghci> import Data.Function
ghci> :i &
(&) :: a -> (a -> b) -> b       -- Defined in ‘Data.Function’
infixl 1 &
ghci> 5 & (+2) & (*3) & (+1) -- pipeline, left-to-right
22 -- (((5 +2) *3) +1)
-- монадический bind вот таким образом и работает, как пайплайн, слева-направо

--}
