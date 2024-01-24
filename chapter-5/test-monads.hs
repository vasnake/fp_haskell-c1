module TestMonads where
-- import Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails(NoAnnot))
-- import Data.Char (toUpper)
import Data.Char
import Control.Monad

{--
Ideas testing sandbox, various snippets, tests
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

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
-- import Data.Functor
instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Leaf maybeA) = Leaf (f <$> maybeA)
    fmap f (Branch left maybeA right) = Branch (f <$> left) (f <$> maybeA) (f <$> right)

test4 = words <$> Leaf Nothing -- Leaf Nothing
test5 = words <$> Leaf (Just "a b") -- Leaf (Just ["a","b"])

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

-- reference
{--

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

-- оператор амперсанд (евро) очень похож на монадический байнд -- лево-ассоциативный и порядок аргументов такой же
ghci> import Data.Function
ghci> :i &
(&) :: a -> (a -> b) -> b       -- Defined in ‘Data.Function’
infixl 1 &
ghci> 5 & (+2) & (*3) & (+1) -- pipeline, left-to-right
22 -- (((5 +2) *3) +1)
-- монадический bind вот таким образом и работает, как пайплайн, слева-направо

--}