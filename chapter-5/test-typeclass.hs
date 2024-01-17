module Test where
import Control.Monad.Identity
-- import Prelude hiding (Monad, (>>=), (>>), return)

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
    deriving Show
-- `Branch (Tree a) a (Tree a)` это конструктор данных, принимающий три значения левое-дерево, значение-в-узле, правое-дерево

instance Functor Tree where
    -- (a -> b) -> f a -> f b
    fmap fn (Leaf x) = Leaf $ fn x
    fmap fn (Branch left x right) = Branch (fmap fn left) (fn x) (fmap fn right)
-- рекурсивно проходим дерево, сигнатуры однозначно определяют реализацию

testTree = Branch (Leaf 2) 3 (Leaf 4)

-- monadic do notation -- reference
{--
import Prelude hiding (Monad, (>>=), (>>), return)
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b -- оператор bind
    (>>) :: m a -> m b -> m b -- облегченный bind, выполняет эффект но игнорирует значение
    mx >> my = mx >>= (\ _ -> my)

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

instance Functor Identity where fmap  f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity -- Monad return
  Identity f <*> Identity v = Identity (f v) 

instance Monad Identity where
    return = Identity
    (Identity x) >>= k = k x

-- import Control.Monad.Identity
wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ = Identity . succ

goWrap4 =
    let i = 3 in
    wrap'n'succ i   >>= (\ x ->
    wrap'n'succ x   >>= (\ y ->
    wrap'n'succ y   >>
    return          (i, x + y) ))

goWrap5 = do
    let i = 3
    x <- wrap'n'succ i
    y <- wrap'n'succ x
    wrap'n'succ y
    return (i, x + y)
--}
-- End Of Reference

-- maybe -- reference
{--
import Prelude hiding (Maybe, Just, Nothing)
import Control.Monad (liftM, ap)
import qualified Control.Monad.Fail as Fail

data Maybe a = Nothing | Just a
  deriving (Show, Eq, Ord)

instance Functor Maybe where
    fmap = liftM

instance Applicative Maybe where
    pure  = return
    (<*>) = ap

instance Monad Maybe where
  --return x = Just x
  return = Just
  (Just x) >>= k = k x
  Nothing >>= _ = Nothing

  (Just _) >> m = m
  Nothing  >> _ = Nothing

instance Fail.MonadFail Maybe where
  fail _ = Nothing
--}
-- End Of Reference
