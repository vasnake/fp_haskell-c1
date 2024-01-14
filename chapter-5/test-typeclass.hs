module Test where

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
    deriving Show
-- `Branch (Tree a) a (Tree a)` это конструктор данных, принимающий три значения левое-дерево, значение-в-узле, правое-дерево

instance Functor Tree where
    -- (a -> b) -> f a -> f b
    fmap fn (Leaf x) = Leaf $ fn x
    fmap fn (Branch left x right) = Branch (fmap fn left) (fn x) (fmap fn right)
-- рекурсивно проходим дерево, сигнатуры однозначно определяют реализацию

testTree = Branch (Leaf 2) 3 (Leaf 4)
