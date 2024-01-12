module Expr where
import Prelude hiding (Monoid, mappend, mempty)

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
--}

-- решение
-- надо раскладывать в суммы произведений, никаких произведений сумм быть не должно.
-- т.е. у нас есть три конструктора, из них два: составные выражения
-- надо пересобрать так, чтобы остались только суммы произведений

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand e = if isExpanded ex then ex else doExpand ex where ex = doExpand e

doExpand :: Expr -> Expr
doExpand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
doExpand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
doExpand (e1 :+: e2) = expand e1 :+: expand e2
doExpand (e1 :*: e2) = expand e1 :*: expand e2
doExpand e = e

isExpanded (e1 :+: e2) = isExpanded e1 && isExpanded e2
isExpanded (e1 :*: e2) = isExpandedProd e1 && isExpandedProd e2 where -- нашли произведение и ушли в ветки
    isExpandedProd (e1 :*: e2) = isExpandedProd e1 && isExpandedProd e2
    isExpandedProd (_ :+: _) = False -- в ветках встретили суммы, т.е. на выходе произведение сумм (нужна сумма произведений)
    isExpandedProd _ = True
isExpanded _ = True

-- reference

-- Вам стоит не особо модифицировать ту функцию, что дана, а написать что-то свое. Решение получается кратким
-- достаточно только дистрибутивности. Ключевое слово - рекурсивные
-- Фактом `deriving Eq` пользоваться не надо (хотя, конечно, наверное, можно).

-- попробуйте такой трюк expand . expand $ v1 * (v2 * (v3 + v4)) т.е. разворачивать пока не развернется,
-- как определить что "развернулось"? в этом ваша задача

{--
дистрибутивность
distributive law, in mathematics, the law relating the operations of multiplication and addition,
stated symbolically as 
a * (b + c) = (a * b) + (a * c)
that is, the monomial factor `a` is distributed, or separately applied, to each term of the binomial factor `b + c`,
resulting in the product `ab + ac`

ассоциативность
The associative property of multiplication says that
while multiplying three numbers, regardless of the way the numbers are grouped,
the end result will always be the same:
a * b * c = (a * b) * c = a * (b * c)

коммутативность
Commutative Law of Multiplication
the result of the multiplication of two numbers stays the same, even if the positions of the numbers are interchanged
a * b = b * a

1 * 2 является произведением 2 чисел.
1 является произведением 1 числа.
Следовательно, 1 + 2 является суммой произведений чисел.

Пусть на вход дают
(Val 1 :+: Val 2) :*: Val 3
Тогда следующие ответы считаются правильными:
Val 1 :*: Val 3 :+: Val 2 :*: Val 3
(Val 1 :*: Val 3) :+: (Val 2 :*: Val 3)
Val 3 :*: Val 1 :+: Val 2 :*: Val 3
Val 3 :*: Val 2 :+: Val 1 :*: Val 3

(1 + 2) * (3 + 4) =
    (1 + 2) * 3 + (1 + 2) * 4 = 
        (1 * 3 + 2 * 3) + (1 * 4 + 2 * 4)

1 * (2 + 3) * 4 = 
    (1 * 2 + 1 * 3) * 4 = 
        (1 * 2 * 4) + (1 * 3 * 4)

--}

-- (1 + 2 + 3) * (4 + 5)
test1 = expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
-- 1 * 4 + (1 * 5 + (2 * 4 + (2 * 5 + (3 * 4 + 3 * 5))))
expected1 = Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))

-- (1 + 2 + 3) * (4 + 5)
-- >
-- (1 + 2) * (4 + 5) + 3 * (4 + 5) -- first rule

test2 = expand $ Val 1 :*: (Val 2 :*: (Val 3 :+: Val 4) :+: Val 5)
-- 1 * (2 * (3 + 4) + 5)
-- 1 * ((2 * 3) + (2 * 4) + 5)
-- (1 * 2 * 3) + (1 * 2 * 4) + (1 * 5)

-- 1 * (2 * (3 + 4))
-- (1 * 2 * 3) + (1 * 2 * 4)

checkExpand (e1 :+: e2) = checkExpand e1 && checkExpand e2
checkExpand (e1 :*: e2) = checkExpand' e1 && checkExpand' e2 where -- нашли произведение и ушли в ветки
    checkExpand' (e1 :*: e2) = checkExpand' e1 && checkExpand' e2
    checkExpand' (_ :+: _) = False -- в ветках встретили суммы, т.е. на выходе произведение сумм (нужна сумма произведений)
    checkExpand' _ = True
checkExpand _ = True

test3 = checkExpand . expand $ (Val 1 :+: Val 2) :*: (Val 3 :+: Val 4)
