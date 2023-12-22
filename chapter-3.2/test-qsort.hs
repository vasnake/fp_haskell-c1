module Qsort where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort xs@[x] = xs
qsort (x : xs) = qsort left ++ (x : qsort right) where
    left = filter (< x) xs
    right = filter (>= x) xs

{--
Напишите реализацию функции `qsort`
Функция qsort должная принимать на вход список элементов
сортировать его в порядке возрастания с помощью сортировки Хоара:
для какого-то элемента `x` изначального списка (обычно выбирают первый)
делить список на элементы меньше и не меньше `x`
потом запускаться рекурсивно на обеих частях.
Разрешается использовать только функции, доступные из библиотеки Prelude

GHCi> qsort [1,3,2,5]
[1,2,3,5]
--}
