module Perms where
{--
Воспользовавшись функциями `map` и `concatMap`
определите функцию `perms`
которая возвращает все перестановки
которые можно получить из данного списка
в любом порядке
Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка

ghci> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Пояснения:

нужно вернуть список списков,
где каждый внутрениий список это один из вариантов перестановки исходного
n!
0! = 1
1! = 1
2! = 2
3! = 6

Метод решения: проворачивание списка. результат
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
можно получить так: три комбинации через проворачивание исходного списка (первая колонка),
плюс, для каждой из трех, комбинации проворачивания хвоста (вторая колонка)
[1,2,3],    [1] + [3,2],
[2,3,1],    [2] + [1,3],
[3,1,2],    [3] + [2,1]

что можно выразить так: каждый эл. исходного списка ставится в голову,
хвост подвергается перестановкам: каждая колонка это перестановка хвоста при выделенной голове
perm [1,2,3] =
1 : [2,3],    1 : [3,2],
2 : [3,1],    2 : [1,3],
3 : [1,2],    3 : [2,1]

--}
perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms [x, y] = [[x,y], [y,x]]
perms lst =
    concatMap permsWithSelectedHead [0..(length lst - 1)] -- for each (indexed) elem in list
    where
        -- add current head to: generated permutations without current head
        permsWithSelectedHead currHeadIdx = map (currentHead :) (perms woCurrHead)
            where
                -- splitAt 0 "foo" -> ("","foo") -- Prefix, Suffix -- вместо "прокручивания" списка
                (currentHead, woCurrHead) = (\(p, s:ss) -> (s, p ++ ss)) (splitAt currHeadIdx lst) 

{--
ghci> "foo" !! 0
'f'
ghci> splitAt 0 "foo"
("","foo")
--}

{--
другой вариант (не доделан): буквально прокручивать список вначале, для получения текущей-головы и остатка
perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms [x, y] = [[x,y], [y,x]]
perms lst@(x:xs) =
    concatMap (selectedHeadPermutations lst) lst
    -- rotateList lst $ length lst - 1
    where
        selectedHeadPermutations :: (Eq a) => [a] -> a -> [[a]]
        selectedHeadPermutations lst x = map (\xs -> x : xs) (perms $ filter (/= x) lst)
        -- rotateList :: (Eq n, Num n) => [a] -> n -> [[a]]
        -- rotateList xs 0 = [xs]
        -- rotateList lst@(x:xs) n = lst : rotateList (xs ++ [x]) (n - 1)
--}