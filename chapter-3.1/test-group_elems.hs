module GroupElems where
-- повторяющиеся элементы собирает во внутренний список-группу
-- не-повторяющиеся элементы имеют свои собственные группы-списки
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = loop x xs [] -- curr-elem, tail, accum
    where
        loop h [] acc = reverse $ add h acc -- empty tail: end of recursion
        loop h (t:ts) acc = loop t ts (add h acc) -- have tail: add head to acc and continue
        -- return accum: add h to the last group or add a new group
        add h [] = [h] : []
        add h (g:gs) = if (head g) == h then (h : g) : gs else [h] : g : gs

{--
GHCi> groupElems []
[]

GHCi> groupElems [1,2]
[[1],[2]]

GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]

GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]
--}
