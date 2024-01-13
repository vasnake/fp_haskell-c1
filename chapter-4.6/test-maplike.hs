module MapLike where
{--
Ниже приведено определение класса `MapLike` типов, похожих на тип `Map`
Определите представителя `MapLike` для типа `ListMap`
определенного ниже как список пар ключ-значение

Для каждого ключа должно храниться не больше одного значения
Функция `insert` заменяет старое значение новым если ключ уже содержался в структуре

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
--}
import Prelude hiding (lookup)
import qualified Data.List as L

-- typeclass
class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

    fromList [] = empty
    fromList ((k, v) : xs) = insert k v (fromList xs)

-- wrapper for list of tuples
newtype ListMap k v = ListMap { getListMap :: [(k, v)] }
    deriving (Eq, Show)

-- MapLike implementation for ListMap
instance MapLike ListMap where
    -- empty :: ListMap k v
    empty = ListMap []

    -- lookup :: Ord k => k -> ListMap k v -> Maybe v
    lookup _ (ListMap []) = Nothing
    lookup key (ListMap ((k, v) : pairs)) = if key == k then Just v else lookup key (ListMap pairs)

    -- delete :: Ord k => k -> ListMap k v -> ListMap k v
    delete _ (ListMap []) = ListMap []
    delete key (ListMap ((k, v) : pairs)) = if key == k then ListMap pairs else ListMap ((k, v) : rest) where
        rest = getListMap $ delete key (ListMap pairs)

    -- insert :: Ord k => k -> v -> ListMap k v -> ListMap k v
    insert k v lst = ListMap ((k, v) : rest) where
        rest = getListMap $ delete k lst
