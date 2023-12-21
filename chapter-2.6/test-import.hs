module Test where
import Data.List hiding (union)
import Data.Set
myUnion [] ys = ys
-- myUnion xs ys = union xs ys -- error: type mismatch
