module SafeEnum where

class (Enum a, Bounded a, Eq a) => SafeEnum a where
    -- total succ and pred, making ring of values
  ssucc :: a -> a 
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x
