module GorkMork where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x
        | doesEnrageGork x && doesEnrageMork x = (stomp . stab) x
        | doesEnrageGork x = stab x
        | doesEnrageMork x = stomp x
        | otherwise = x 
