module Demo where
factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1 -- связываем параметр с конкретным значением
factorial' n = n * factorial' (n - 1) -- матчинг по порядку, если не 0 то переменная-с-не-нулем

doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = n * doubleFact (n - 2)

factorial'' 0 = 1
factorial'' n = if n < 0 then error "arg must be >= 0" else n * factorial'' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n | n == 0    = 1
             | n > 0     = n * factorial4 (n - 1)
             | otherwise = error "arg must be >= 0"
