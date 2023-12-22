module ReadDigits where

import Data.Char (isDigit)
readDigits :: String -> (String, String)
readDigits = span isDigit

{--
Напишите функцию `readDigits`
принимающую строку и возвращающую пару строк
Первый элемент пары содержит цифровой префикс исходной строки
второй - ее оставшуюся часть

GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","")
--}
