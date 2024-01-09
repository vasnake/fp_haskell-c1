module ParsePerson where
{--
Реализуйте функцию `parsePerson`
которая разбирает строки и возвращает либо результат типа `Person`, либо ошибку типа `Error`

`firstName = John\nlastName = Connor\nage = 30`

Строка, которая подается на вход, должна разбивать по символу '\n' на список строк
каждая из которых имеет вид `X = Y`.
Если входная строка не имеет указанный вид, то функция должна возвращать `ParsingError`

Если указаны не все поля, то возвращается `IncompleteDataError`

Если в поле `age` указано не число, то возвращается `IncorrectDataError str`, где `str` — содержимое поля `age`

Если в строке присутствуют лишние поля, то они игнорируются

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
data Person = Person { firstName :: String, lastName :: String, age :: Int }
parsePerson :: String -> Either Error Person
parsePerson = undefined
--}

-- решение
-- лишние поля: не ошибка, недостаточно полей: ошибка
-- неправильный тип возраста: ошибка
-- строка разбивается по `\n` в список строк. Каждая строка в списке разбивается по `=` на пару (имя, значение)
-- если строка не разбивается по ` = ` то это ошибка
-- из трех пар по именам (firstName, lastName, age) собирается запись Person
-- необходимо учесть возможный порядок возникновения ошибок: сначала возможна ParsingError, если прошли, то возможна IncompleteDataError, ...

-- import Data.Text (lines, pack, unpack)
import Data.Char (isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving Show

parsePerson :: String -> Either Error Person
parsePerson = pairsToPerson . listToPairs . splitToList -- split then parse pairs then parse Person

splitToList :: String -> Either Error [String] -- ParsingError
splitToList str = if length lst < 3 then Left ParsingError else Right lst where
    lst = Prelude.lines str

listToPairs :: Either Error [String] -> Either Error [(String, String)] -- ParsingError
listToPairs (Left err) = Left err
listToPairs (Right lst) = if length pairs /= length lst then Left ParsingError else Right pairs where
    pairs = dropEmpty . normalize . map (span (/= '=')) $ lst

    dropEmpty :: [(String, String)] -> [(String, String)]
    dropEmpty = filter (\ (k, v) -> (not . null $ k) && (not . null $ v))

    normalize :: [(String, String)] -> [(String, String)]
    normalize [] = []
    normalize ((k, v):xs) = (normK k, normV v) : normalize xs

    -- trimRightSpaces
    normK [] = []
    normK (x : xs) = if x == ' ' && all (== ' ') xs then [] else x :  normK xs

    -- trimHeadingSpaceOrEq
    normV [] = []
    normV ('=':xs) = normV xs
    normV (' ':xs) = normV xs
    normV xs = xs

pairsToPerson :: Either Error [(String, String)] -> Either Error Person -- IncompleteDataError | IncorrectDataError
pairsToPerson (Left err) = Left err
pairsToPerson (Right lst) = if length provided /= length required then Left IncompleteDataError else makePerson provided where
    provided = filter (\ (k, _) -> k `elem` required) lst
    required = ["firstName", "lastName", "age"]

    makePerson :: [(String, String)] -> Either Error Person
    makePerson pairs = if not validAge then Left $ IncorrectDataError age else
        Right Person { firstName = fn, lastName = ln, age = read age :: Int } where
            age = getValue "age"
            fn = getValue "firstName"
            ln = getValue "lastName"
            validAge = not (null age) && all isDigit age && (head age /= '0')

            getValue k = case lookup k pairs of
                Just v -> v
                Nothing -> error "No such key: " ++ k

{--
> parsePerson "firstName = John\nlastName = Connor\nage = 30"
Right (Person {firstName = "John", lastName = "Connor", age = 30})

> parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
Right (Person {firstName = "John Smith", lastName = "Connor", age = 30})

> parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"
Right (Person {firstName = "Barbarian", lastName = "Conn On", age = 30})

ParsingError
parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"
parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd\n drrr"

IncompleteDataError
parsePerson "firstName=Barbarian\nlastName=Conn On"

IncorrectDataError str
parsePerson " firstName = John\nlastName = Connor\nage = 2f8 "
--}
