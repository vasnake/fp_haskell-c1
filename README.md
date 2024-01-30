# FP Haskell

Часть 1 / 2
[Функциональное программирование на языке Haskell / Денис Москвин / stepik](https://stepik.org/course/75/syllabus?next=) /
https://stepik.org/course/Функциональное-программирование-на-языке-Haskell-75 /
https://stepik.org/75

- [Chapter 1, intro](./chapter1.md)
- [Chapter 2, basics](./chapter2.md)
- [Chapter 3, lists](./chapter3.md)
- [Chapter 4, data types](./chapter4.md)
- [Chapter 5, monads](./chapter5.md)

Хороший вводный курс. Кратко, по делу, с интересными задачками.
Не все задачки одинаково полезны, но, в целом, ОК.
К сожалению, лектор часто тратит время на подробное объяснение тривиальных вещей, при этом важные концепции ФП и Хаскель упоминаются мимоходом.
Эффект от этого огорчительный: внимание притупляется пространным рассуждением о банальных вещах и как только ты начинаешь зевать,
хоп, проскакивает важная мысль. Если успел заметить -- молодец, иди гугли, что это было. Если не успел -- увы, ничего нового не узнал.

Предлагаю: к каждому степу давать список ссылок на материалы для самостоятельного изучения по данной теме.

Курс обязателен к изучению для всех, кто хочет разобраться в функциональном программировании.
Терминология, паттерны решения проблем - это мастхев и оно тут есть.

[Certificate](./stepik-certificate-75-7e10396.pdf) https://stepik.org/cert/2317467?lang=en

## short notes

Нотация и как её читать
- Отступы важны и увеличение отступа означает "продолжение блока с предыущей строки".
- `foo :: Bar` означает "нечто foo типа Bar". `Bar` это тип объекта `foo`.
Например: `getListMap :: [(k,v)]` означает "getListMap имеет тип список туплей (ака пар)".
- `a -> b` это базовый стрелочный тип, функция (отображение) из `a` в `b`. А и б это типы, которые населены множеством значений.
- `\ x -> some-expressions` это декларация лямбды, лямбда-выражение. Функция, принимающая `х` и что-то делающая.
- `foo bar` это применение ф. `foo` к параметру `bar`. Имеет наивысший приоритет и левую ассоциативность.
- Скобочки `(some-expressions)` используются для группировки, явного определения ассоциативности,
как конструктор кортежей (в сочетании с запятой `,`).
Особый случай: группировка унарного минуса и числа: `(-42)`, без скобок отрицательное число нельзя записать.
- Квадратные скобки `[x, y, z]` означают список (конструктор списка).
- Фигурные скобки `{ foo::Int, bar::Char }` означают record syntax в определении конструктора данных, где пара `foo::Bar` означает "метка::Тип".
- Оператор "не-равно" записывается как `/=` в отличие от привычного  `!=`.
- Обратная стрелочка `<-` используется в list comprehension, в монадических цепочках (do-нотация), как метафора для "получить из" или "присвоить".
- `data`: ключевое слово, начало определения типа данных (конструктор типа `=` конструктор(ы) данных)
- `type`: ключевое слово, начало определения синонима (алиаса) типа (конструктор типа `=` конструктор типа)
- `newtype`: ключевое слово, начало определения эфемерного типа с одним конструктором и одним параметром конструктора
- `case ... of ...`: позволяет использовать пат.мат. в правой части уравнений.
- `do ...`: запись цепочки монадических вычислений (пайплайн стрелок Клейсли, связанных оператором монадыц `bind`)
- `let ... in ...`, `where`: два способа абстрагировать вычисления внутри функции

Существует охулиард "операторов", например `.`, `$`, `&`, `<$>` и т.д. Это инфиксные двух-параметрические функции,
обладающие ассоциативностью (левой или правой) и приоритетом (число от 0 до 9),
применение которых можно записывать не только в инфиксном, но и в префиксном виде, e.g. `(+) 40 2`.
Полезно смотреть информацию по операторам в repl, e.g. `ghci> :info (/=)`.

В интерпретаторе можно выполнять многострочные блоки кода, это делается так (см. маркеры `:{`, `:}`)
```hs
ghci> :{
ghci| foo :: (Num b) => a -> b
ghci| foo x = 42
ghci| :}
```
repl

> A `parameter` is a variable in a function definition. It is a placeholder and hence does not have a concrete value.
An `argument` is a value passed during function invocation.

> In logic, mathematics, and computer science, `arity` is the number of arguments or operands ...
In mathematics, `arity` may also be called `rank`....
In logic and philosophy, `arity` may also be called `adicity` and `degree`
In linguistics, it is usually named `valency`

Почему типы данных `SomeTuple = Pair(a, b)` и `Maybe = Nothing | Just a` называются, соответственно, "product" и "sum"?
Название вытекает из ответа на вопрос: как посчитать область значений, покрываемую парой? `a * b`, перемножить мощности.
Для суммы, разумеется, сложить мощности.

Когда вы слышите "сборка списка", надо понимать, что список собирается добавлением головы к хвосту.
Т.е. сборка - это выращиевание списка добавлением элементов справа-налево `let xs = 1:(2:(3:[]))`

Работа с бесконечными последовательностями опирается на ленивость (вычисления до WHNF) и концепцию редукции-через-подстановки
> В Haskell процесс вычисления результата - это череда подстановок

`WHNF`: слабая заголовочная нормальная форма, weak head normal form. Вычисления форсируются до этой формы.

`foldr`, `foldl`: два вида сверток, правая и левая. Правая умеет работать с бесконечностью, левая может быть оптимизирована (TCO).

Комбинатор: функция без свободных переменных, в теле только аргументы и операции применения. См. лямбда-исчисление.

Typeclass: это одно-параметрическое описание интерфейса (API), выделяющего некий "класс" типов. Чтобы тип стал членом этого класса,
должен быть реализован instance для этого типа, воплощающий данный интерфейс.

Тип: может быть пользовательский (`data`, `newtype`, ...) или встроенный. Любое выражение имеет тип. Тип выражения выводится компилятором.
Слово `type` используется для создания алиаса существующего типа.

Наблюдение: в ФП часто достаточно видеть сигнатуру (тип) функции, чтобы понять ее смысл, семантику.
К этому надо стремиться в своем коде.

`Type`, `Kind`, `Class`: терминология иерархии типизации.
Есть некое выражение (`42 * pi`), у него есть тип, возможно, оно состоит из других выражений и функций, которые должны быть совместимы по типам.
Применение фунции к выражению проверяется на корректность через типы (параметров функции, действительных аргументов).
Есть конструкторы типов, по сути - функции над типами. Их корректность проверяется через "над-типы" - виды (kind).
Над выражениями есть типы, над типами есть кайнды.

А есть "классы" - `class`, это, на самом деле, type-class, т.е. классы типов.
По сути - интерфейсы с одним параметрам типа, определяющие API для некоторого множества типов.
Чтобы тип мог войти в конкретное множество (тайпкласс), для этого типа должен быть определен `instance`,
в котором реализованы все обязательные методы интерфейса (class minimal complete definition).

type-class в Scala: параметризованный интерфейс, один параметр типа.
Интерфейс определяет алгебру для множества возможных аргументов этого параметра.

Наиболее важные типы и классы типов
- List: тип данных
- Maybe: тип данных
- Either: тип данных
- Monoid: type-class (нейтральный элемент и бинарная операция)
- Kleisli arrow: функция `a -> m b` где тип `m` это враппер над `b`, нужен для поддержки не-чистоты и не-тотальности функции `a -> b`.

- `Functor`: type-class с одним методом `fmap :: (a -> b) -> f a -> f b`:
поднимает функцию в контекст, aka оператор "применения" `<$>`. Структура "контейнера" не меняется.
Законы функтора: 1) `fmap id = id`; 2) `fmap (f . g) = (fmap f) . (fmap g)`.

- `Monad`: тайпкласс с двумя методами
`return :: a -> m a` aka `pure` поднимает значение в монаду, позволяет любую стрелку сделать стрелкой Клейсли `k`.
`bind :: m a -> (a -> m b) -> m b` aka `>>=` оператор монадического связывания вычислений,
структура "контейнера" может меняться (вычисления с эффектами).
Законы монад: 1) левая "единица" pure `(return a) >>= k = k a`; 2) правая "единица" pure `m >>= return = m`; 3) ассоциативность bind `(m >>= k1) >>= k2 = m >>= (\ x -> k1 x >>= k2)`

В стдлиб обычно используется соглашение:
- `runFoo` для запуска вычислений в монаде (и значение и эффекты),
- `execFoo` для получения эффектов от вычислений (значение игнорируется),
- `evalFoo` для получения значения и игнорирования эффектов

`Semigroup`, `Monoid`, `Group`
- полугруппа - это где есть ассоциативная бинарная операция
- моноид - это полугруппа с единицей
- группа - это моноид с обратными элементами
- Не любой моноид - группа, но любая группа - уже моноид.

## links

Часть 2 / 2
[Функциональное программирование на языке Haskell (часть 2)](https://stepik.org/course/693/info) /
https://stepik.org/693

- https://hoogle.haskell.org/?hoogle=isDigit&scope=set%3Ahaskell-platform
- https://wiki.haskell.org/Pronunciation
- https://github.com/bitemyapp/learnhaskell/blob/master/guide-ru.md
- https://hub.docker.com/_/haskell/tags
- https://downloads.haskell.org/~ghc/9.4.7/docs/users_guide/ghci.html?highlight=set#ghci-cmd-:set%20+s
- https://youtu.be/0h3Ot1C0d2I?feature=shared
- https://compscicenter.ru/courses/func-prog/2015-spring/
- https://www.lektorium.tv/course/22797
- https://youtube.com/playlist?list=PLoJC20gNfC2gpI7Dl6fg8uj1a-wfnWTH8&feature=shared
- https://github.com/denisshevchenko/ohaskell.guide
- https://wiki.haskell.org/Programming_guidelines#Naming_Conventions

Все подряд
- https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html#untilJust
- https://exercism.org/tracks/haskell
- https://www.youtube.com/playlist?list=PLlb7e2G7aSpRDR44HMNqDHYgrAOPp7QLr
- https://github.com/hmemcpy/milewski-ctfp-pdf
- http://sebastian-millies.blogspot.com/2016/02/recursive-function-memoization-with.html
- https://gitlab.haskell.org/haskell/prime/-/wikis/libraries/proposals/monad-fail#adapting-old-code
- https://www.youtube.com/watch?v=IkXg_mjNgG4
- https://www.adit.io/
- https://egghead.io/courses/professor-frisby-introduces-composable-functional-javascript
- https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
- https://medium.com/@xanderdeseyn/grasping-haskell-functors-applicatives-and-monads-part-1-93368e0a7a74#.e3s8heals
- https://medium.com/@xanderdeseyn/grasping-haskell-functors-applicatives-and-monads-part-2-65255e3e6a1d#.j86j4dlva
- https://en.wikibooks.org/wiki/Haskell/Category_theory
- https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/
- https://youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_&feature=shared
- https://wiki.haskell.org/Phantom_type
- https://cdsmithus.medium.com/monoids-are-composable-list-summarizers-77d2baf23ffc
- https://en.wikipedia.org/wiki/Semigroup_action#:~:
- https://habr.com/ru/articles/323526/
- https://stackoverflow.com/questions/18934882/haskell-line-of-code-not-compiling-illegal-datatype-context
- https://www.codewars.com/kata/haskell
- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
- https://wiki.haskell.org/Performance/GHC
- https://github.com/quchen/articles/blob/master/build.md
- https://stackoverflow.com/questions/2263541/what-does-mean-in-haskell
- https://wiki.haskell.org/Lazy_pattern_match
- https://tio.run/##LYsxCsIwAAD3vOKgDoqktM7WxUFdjKCbihSatMEYpe1QIr49VvC2g7um7O7auRjfMkEdTju1P9422zVSLUjkRwjDQE3BJSBX2J8uC/LsD32j/RhMA3OGGdp1miDEo7R@vF6t9T0TzNNVLQZbcc5J0ytZjF8
- https://www.futurelearn.com/courses/functional-programming-haskell
- https://wiki.haskell.org/Monomorphism_restriction
- https://rsdn.org/forum/decl/4104888.all
- https://wiki.haskell.org/Programming_guidelines#Naming_Conventions
- https://github.com/keathley/pointfree.io
- https://github.com/bmillwood/pointfree
- https://aosabook.org/en/v2/ghc.html#fig.ghc.pipeline
- https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/an-external-representation-for-the-ghc-core-language-for-ghc-6.10.html#introduction
- https://learnyouahaskell.com/starting-out#an-intro-to-lists
- https://stackoverflow.com/questions/11046590/the-seq-function-and-strictness/11048004#11048004
- https://downloads.haskell.org/ghc/latest/docs/users_guide/flags.html#compiler-debugging-options
- https://wiki.haskell.org/Reducible_expression
- https://wiki.haskell.org/Thunk
- https://wiki.haskell.org/Weak_head_normal_form
- https://medium.com/@aleksandrasays/brief-normal-forms-explanation-with-haskell-cd5dfa94a157
- http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/
- https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270011
- https://learnyouahaskell.com/types-and-typeclasses
- https://chalmers.instructure.com/courses/7710/pages/lectures
- https://www.cse.chalmers.se/edu/year/2018/course/TDA452/
- https://github.com/bitemyapp/learnhaskell
- https://wiki.haskell.org/IDEs
- https://www.haskell.org/ghcup/steps/
- https://wiki.haskell.org/Prelude
- https://wiki.haskell.org/Pointfree
- https://wiki.haskell.org/Keywords#infix.2C_infixl.2C_infixr
- https://wiki.haskell.org/Section_of_an_infix_operator
- https://github.com/Copilot-Language/copilot
- https://gist.github.com/klapaucius/f0adec8a567b7bf000c8bcf99686a9bd
