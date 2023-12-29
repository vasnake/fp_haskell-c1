# FP Haskell

[Функциональное программирование на языке Haskell / Денис Москвин / stepik](https://stepik.org/course/75/syllabus?next=)

- [Chapter 1, intro](./chapter1.md)
- [Chapter 2, basics](./chapter2.md)
- [Chapter 3, lists](./chapter3.md)

Хороший вводный курс. Кратко, по делу, с интересными задачками.
Не все задачки одинаково полезны, но, в целом, ОК.
К сожалению, лектор часто тратит время на подробное объяснение тривиальных вещей, при этом важные концепции ФП и Хаскель упоминаются мимоходом.
Эффект от этого огорчительный: внимание притупляется пространным рассуждением о банальных вещах и как только ты начинаешь зевать,
оп, проскакивает важная мысль. Если успел заметить -- молодец, иди гугли, что это было. Если не успел -- увы, ничего нового не узнал.
Предлагаю: к каждому степу давать список ссылок на материалы для самостоятельного изучения по данной теме.

## some stuff

Нотация и как её читать.
`a -> b` это базовый стрелочный тип, функция (отображение) из `a` в `b`. А и б это типы, которые населены множеством значений.
`\ x -> some-expressions` это декларация лямбды, лямбда-выражение. Функция, принимающая х и что-то делающая.
`foo bar` это применение ф. foo к параметру bar. Имеет наивысший приоритет и левую ассоциативность.
Скобочки `(some-expressions)` используются для группировки, явного определения ассоциативности и как конструктор кортежей
(в сочетании с запятой `,`).
Оператор "не-равно" записывается как `/=` в отличие от привычного  `!=`.
Полезно смотреть информацию по операторам в repl, e.g. `ghci> :info (/=)`.
Обратная стрелочка `<-` используется в list comprehension, в монадических цепочках (do-нотация), как метафора для "получить из" или "присвоить".

> A parameter is a variable in a function definition. It is a placeholder and hence does not have a concrete value.
An argument is a value passed during function invocation.

Почему типы данных `SomeTuple = Pair(a, b)` и `Maybe = Nothing | Just a` называются, соответственно, "product" и "sum"?
Название вытекает из ответа на вопрос: как посчитать область значений, покрываемую парой? `a * b`, перемножить мощности.
Для суммы, разумеется, сложить мощности.

Когда вы слышите "сборка списка", надо понимать, что список собирается добавлением головы к хвосту.
Т.е. сборка - это выращиевание списка добавлением элементов справа-налево `let xs = 1:(2:(3:[]))`

Работа с бесконечными последовательностями опирается на ленивость (вычисления до WHNF) и концепцию редукции-через-подстановки
> В Haskell процесс вычисления результата - это череда подстановок

## links

- https://github.com/bitemyapp/learnhaskell/blob/master/guide-ru.md
- https://hub.docker.com/_/haskell/tags
- https://hoogle.haskell.org/?hoogle=isDigit&scope=set%3Ahaskell-platform
- https://downloads.haskell.org/~ghc/9.4.7/docs/users_guide/ghci.html?highlight=set#ghci-cmd-:set%20+s
- https://youtu.be/0h3Ot1C0d2I?feature=shared
- https://compscicenter.ru/courses/func-prog/2015-spring/
- https://www.lektorium.tv/course/22797
- https://youtube.com/playlist?list=PLoJC20gNfC2gpI7Dl6fg8uj1a-wfnWTH8&feature=shared
- https://github.com/denisshevchenko/ohaskell.guide
- https://wiki.haskell.org/Programming_guidelines#Naming_Conventions
