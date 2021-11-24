
--Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути книгою (ав-
--тор/спiвавтори, назва, мiсто, видавництво, рiк), статтею (автор/спiвавтори, на-
--зва статтi, назва журналу, рiк, номер журналу, сторiнки) або тезами доповiдi
--(автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). Ви-
--значне функцiї для :
--пошуку усiх видавництв, журналiв, конференцiй;



type Author = String
type Name = String
type City = String
type Year = Int
type Number = Int
type Pages = Int
type Source = String

data Publication = Book Author Name City Source Year |  
               Article Author Name Source Year Number Pages |
               Thesus Author Name Source City Year Pages
               deriving Show




showPublish:: [Publication] -> [Source]
showPublish [] = []
showPublish ( Book _ _ _ [] _:xs)  = showPublish xs 
showPublish ( Book _ _ _ n _ :xs) = isAlreadyExists n (showPublish xs)

showPublish ( Article _ _ [] _ _ _:xs) = showPublish xs
showPublish ( Article _ _ n _ _ _:xs) = isAlreadyExists n (showPublish xs)

showPublish ( Thesus _ _ [] _ _ _:xs) = showPublish xs 
showPublish ( Thesus _ _ n _ _ _:xs) = isAlreadyExists n (showPublish xs)

publications = [
    Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
    Article "Anatolii" "Covid-19" "Lancet" 2021 12 250,
    Thesus "Artem" "Metaphysics" "Conference of physics" "Amsterdam" 2020 12,
    Book "Andrii" "UI/UX design" "Rivne" "Rivne book" 2020,
    Book "Anton" "Study easy" "Kyiv" "Malyatko" 2021,
    Article "Arsentii" "Web developing" "Webdev" 2021 30 25,
    Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
    Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001
            ]

--масив значень для тестування створеної функції


isAlreadyExists n l
  | n `elem` l = l
  | otherwise = n:l

--Функція іsAlreadyExists перевіряє чи зустрічається дане джерело у нашому списку
--Якщо так - Пропускає його
--Якщо ні - додає в результуючий масив


--Тестування
--publications = [
    -- Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
    -- Article "Anatolii" "Covid-19" "Lancet" 2021 12 250,
    -- Thesus "Artem" "Metaphysics" "Conference of physics" "Amsterdam" 2020 12,
    -- Book "Andrii" "UI/UX design" "Rivne" "Rivne book" 2020,
    -- Book "Anton" "Study easy" "Kyiv" "Malyatko" 2021,
    -- Article "Arsentii" "Web developing" "Webdev" 2021 30 25, 
    --]
--ghci> showPublish publications
--["Lancet","Conference of physics","Rivne book","Malyatko","Webdev"]

-- publications = [
--     Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
--     Article "Anatolii" "Covid-19" "Lancet" 2021 12 250,
--     Thesus "Artem" "Metaphysics" "Conference of physics" "Amsterdam" 2020 12,
--     Book "Andrii" "UI/UX design" "Rivne" "Rivne book" 2020,
--     Book "Anton" "Study easy" "Kyiv" "Malyatko" 2021,
--     Article "Arsentii" "Web developing" "Webdev" 2021 30 25,
--     Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
--     Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
--              ]
--ghci> showPublish publications
--["Lancet","Conference of physics","Rivne book","Webdev","Malyatko"]
