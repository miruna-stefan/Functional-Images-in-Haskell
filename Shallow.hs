{-# LANGUAGE TupleSections #-}
module Shallow where

import Data.List hiding (union)
import qualified Data.Set as S
import Debug.Trace

{-
    Punct bidimensional, reprezentat ca pereche de coordonate reale (x, y).
    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Point = (Float, Float)

{-
    Tip de funcție care primește un punct, și este parametrizat în raport
    cu tipul rezultatului.
-}
type Pointed a = Point -> a

{-
    Regiune bidimensională, reprezentată ca o funcție caracteristică
    (Point -> Bool). Pentru un punct care aparține regiunii, se întoarce True;
    altfel, False.
-}
type Region = Pointed Bool

{-
    Transformare bidimensională, reprezentată ca o funcție peste puncte.
-}
type Transformation = Point -> Point

{-
    *** TODO ***

    Implementați funcția inside, care verifică dacă un punct aparține unei
    regiuni (ea însăși reprezentată ca o funcție caracteristică).

    Constrângeri: funcția trebuie implementată point-free.

    Hint: implementați mai întâi funcția cu explicitarea parametrului formal
    (point-wise), și de-abia apoi transformați-o în stil point-free.

    Exemple:

    > inside (0, 0) (== (0, 0))
    True

    > inside (1, 1) (== (0, 0))
    False
-}
inside :: Point -> Region -> Bool
inside = flip ($)

{-
    *** TODO ***

    Implementați funcția fromPoints, care construiește o regiune pe baza unei
    liste de puncte.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > fromPoints [(0, 0), (1, 1)] (0, 0)
    True

    > inside (0, 0) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    True

    > fromPoints [(0, 0), (1, 1)] (0, 1)
    False

    > inside (0, 1) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    False
-}
fromPoints :: [Point] -> Region
fromPoints = flip elem

{-
    *** TODO ***

    Implementați funcția rectangle, care generează o regiune aferentă
    unui dreptunghi, cu lățime și înălțime date, simetric față de originea
    (0, 0). De exemplu, un dreptunghi cu lățimea 2 și înălțimea 2 va avea
    punctul din stânga-sus (-1, 1), iar din dreapta-jos, (1, -1).

    Exemple:

    > rectangle 2 2 (0, 0)
    True

    > rectangle 2 2 (-1, 1)
    True

    > rectangle 2 2 (1, -1)
    True

    > rectangle 2 2 (2, 2)  
    False
-}
rectangle :: Float -> Float -> Region
{-
creez o functie lambda care primeste punctul pe care il primeste ca
parametru functia Region si verifica daca punctul se afla in dreptunghi,
adica daca abscisa si ordonata (luate in modul pentru ca ne intereseaza
distantele propriu-zise, nu cadranul in care se afla punctele) sa nu
depaseasca jumatate din latime, respectiv inaltime
-}
rectangle width height = \point -> if abs (fst point) <= width / 2 && abs (snd point) <= height / 2 then True else False

{-
    *** TODO ***

    Implementați funcția circle, care generează o regiune aferentă unui cerc,
    cu rază dată și centrul în originea (0, 0).

    Exemple:

    > circle 1 (0, 0)
    True

    > circle 1 (1, 0)
    True
    
    > circle 1 (0, 1)
    True
    
    > circle 1 (1, 1)
    False
-}
circle :: Float -> Region
{-
calculeaza distanta de la origine la punctul primit ca
parametru si verifica daca aceasta este mai mica sau egala cu raza cercului
-}
circle radius = \point ->
    let d = sqrt(fst point ** 2 + snd point ** 2)
    in
        if d <= radius then True else False

{-
    *** TODO ***

    Implementați funcția plot, care generează diagrama unei regiuni,
    pe o suprafață de desenare de dimensiuni fixate. Punctul (0, 0)
    se află în centrul suprafeței de desenare, iar lățimea și înălțimea
    unui cadran (dintre cele 4) sunt primite ca parametri. De exemplu, dacă
    lățimea este 2 și înălțimea este 1, punctul din stânga-sus al suprafeței
    este (-2, 1), iar cel din dreapta-jos, (2, -1). Pentru fiecare punct
    cu coordonate întregi de pe suprafața de desenare, se introduce caracterul
    '*', dacă punctul aparține regiunii de desenat, sau '.', altfel. Funcția
    se utilizează în conjuncție cu funcția printPlot, definită mai jos
    în schelet, pentru o mai bună vizualizare.

    Constrângeri: funcția trebuie implementată cu list comprehensions,
    fără recursivitate explicită.

    Hints:
    * fromIntegral pentru conversia de la Int la Float.
    * intercalate pentru alipirea mai multor liste folosind un element
      de legătură.

    Exemple:

    > printPlot 2 1 $ fromPoints [(0, 0), (1, 1)]
    ...*.
    ..*..
    .....

    > printPlot 2 2 $ rectangle 2 2
    .....
    .***.
    .***.
    .***.
    .....

    Deși dimensiunile dreptunghiului sunt 2 și 2, apariția a câte 3 caractere
    '*' pe orizontală și pe verticală poate fi înțeleasă dacă vă gândiți
    la coordonatele vizate, -1, 0 și 1, în toate combinațiile (x, y).

    > printPlot 2 2 $ circle 2     
    ..*..
    .***.
    *****
    .***.
    ..*..
-}
plot :: Int -> Int -> Region -> String
{- se verifica toate valorile lui x din intervalul [-width, width] cu primul y din
intervalul [height, -height], construindu-se primul rand al zonei (acesta va fi primul
element generat in lista de rezultate), apoi se verifica iar toti x-ii cu urmatorul y din
[height, -height], formandu-se al doilea rand si tot asa. Tocmai pentru a pune in evidenta 
acest fapt, am folosit un let, care imi creeaza un alias pentru functia care asteapta un y
si genereaza randul corespunzator din matrice. Apelam aceasta functie pentru toti y-ii \
pentru a obtine matricea completa.
La final, fiecare element din lista de rezultate va corespunde unui rand din matrice (adica
rezultatul va fi stocat sub forma de lista de randuri din matrice).
Intercalnad lista rezultata cu '\n', practic vom adauga caracterul '\n' dupa fiecare rand.
-}
plot width height region = intercalate "\n" [
    let createRow y = [if inside (fromIntegral x, fromIntegral y) region then '*' else '.'| x <- [-width..width]]
    in createRow y | y <- reverse [-height..height]
    ]

{-
    Utilizați această funcție pentru vizualizarea diagramelor,
    după ce implementați funcția plot.
-}
printPlot :: Int -> Int -> Region -> IO ()
printPlot width height region = putStrLn $ plot width height region

{-
    *** TODO ***

    Implementați funcțiile promoteUnary și promoteBinary, care primesc
    o funcție unară (a -> b), respectiv binară (a -> b -> c), și o promovează
    pentru a opera pe rezultatul(-ele) unor funcții (Point -> a) etc.

    Constrângeri: funcția promoteUnary trebuie implementată point-free.

    Hint: dacă expandăm referirile la Pointed din tipul funcției promoteUnary,
    obținem (a -> b) -> (Point -> a) -> (Point -> b). Practic, trebuie
    construită o funcție cu tipul (Point -> b), pornind de la o funcție cu tipul
    (Point -> a) și aplicând apoi funcția cu tipul (a -> b) pe rezultatul ei.
    Extindeți apoi ideea pentru promoteBinary.

    Exemple:

    > promoteUnary (+ 1) (\(x, _) -> x) (3, 2)
    4.0

    > promoteBinary (+) (\(x, _) -> x) (\(_, y) -> y) (3, 2)
    5.0
-}
promoteUnary :: (a -> b) -> Pointed a -> Pointed b
promoteUnary = (.)

promoteBinary :: (a -> b -> c) -> Pointed a -> Pointed b -> Pointed c
promoteBinary f pointed1 pointed2 point = f (pointed1 point) $ pointed2 point

{-
    *** TODO ***

    Implementați funcțiile complement, union și intersection, care determină
    complementul, reuniunea, respectiv intersecția a două regiuni.

    Constrângeri: funcțiile trebuie implementate point-free, utilizând
    promoteUnary sau promoteBinary, după caz.

    Exemple:

    > printPlot 2 2 $ complement $ circle 2
    **.**
    *...*
    .....
    *...*
    **.**

    > printPlot 2 2 $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    *....
    ..*..
    .***.
    ..*..
    ....*

    > printPlot 2 2 $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    .....
    .....
    ..*..
    .....
    .....
-}
complement :: Region -> Region
complement = promoteUnary (not)

union :: Region -> Region -> Region
union = promoteBinary (||)

intersection :: Region -> Region -> Region
intersection = promoteBinary (&&)

{-
    *** TODO ***

    Implementați funcția translation, care generează o translație
    cu deplasamente primite ca parametri. Deși contraintuitiv, deplasamentele
    trebuie scăzute, nu adunate, din coordonatele punctului transformat.
    De exemplu, dacă punctul (0, 0) aparține unei regiuni de interes, atunci
    punctul (1, 2) va trebui să aparțină regiunii în urma translației
    cu deplasamentele 1 și 2. Din moment ce funcția caracteristică a regiunii
    întoarce True pentru (0, 0), nu pentru (1, 2), cele două deplasamente
    trebuie scăzute.

    Exemple:

    > translation 1 2 (1, 2)
    (0.0,0.0)
-}
translation :: Float -> Float -> Transformation
-- point = pereche de coordonate (x, y), unde x = fst point, y = snd point
translation tx ty = \point -> (fst point - tx, snd point - ty)

{-
    *** TODO ***

    Implementați funcția scaling, care generează o scalare cu un factor primit
    ca parametru. Similar cu observația de la funcția translate, factorul
    contribuie prin împărțire, nu prin înmulțire.

    Exemple:

    > scaling 2 (2, 2)
    (1.0,1.0)
-}
scaling :: Float -> Transformation
scaling factor = \point -> (fst point / factor, snd point / factor)

{-
    *** TODO ***

    Implementați funcția applyTransformation, care aplică o transformare asupra
    unei regiuni.

    Constrângeri: funcția trebuie implementată point-free.

    Exemple:

    > printPlot 2 2 $ applyTransformation (translation 1 0) (circle 2)
    ...*.
    ..***
    .****
    ..***
    ...*.

    > printPlot 2 2 $ applyTransformation (scaling 0.5) (circle 2)    
    .....
    ..*..
    .***.
    ..*..
    .....
-}
applyTransformation :: Transformation -> Region -> Region
applyTransformation = flip (.)

{-
    *** TODO ***

    Implementați funcția combineTransformations, care combină transformările
    dintr-o listă într-o singură transformare. Ordinea de aplicare
    a transformărilor este dată de ordinea lor în listă.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > printPlot 2 2 $ applyTransformation
        (combineTransformations [translation 1 0, scaling 0.5]) (circle 2)
    .....
    ...*.
    ..***
    ...*.
    .....

    Echivalent cu:

    > printPlot 2 2 $ applyTransformation (translation 1 0) $
        applyTransformation (scaling 0.5) (circle 2)
-}
combineTransformations :: [Transformation] -> Transformation
combineTransformations = foldl (\transf acc -> acc . transf) id

{-
    *** TODO ***

    Funcția circles de mai jos generează o regiune formată din n cercuri de rază
    2, translatate succesiv cu 6 unități pe orizontală.

    Explicați la prezentare utilitatea evaluării leneșe pentru determinarea
    eficientă a apartenenței unui punct la regiunea construită prin reuniune.

    Hint: utilizați trace (vezi laboratorul 7) în funcția circle pentru afișarea
    punctului primit ca parametru și evaluați expresiile de mai jos:
    > inside (0, 0) $ circles 3
    > inside (6, 0) $ circles 3
    > inside (12, 0) $ circles 3
    > inside (18, 0) $ circles 3

    Exemple:

    > printPlot 15 3 $ circles 3
    ...............................
    ...............*.....*.....*...
    ..............***...***...***..
    .............*****.*****.*****.
    ..............***...***...***..
    ...............*.....*.....*...
    ...............................

    Răspuns: Fie punctul (x, y) pe care il cautam in regiunea construita prin reuniunea cercurilor.
    Daca il gasim in primul cerc, e gata, nu mai trebuie sa cautam in restul cercurilor.
    Daca nu il gasim, cautam in cercul translatat, adica avand centrul (6, 0). Daca il gasim aici,
    se opreste (true), iar daca nu il gasim, cautam in urmatorul cerc translatat, de centru (12, 0)
    si tot asa. Daca il gaseste, se opreste, daca nu, translateaza si cauta mai departe.

    Astfel, evaluarea leneasa este utila pentru ca nu se va evalua decat pana la cercul in care
    se gaseste punctul, fara a mai evalua si restul cercurilor. Adica daca punctul este gasit
    intr-un cerc, verificarea se va opri imediat, fara a mai fi evaluate si restul cercurilor.
-}
circles :: Int -> Region
circles n
    | n <= 0    = const False
    | otherwise = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             (circles (n - 1)))

{-
    *** TODO ***

    Explicați la prezentare cum se comportă reuniunea infinită de mai jos
    când se verifică apartenența unui punct care NU aparține regiunii.

    Răspuns: Pentru un punct care nu apartine regiunii, verificarea va continua la nesfarsit.
    Se va cauta punctul in primul cerc, nu se va gasi, va continua cautarea in cercul
    translatat (avand centrul in punctul (6, 0)), nu il va gasi nici aici, va cauta in
    urmatorul cerc translatat si tot asa la infinit. Va continua sa verifice pe rand 
    in fiecare cerc translatat, fara a se opri vreodata, deoarece punctul nu se 
    va gasi niciodata.
    Deci verificarea va continua la nesfarsit.
-}
infiniteCircles :: Region
infiniteCircles = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             infiniteCircles)

{-
    *** TODO BONUS ***

    Implementați funcția bfs, care realizează o căutare în lățime într-un spațiu
    oarecare de stări de tipul a, pornind de la o stare inițială start și de la
    o funcție expand, care determină pentru o stare curentă lista stărilor vecin.
    Funcția întoarce o listă, eventual infinită, de perechi de forma
    (stare, distanță), unde stările sunt ordonate conform parcurgerii în lățime,
    iar distanța reprezintă numărul de expandări realizate pentru a obține
    starea respectivă.

    Atenție! Pot exista multiple căi către aceeași stare, astfel fiind necesară
    reținerea stărilor deja vizitate utilizând o mulțime (Set, vezi modulul
    Data.Set). Observați la începutul acestui fișier linia "import qualified
    Data.Set as S". Acest lucru înseamnă că toate tipurile și funcțiile din acel
    modul trebuie prefixate cu "S."; de exemplu: S.Set, S.insert etc.

    Hint: dacă ar exista o cale unică de la starea inițială la orice altă stare,
    nefiind necesară reținerea stărilor deja vizitate, și nici nu s-ar solicita
    calculul distanțelor, funcția ar putea fi implementată prin:

    bfs :: a -> (a -> [a]) -> [a]
    bfs start expand = result
      where
        result = start : concat (map expand result)

    map operează independent pe stări și este insuficient de expresiv pentru
    a permite purtarea mulțimii de stări vizitate de la o stare la alta. Pentru
    acest lucru, ar fi necesar foldl. Funcționala predefinită mapAccumL
    realizează exact această combinație, map + foldl. O puteți utiliza pentru
    a extinde implementarea de mai sus.
-}
bfs :: (Ord a) => a -> (a -> [a]) -> [(a, Int)]
bfs start expand = undefined

{-
    *** TODO BONUS ***

    Implementați funcția regionAvoidingBfs, care determină distanța minimă
    de la orice punct la un nod de start, obținută prin deplasări către nord,
    est, sud sau vest, și ocolind regiunea primită ca parametru.

    Constrângeri: utilizați funcția bfs.

    Exemple:

    > lookup (3, 0) $ regionAvoidingBfs (-3, 0) $ circles 3
    Just 12

    Explicație: distanța de la punctul (-3, 0) la punctul (3, 0) este 12,
    și este descrisă mai jos, prin distanțele către punctele intermediare.
    Au fost folosite și cifre hexazecimale, pentru încadrarea într-un singur
    caracter. Distanța 0 corespunde punctului (-3, 0), iar distanța C (12),
    punctului (3, 0).

    ...................567...................
    ..................34*89...*.....*........
    .................12***AB.***...***.......
    .................0*****C*****.*****......
    ...................***...***...***.......
    ....................*.....*.....*........
    .........................................
-}
regionAvoidingBfs :: Point -> Region -> [(Point, Int)]
regionAvoidingBfs start region = undefined
