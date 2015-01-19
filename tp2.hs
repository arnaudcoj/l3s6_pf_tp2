{-
TP2 Programmation Fonctionnelle
Fichier présentant le début du TP (Q1 à Q4)

Matthieu Caron
Arnaud Cojez
-}

--Q1

alterne :: [t] -> [t]

alterne [] = []
alterne (x:[]) = [x]
alterne (x1:x2:xs) = x1:(alterne xs)

--Q2

combine :: (a -> b -> c) -> [a] -> [b] -> [c]

combine _ _ []  = []
combine _ [] _  = []
combine f (x:xs) (y:ys) = (f x y):(combine f xs ys)
                          
--Q3

pasPascal :: [Integer] -> [Integer]

pasPascal xs = zipWith (+) (xs++[0]) (0:xs) 

--Q4

pascal :: [[Integer]]

pascal = iterate pasPascal [1]

