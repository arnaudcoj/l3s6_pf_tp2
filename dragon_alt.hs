{-
TP2 Programmation Fonctionnelle
Fichier présentant le second algorithme de la courbe du dragon (Q8)

Matthieu Caron
Arnaud Cojez
-}

import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) red (dragonAnime (50,250) (450,250))

{-
Certains traits venaient parasiter l'affichage de l'a courbe du dragon, en effet, avec notre algorithme, certains points apparaissaient plusieurs fois, d'où l'utilisation de la fonction dragonCouple
-}

dragonAnime a b t = Pictures (dragonCouple (dragon a b (round t `mod` 20)))

--Permet de ne pas afficher les traits parasites entre certains points
dragonCouple :: [Point] -> [Picture]

dragonCouple [] =  []
dragonCouple (xA:xB:xS) = (Line [xA,xB]):(dragonCouple xS)

--Q5

pointAintercaler :: Point -> Point -> Point

pointAintercaler (xA, yA) (xB, yB) = ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)

--Q8
    
dragonOrdre :: Point -> Point -> Int -> Path

dragonOrdre xA xB 0 = [xA,xB]
dragonOrdre xA xB n =
    let xC = pointAintercaler xA xB in
    (dragonOrdre xA xC (n-1))++(dragonOrdre xB xC (n-1))


dragon :: Point -> Point -> Int -> Path      

dragon xA xB t = (dragonOrdre xA xB t)++[]
