import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

--Q5

pointAintercaler :: Point -> Point -> Point

pointAintercaler (xA, yA) (xB, yB) = ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)

--Q8

    
dragonOrdre :: Point -> Point -> Int -> Path

dragonOrdre xA xB 0 = [xA,xB]
dragonOrdre xA xB n =
    let xC = pointAintercaler xA xB in
    (dragonOrdre xA xC n-1):xC:(dragonOrdre xB xC n-1):[xB] 

                           
dragon :: Point -> Point -> [Path]      

dragon xA xB = dragonOrdre xA xB 12
