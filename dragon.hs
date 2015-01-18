import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

--Q5

pointAintercaler :: Point -> Point -> Point

pointAintercaler (xA, yA) (xB, yB) = ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)

--Q6

pasDragon :: Path -> Path

pasDragon (xA:[]) = [xA]
pasDragon (xA:xB:[]) = xA:(pointAintercaler xA xB):[xB]
pasDragon (xA:xB:xC:xs) = xA:(pointAintercaler xA xB):xB:(pointAintercaler xC xB):(pasDragon (xC:xs))

--Q7

dragon :: Point -> Point -> [Path]

dragon xA xB = iterate pasDragon [xA,xB]
