type Date = (Int, Int, Int)

fst3 :: (a, b, c) -> a 
fst3 (x,y,z) = x

snd3 :: (a, b, c) -> b
snd3 (x,y,z) = y

thr3 :: (a, b, c) -> c 
thr3 (x,y,z) = z


age :: Date -> Date -> Int
age now birth = yearDifference - offset 
                where yearDifference =  thr3 now - thr3 birth
                      offset = fromEnum ((snd3 now, fst3 now) < (snd3 birth, fst3 birth))



