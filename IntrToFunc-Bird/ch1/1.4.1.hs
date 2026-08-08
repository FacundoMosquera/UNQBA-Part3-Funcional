{- 
f :: Integer -> Integer
g :: Integer -> (Integer -> Integer)

h :: Integer -> Integer -> Integer
h x y = f (g x y)


h = f . g  #False 

h x = f . (g x) #True

h x y = (f . g) x y #False

-}