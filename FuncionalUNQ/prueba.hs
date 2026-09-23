twice :: (a -> a) -> (a -> a)
twice f = g 
        where g x = f (f x)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

doble :: Int -> Int 
doble x = x + x 

uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f = g 
        where g p = f (swap p)

apply :: (a -> b) -> (a -> b)
apply f = g 
        where g x = f x

flip :: (b -> a -> c) -> a -> b -> c 
flip f = h 
        where h x = k 
                where k y = (f y) x

appFork :: (a -> b, a -> c) -> (a -> (b, c))
appFork (f, g) = h 
                where h x = (f x, g x) 

appDup :: ((a,a) -> b) -> a -> b
appDup f = g 
          where g x = f (x,x)

twice' :: (a -> a) -> a -> a 
twice' = appDup (uncurry (.))