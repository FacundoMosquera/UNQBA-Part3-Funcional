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