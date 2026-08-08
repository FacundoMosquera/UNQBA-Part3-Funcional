double :: (a -> a) -> a -> a
double f x = f (f x)

square :: Integer -> Integer 
square x = x*x

quad :: Integer -> Integer 
quad = double square