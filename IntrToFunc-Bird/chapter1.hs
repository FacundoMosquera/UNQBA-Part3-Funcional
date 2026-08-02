-- 1.1 -----------------
double :: (a -> a) -> a -> a
double f x = f (f x)

square :: Float -> Float 
square x = x*x

quad :: Float -> Float 
quad = double square


-- 1.2 ----------------
greater :: Int -> Int -> Int
greater x y = if x > y then x else y

-- 1.3-----------------
areaOfACircle :: Float -> Float 
areaOfACircle r = 22/7 * square r


-- 4.1-------------------
curry :: ((a, b) -> c) -> a -> b -> c 
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

