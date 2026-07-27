-- 1.1 -----------------
double :: (a -> a) -> a -> a
double f x = f (f x)

square :: Int -> Int 
square x = x*x

quad :: Int -> Int 
quad = double square


-- 1.2 ----------------
greater :: Int -> Int -> Int
greater x y = if x > y then x else y

-- 1.3-----------------
areaOfACircle :: Float -> Float 
areaOfACircle r = 22/7 * square r
