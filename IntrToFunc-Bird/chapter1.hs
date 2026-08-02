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


-----------------------
curry :: ((a, b) -> c) -> a -> b -> c 
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

quad' :: Float -> Float 
quad' = compose square square 

-- 4.1 -----------------
{- 
f :: Integer -> Integer
g :: Integer -> (Integer -> Integer)

h :: Integer -> Integer -> Integer
h x y = f (g x y)


h = f . g  #False 

h x = f . (g x) #True

h x y = (f . g) x y #False

-}


-- 4.2 ------------------
{-
delta :: Float -> Float -> Float -> Float
-}

-- 4.5 ------------------
appliedToOne :: (Integer -> Integer) -> Integer 
appliedToOne f = f 1

idF :: (Integer -> Integer) -> (Integer -> Integer)
idF f = f


-- 4.7 -------------------
{-
*curry (uncurry f) x y = f x y

    curry (uncurry f) x y
=>                def curry
   (uncurry f) (x,y)
=>                def uncurry
    f x y


* uncurry (curry f) (x,y) = f (x,y)

    uncurry (curry f) (x,y)
=>                 def uncurry 
    (curry f) x y
=>                 def curry 
    f (x,y)
-}


-- guarded equations --
fact :: Integer -> Integer
fact n
    | n < 0  = error "negative argument to fact"
    | n == 0 = 1 
    | n > 0  = n * fact (n-1)


-- probando local definitions --
fprueba :: Float -> Float 
fprueba x = x + 1 where x = 3 -- f 1 deberia evaluar a 4 #(lo hace)

{- 
    Si intento definir nuevamente x en la definicion como el argumento dado (x) + 2
        "where x = x + 2"
    A la hora de ejecutar genera un stack overflow.
 -}


localYguard :: Integer -> Integer -> Integer 
localYguard x y 
            | x <= 10 = x + a 
            | x > 10  = x - a
                where a = (y + 1)

-- 5.1 ---------------
fib :: Integer -> Integer 
fib n 
    | n == 0 = 0
    | n == 1 = 1
    | n >  1 = fib (n-1) + fib (n-2)

-- 5.2 ---------------
abs :: Integer -> Integer 
abs n 
    | n >= 0 =  n 
    | n < 0  = -n