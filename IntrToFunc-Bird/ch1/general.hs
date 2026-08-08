square :: Integer -> Integer 
square x = x * x

curry :: ((a, b) -> c) -> a -> b -> c 
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

quad :: Integer -> Integer 
quad = compose square square 

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