import Prelude hiding (id)
-- EJERCICIO 1
curry :: ((a,b) -> c) -> a -> b -> c 
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

-- EJERCICIO 2 y 3
apply :: (a -> b) -> a -> b
apply f x = f x 

twice :: (a -> a) -> a -> a 
twice f x = f (f x)

id :: a -> a 
id x = x 

flip :: (a -> b -> c) -> b -> a -> c 
flip f x y = f y x 

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

uflip :: ((a,b) -> c) -> (b,a) -> c 
uflip f p = f (swap p)

const :: a -> b -> a 
const x y = x 

compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

{-
EJERCICIO 4 

a) (apply apply) apply :: (a -> b) -> a -> b 

b) (twice doble) 2 :: Int 

c) ((twice twice) twice) swap :: (a,a) -> (a,a)

d) ((flip twice) 1) doble :: Int



twice twice twice :: (a' -> a') -> a' -> a'
swap :: (a,b) -> (b,a)
----------------------------------------- a'<-(a,b), b<-a 
twice twice twice swap :: (a,a) -> (a,a)

flip :: (a -> b -> c) -> b -> a -> c
twice :: (a' -> a') -> a' -> a'
------------------------------------ a<- (a'->a'), b <- a', c<- a'
flip twice ::  a' -> (a'->a') -> a'

-}

--EJERCICIO 5
--a)
appDup :: ((a,a) -> b) -> a -> b
appDup = \f -> \x -> f (x,x) 

--b)
appFork :: (a->b, a->c) -> a -> (b,c)
appFork = \(f,g) -> \x -> (f x, g x)

--c)
appPar :: (a->b, c->d) -> (a,c) -> (b,d)
appPar = \(f,g) -> \(x,y) -> (f x, g y)

--d)
appDist :: (a->b) -> (a,a) -> (b,b)
appDist = \f -> \(x,y) -> (f x, f y)

--e)
subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst = \f -> \g -> \x -> f x (g x)

{-
EJERCICIO 6 

a)

compose (fst snd) :/:  
    ya que (fst snd) :/: porque fst espera una tupla pero está recibiendo una función

probando lo que probablemente fue la idea: compose fst snd 

compose :: (b' -> c') -> (a' -> b') -> a' -> c'
fst :: (a,b) -> a
----------------------------------------- b'<-(a,b), c'<-a
compose fst :: (a'-> (a,b)) -> a' -> a

compose fst :: (a'-> (a,b)) -> a' -> a
snd :: (c, d) -> d
-------------------------------------- a'<- (c,d), d <- (a,b)
compose fst snd :: (c,(a,b)) -> a


b)

uncurry :: (a' -> b' -> c') -> (a',b') -> c'
curry :: ((a,b) -> c) -> a -> (b -> c)
------------------------------------------- a' <- ((a,b) -> c), b'<- a, c'<- (b -> c) 
uncurry curry :: ((a,b) -> c, a) -> (b -> c)

uncurry curry :: ((a,b) -> c, a) -> (b -> c)
snd :: (t1, t2) -> t2 
--------------------------------------------
uncurry curry snd :/: 

c) 
    (apply id) ((id apply) apply)

    id :: a -> a
    apply :: (a1 -> b1) -> a1 -> b1
    ------------------------------- a <- (a1 -> b1) -> a1 -> b1
    id apply :: (a1 -> b1) -> a1 -> b1


    id apply :: (a1 -> b1) -> a1 -> b1
    apply :: (a2 -> b2) -> a2 -> b2
    ---------------------------------- a1 <- (a2 -> b2), b1 <- (a2 -> b2)
    (id apply) apply :: (a2 -> b2) -> a2 -> b2


    apply :: (a3 -> b3) -> a3 -> b3 
    id :: a' -> a'
    ------------------------------- a3 <- a', b3 <- a'
    apply id :: a' -> a'


    apply id :: a' -> a'
    (id apply) apply :: (a2 -> b2) -> a2 -> b2
    --------------------------------------------- a' <- (a2 -> b2) -> a2 -> b2
    (apply id) ((id apply) apply) :: (a2 -> b2) -> a2 -> b2


d)    
     compose (compose doble doble) :: (a -> Int) -> a -> Int ? (calculado a ojo)

    compose :: (b -> c) -> (a -> b) -> a -> c
    doble :: Int -> Int
    ----------------------------------------- b <- Int, c <- Int
    compose doble :: (a -> Int) -> a -> Int


    compose doble :: (a -> Int) -> a -> Int 
    doble :: Int -> Int 
    --------------------------------------- a <- Int
    compose doble doble :: Int -> Int

    compose :: (b' -> c') -> (a' -> b') -> a' -> c'
    compose doble doble :: Int -> Int 
    ----------------------------------------------- b' <- Int, c' <- Int
    compose (compose doble doble) :: (a' -> Int) -> a' -> Int



e)

    (compose compose) doble doble 



    compose :: (b' -> c') -> (a' -> b') -> a' -> c'
    compose :: (b -> c) -> (a -> b) -> a -> c
    ----------------------------------------------- b' <- (b -> c), c' <- (a -> b) -> a -> c
    compose compose :: (a' -> (b -> c)) -> a' -> (a -> b) -> a -> c

    compose compose :: (a' -> b -> c) -> a' -> (a -> b) -> a -> c
    doble :: Int -> Int 
    ---------------------------------------------------------------
    (compose compose) doble :/:


-}

-- EJERCICIO 7
many :: Int -> (a -> a) -> (a -> a) 
many 0 f = id 
many n f = compose f (many (n-1) f)
