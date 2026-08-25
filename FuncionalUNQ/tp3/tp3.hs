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


-}