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


{-
    EJERCICIO 8

a)
    (Int -> Int) -> Int -> Int

    CASTELLANO: 
        Es una función que toma una función de enteros a enteros y devuelve otra función que
    toma un entero y devuelve un entero.

    FRANCÉS: 
        Es una función que toma otra función de enteros a enteros y un entero y devuelve un entero


b)

    (a -> b -> c) -> (a -> b) -> c

    CASTELLANO: 
        Es una función que toma otra función que toma un tipo a y devuelve una función que toma
    un tipo b y devuelve un tipo c, y devuelve una función que toma una nueva función de a en b
    y devuelve un tipo c

    FRANCÉS:
        Es una función que toma dos funciones, la primera de que toma un tipo a y un tipo b y devuelve
    c, y la segunda toma a y devuelve c, y finalmente devuelve un tipo c


c)

    (a -> b, c -> d) -> (a, c) -> (b, d)


    CASTELLANO:
        Es una función que toma una tupla cuyo primer elemento es una función de a en b, y su 
    segundo elemento es una función de c en d, y devuelve una función que toma una tupla a,c y
    devuelve una tupla de elementos b, d

    FRANCÉS:
        Es una función que toma una primer tupla cuyo primer elemento es una función de a en b, y su
    segundo elemento es una función de c en d, y una segunda tupla cuyos elementos son a y c, y
    devuelve una tupla de elementos b, d



d)

    ((a, a) -> b) -> a -> b
 

    CASTELLANO:
        Es una función que toma una función que toma una tupla de elementos de a y devuelve un tipo
    b, y devuelve una función que toma un tipo a y devuelve un tipo b

    FRANCÉS:
        Es una función que toma dos argumentos, el primero una función que toma una tupla de elementos
    de a y devuelve un tipo b, y el segundo argumento es un tipo a, y devuelve un tipo b


e)

    (a -> b -> c) -> (b -> a -> c)

    
    CASTELLANO:
        Es una función que toma otra función que toma un elemento a para devolver otra función que toma
    un elemento b y finalmente devolver un tipo c, y devuelve una función que toma un elemento b y 
    devuelve una función que toma un elemento a y devuelve un tipo c

    FRANCÉS:
        Es una función que toma otra función que toma un elemento de tipo a y otro de tipo b y
    devuelve un tipo c, y toma un elemento b y un elemento a, y devuelve un tipo c.


f)

    (a -> b) -> (a, a) -> (b, b)


    CASTELLANO:
        Es una función que toma otra función que toma un elemento a y devuelve un tipo b, y devuelve
    una función que toma una tupla de elementos de a y devuelve una tupla de elementos de b.

    FRANCÉS:
        Es una función que toma otra función que toma un elemento a y devuelve un tipo b, y toma
    una tupla de elementos de a y devuelve una tupla de elementos de b


g)

    (​a -> b, a -> c) -> a -> (b, c)


    CASTELLANO: 
        Es una función que toma una tupla cuyo primer elemento es una función que toma un a y
    devuelve un b y su segundo elemento es una función que toma un a y devuelve un c, y devuelve
    una función que toma un elemento a y devuelve una tupla de b y c

    FRANCÉS:
        Es una función que toma una tupla cuyo primer elemento es una función que toma un a y 
    devuelve un b y su segundo elemento es una función que toma un a y devuelve un c, y toma un
    a y devuelve una tupla de b y c


h)

    (a -> b -> c) -> (a -> b) -> a -> c


    CASTELLANO: 
        Es una función que toma una función que toma un elemento a y devuelve una función que toma
    un elemento b y devuelve un c, y devuelve una función que toma otra función que toma un elemento
    a y devuelve un elemento b, y devuelve una función que toma un elemento a y devuelve un c

    FRANCÉS: 
        Es una función que toma 3 argumentos, el primero es una función que toma un elemento a y un
    elemento b y devuelve c, el segundo es otra función que toma un elemento a y devuelve un elemento b,
    y el tercer argumento es un elemento a, y finalmente devuelve un c


i)

    a -> b -> a


    CASTELLANO:
        Es una función que toma un elemento a y devuelve una función que toma un elemento b y devuelve a

    FRANCÉS: 
        Es una función que toma un elemento a y un elemento b y devuelve un elemento a
-}


-- EJERCICIO 9

--a)
doble :: Int -> Int 
doble x = x+x

cuadruple = compose doble doble

cuadruple' = twice doble 

cuadruple'' = many 2 doble

--b)
timesTwoPlusThree = compose (3+) doble


--c)
fourTimes = many 4

fourTimes' f = compose (twice f) (twice f)

fourTimes'' f = twice twice f