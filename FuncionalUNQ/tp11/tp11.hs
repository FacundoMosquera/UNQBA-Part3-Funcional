import Prelude hiding (scanr, zipWith, foldr1, foldr, filter, map)
-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --
data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa
    deriving Show

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = fromEnum (f i) + cantidadCapasQueCumplen f p


conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)


soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) = armarSiCumple f i (soloLasCapasQue f p)


armarSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
armarSiCumple f i p = if f i then Capa i p else p


-- ================================================================================= --
--                                  EJERCICIO 2                                      --
-- ================================================================================= --
--a)
sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . tieneLactosa)

tieneLactosa :: Ingrediente -> Bool 
tieneLactosa Queso = True 
tieneLactosa _     = False

--b)
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa =  ((==0) . cantidadCapasQueCumplen tieneLactosa)

--c)
cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso = cantidadCapasQueCumplen tieneLactosa

--d)
conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas doblarAceitunas

doblarAceitunas :: Ingrediente -> Ingrediente 
doblarAceitunas (Aceitunas n) = Aceitunas (n*2)
doblarAceitunas i             = i



-- ================================================================================= --
--                                  EJERCICIO 3                                      --
-- ================================================================================= --
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b 
pizzaProcesada f z Prepizza   = z
pizzaProcesada f z (Capa i p) = f i (pizzaProcesada f z p)


-- ================================================================================= --
--                                  EJERCICIO 4                                      --
-- ================================================================================= --
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen' f = pizzaProcesada ((+) . (fromEnum . f)) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas' f = pizzaProcesada (Capa . f) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada (armarSiCumple f) Prepizza

sinLactosa' :: Pizza -> Pizza 
sinLactosa' = pizzaProcesada (armarSiCumple (not . tieneLactosa)) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool 
aptaIntolerantesLactosa' = (==0) . (pizzaProcesada ((+) . (fromEnum . tieneLactosa)) 0) 

cantidadDeQueso' :: Pizza -> Int 
cantidadDeQueso' = pizzaProcesada ((+) . (fromEnum . tieneLactosa)) 0

conElDobleDeAceitunas' :: Pizza -> Pizza 
conElDobleDeAceitunas' = pizzaProcesada (Capa . doblarAceitunas) Prepizza


-- ================================================================================= --
--                                  EJERCICIO 5                                      --
-- ================================================================================= --
--a)
cantidadAceitunas :: Pizza -> Int 
cantidadAceitunas = pizzaProcesada ((+) . nroDeAceitunas) 0

nroDeAceitunas :: Ingrediente -> Int 
nroDeAceitunas (Aceitunas n) = n 
nroDeAceitunas _             = 0

--b)
capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i is -> if f i then i:is else is) []

--c)
conDescripcionMejorada :: Pizza -> Pizza 
conDescripcionMejorada = pizzaProcesada compAceitunas Prepizza

compAceitunas :: Ingrediente -> Pizza -> Pizza 
compAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p 
compAceitunas i              p                     = Capa i p

--d)
conCapasDe :: Pizza -> Pizza -> Pizza 
conCapasDe = flip (pizzaProcesada Capa)

--e)
{-
    Razonamiento:
    Si se intenta realizar con pizzaProcesada, en un enfoque directo, donde primerasNCapas
    recibe el entero, no parece posible.
    Sin embargo, si se utiliza flip y pizzaProcesada, el tipo de esta ultima con los primeros dos
    argumentos dados, sería Pizza -> b. Donde b = Int -> Pizza.
    Siguiendo este razonamiento, el elemento que se remplaza en el caso base debe ser una función,
    que recibe un entero y sin importar cuál es, devuelve Prepizza al haberse quedado sin ingredientes
    por ende (\n -> Prepizza).
    Por otro lado, analizando la función en los casos recursivos, f, debe ser una de: 
    Ingrediente -> b -> b, y como instanciamos b previamente, el tipo final de f es:
    Ingrediente -> (Int -> Pizza) -> Int -> Pizza.
    
-}
primerasNCapas :: Int -> Pizza -> Pizza 
primerasNCapas = flip (pizzaProcesada f (\n -> Prepizza))
        where f i g n = if n==0 then Prepizza else Capa i (g (n-1))



-- ================================================================================= --
--                                  EJERCICIO 6                                      --
-- ================================================================================= --
{-

a)
        ∀f. length . soloLasCapasQue f = cantidadCapasQueCumplen f
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀p.∀f. (length . soloLasCapasQue f) p = cantidadCapasQueCumplen f p
    
    Dem.:
    Sea p' un elemento de Pizza. Por ppio. de ind. sobre la estructura de p'.

    caso base, p' = Prepizza
            (length . soloLasCapasQue f) Prepizza = cantidadCapasQueCumplen f Prepizza
    
    caso inductivo, p' = (Capa i pi)
            HI) (length . soloLasCapasQue f) pi = cantidadCapasQueCumplen f pi
            TI) (length . soloLasCapasQue f) (Capa i pi) = cantidadCapasQueCumplen f (Capa i pi)

    
    ||caso base, p' = Prepizza||

    LADO IZQ    
            (length . soloLasCapasQue f) Prepizza
        =                           (def. (.))
            length (soloLasCapasQue f Prepizza)
        =                           (def. soloLasCapasQue.1)
            length Prepizza
        =                           (def. length.1)
            0

    LADO DER
            cantidadCapasQueCumplen f Prepizza
        =                           (def. cantidadCapasQueCumplen.1)
            0


    ||caso inductivo, p' = (Capa i pi)||

    LADO IZQ
            (length . soloLasCapasQue f) (Capa i pi)
        =                           (def. (.))
            length (soloLasCapasQue f (Capa i pi))
        =                           (def. soloLasCapasQue.2)
            length (armarSiCumple f i (soloLasCapasQue f pi))
        =                           (def. armarSiCumple)
            length (if f i then Capa i (soloLasCapasQue f pi) else (soloLasCapasQue f pi))
    
    LADO DER
            cantidadCapasQueCumplen f (Capa i pi)
        =                           (def. cantidadCapasQueCumplen.2)
            fromEnum (f i) + cantidadCapasQueCumplen f pi
    
        |||subcaso f i = True||
        |
        |LADO IZQ (continuación)
        |    =                           (rama then del if)
        |        length (Capa i (soloLasCapasQue f pi))
        |    =                           (def. length.2)
        |        1 + length (soloLasCapasQue f pi)
        |    =                           (def. (.))
        |        1 + (length . soloLasCapasQue f) pi
        |    =                           (HI)
        |        1 + cantidadDeCapasQueCumplen f pi
        |
        |LADO DER (continuación)
        |        fromEnum True + cantidadCapasQueCumplen f pi
        |    =                           (def. fromEnum)
        |        1 + cantidadCapasQueCumplen f pi


        |||subcaso f i = False||
        |
        |LADO IZQ (continuación)
        |    =                           (rama else del if)
        |        length (soloLasCapasQue f pi)
        |    =                           (def. (.))
        |        (length . soloLasCapasQue f) pi
        |
        |LADO DER (continuación)
        |        fromEnum False + cantidadCapasQueCumplen f pi
        |    =                           (def. fromEnum)
        |        0 + cantidadCapasQueCumplen f pi
        |    =                           (aritmética)
        |        cantidadCapasQueCumplen f pi
        |    =                           (HI)
        |        (length . soloLasCapasQue f) pi





b)
        ∀f.∀p1.∀p2. cantidadCapasQueCumplen f (conCapasDe p1 p2)
                    = cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p2
        
        Dem.:
        Sea f una función de tipo Ingrediente -> Bool.
        Sean p y q elementos de Pizza. Por ppio. de ind. sobre la estructura de p.

        caso base, p = Prepizza
                cantidadCapasQueCumplen f (conCapasDe Prepizza q)
                    = cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f q
        
        caso inductivo, p = (Capa i p')
                HI) cantidadCapasQueCumplen f (conCapasDe p' q)
                    = cantidadCapasQueCumplen f p' + cantidadCapasQueCumplen f q
                TI) cantidadCapasQueCumplen f (conCapasDe (Capa i p') q)
                    = cantidadCapasQueCumplen f (Capa i p') + cantidadCapasQueCumplen f q


        ||caso base, p = Prepizza||

        LADO IZQ
                cantidadCapasQueCumplen f (conCapasDe Prepizza q)
            =                           (def. conCapasDe.1)
                cantidadCapasQueCumplen f q
        
        LADO DER
                cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f q
            =                           (def. cantidadCapasQueCumplen.1)
                0 + cantidadCapasQueCumplen f q
            =                           (aritmética)
                cantidadCapasQueCumplen f q
        


        ||caso inductivo p = (Capa i p')||

        LADO IZQ 
                cantidadCapasQueCumplen f (conCapasDe (Capa i p') q)
            =                           (def. conCapasDe.2)
                cantidadCapasQueCumplen f (Capa i (conCapasDe p' q))
            =                           (def. cantidadCapasQueCumplen.2)
                fromEnum (f i) + cantidadCapasQueCumplen f (conCapasDe p' q)

        LADO DER
                cantidadCapasQueCumplen f (Capa i p') + cantidadCapasQueCumplen f q
            =                           (def. cantidadCapasQueCumplen.2)
                fromEnum (f i) + cantidadCapasQueCumplen f p' + cantidadCapasQueCumplen f q
            =                           (HI)
                fromEnum (f i) + cantidadCapasQueCumplen f (conCapasDe p' q)

        


c)
        ∀f.∀p1.∀p2. conCapasTransformadas f (conCapasDe p1 p2) 
                    = conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2)

        Dem.:
        Sea f una función de Ingrediente -> Ingrediente. 
        Sean p y q elementos de Pizza. Por ppio. de ind. sobre la estructura de p.

        
        caso base, p = Prepizza
                conCapasTransformadas f (conCapasDe Prepizza q) 
                    = conCapasDe (conCapasTransformadas f Prepizza) (conCapasTransformadas f q)

        caso inductivo, p = (Capa i p')
                HI) conCapasTransformadas f (conCapasDe p' q) 
                    = conCapasDe (conCapasTransformadas f p') (conCapasTransformadas f q)
                TI) conCapasTransformadas f (conCapasDe (Capa i p') q) 
                    = conCapasDe (conCapasTransformadas f (Capa i p')) (conCapasTransformadas f q)



        ||caso base, p = Prepizza||

        LADO IZQ
                conCapasTransformadas f (conCapasDe Prepizza q)
            =                           (def. conCapasDe.1)
                conCapasTransformadas f q
        
        LADO DER
                conCapasDe (conCapasTransformadas f Prepizza) (conCapasTransformadas f q)
            =                           (def. conCapasTransformadas.1)
                conCapasDe Prepizza (conCapasTransformadas f q)
            =                           (def. conCapasDe.1)
                conCapasTransformadas f q



        ||caso inductivo, p = (Capa i p')||

        LADO IZQ
                conCapasTransformadas f (conCapasDe (Capa i p') q)
            =                           (def. conCapasDe.2)
                conCapasTransformadas f (Capa i (conCapasDe p' q))
            =                           (def. conCapasTransformadas.2)
                Capa (f i) (conCapasTransformadas f (conCapasDe p' q))
        

        LADO DER
                conCapasDe (conCapasTransformadas f (Capa i p')) (conCapasTransformadas f q)
            =                           (def. conCapasTransformadas.2)
                conCapasDe (Capa (f i) (conCapasTransformadas f p')) (conCapasTransformadas f q)
            =                           (def. conCapasDe.2)
                Capa (f i) (conCapasDe (conCapasTransformadas f p') (conCapasTransformadas f q))
            =                           (HI)
                Capa (f i) (conCapasTransformadas f (conCapasDe p' q))




d)
        ∀f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀p.∀f. (cantidadCapasQueCumplen f . soloLasCapasQue f) p = cantidadCapasQueCumplen f p
    
    Dem.:
    Sea f una función de Ingrediente -> Bool. Sea p un elemento de Pizza. 
    Por ppio. de ind. sobre la estructura de p. 

    caso base, p = Prepizza
            (cantidadCapasQueCumplen f . soloLasCapasQue f) Prepizza = cantidadCapasQueCumplen f Prepizza


    caso inductivo, p = (Capa i p')
            HI) (cantidadCapasQueCumplen f . soloLasCapasQue f) p' = cantidadCapasQueCumplen f p'
            TI) (cantidadCapasQueCumplen f . soloLasCapasQue f) (Capa i p') = cantidadCapasQueCumplen f (Capa i p')

    

    ||caso base, p = Prepizza||

            (cantidadCapasQueCumplen f . soloLasCapasQue f) Prepizza
        =                           (def. (.))
            cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza)
        =                           (def. soloLasCapasQue.1)
            cantidadCapasQueCumplen f Prepizza

    
    ||caso inductivo, p = (Capa i p')

    LADO IZQ
            (cantidadCapasQueCumplen f . soloLasCapasQue f) (Capa i p')
        =                           (def. (.))
            cantidadCapasQueCumplen f (soloLasCapasQue f (Capa i p'))
        =                           (def. soloLasCapasQue.2)
            cantidadCapasQueCumplen f (armarSiCumple f i (soloLasCapasQue f p'))
        =                           (def. armarSiCumple)
            cantidadCapasQueCumplen f (if f i then Capa i (soloLasCapasQue f p') else (soloLasCapasQue f p'))

    

    LADO DER
            cantidadCapasQueCumplen f (Capa i p')
        =                           (def. cantidadCapasQueCumplen.2)
            fromEnum (f i) + cantidadCapasQueCumplen f p'
        =                           (HI)
            fromEnum (f i) + (cantidadCapasQueCumplen f . soloLasCapasQue f) p'
        =                           (def. (.))
            fromEnum (f i) + cantidadCapasQueCumplen f (soloLasCapasQue f p')

    
        ||subcaso (f i) = True||
        |
        |LADO IZQ (continuación)
        |    =                           (rama then del if)
        |        cantidadCapasQueCumplen f (Capa i (soloLasCapasQue f p'))
        |    =                           (def. cantidadDeCapasQueCumplen.2)
        |        fromEnum (f i) + cantidadCapasQueCumplen f (soloLasCapasQue f p')
        |
        |LADO DER
        |        fromEnum (f i) + cantidadCapasQueCumplen f (soloLasCapasQue f p')

        
        ||subcaso (f i) = False||
        |
        |LADO IZQ (continuación)
        |    =                           (rama else del if)
        |        cantidadCapasQueCumplen f (soloLasCapasQue f p')
        |
        |LADO DER (continuación)
        |        fromEnum (False) + cantidadCapasQueCumplen f (soloLasCapasQue f p')
        |    =                           (def. fromEnum)
        |        0 + cantidadCapasQueCumplen f (soloLasCapasQue f p')
        |    =                           (neutro de la suma)
        |        cantidadCapasQueCumplen f (soloLasCapasQue f p')
            
-}  

-- ================================================================================= --
--                                  EJERCICIO 7                                      --
-- ================================================================================= --
--a)
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs


--b)
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x 
                    then x : filter f xs
                    else filter f xs

--c)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z   
foldr f z (x:xs) = f x (foldr f z xs)

--d)
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
recr z f []     = z 
recr z f (x:xs) = f x xs (recr z f xs)

--e)
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f []     = error "La lista es vacía." 
foldr1 f (x:xs) = if null xs then x else f x (foldr1 f xs) 

--f)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f []     ys = []
zipWith f (x:xs) ys = case ys of 
                    (y:ys') -> f x y : zipWith f xs ys
                    []      -> []

--g)
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z []     = [z]
scanr f z (x:xs) = let (b:bs) = (scanr f z xs)
                    in f x b : b : bs


-- ================================================================================= --
--                                  EJERCICIO 8                                      --
-- ================================================================================= --

{-
a)
        map id = id
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀xs. map id xs = id xs
    
    Dem.: 
    Sea zs una lista arbitraria. Por ppio. de ind. sobre la estructura de zs.

    caso base, zs = []
            map id [] = id []

    caso inductivo, zs = (z:zs')
            HI) map id zs' = id zs'
            TI) map id (z:zs') = id (z:zs')

    
    ||caso base, zs = []||

    LADO IZQ
            map id [] 
        =                           (def. map.1)
            []
    
    LADO DER
            id []
        =                           (def. id)
            []

    
    ||caso inductivo zs = (z:zs')||

    LADO IZQ
            map id (z:zs')
        =                           (def. map.2)
            id z : map id zs'
        =                           (HI)
            id z : id zs'
        =                           (def. id)
            z:zs'
    
    LADO DER
            id (z:zs')
        =                           (def. id)
            z:zs'
    




b)
        ∀f.∀g. map f . map g = map (f . g)
    Por ppio. de extensionalidad, es equivalente a demostrar que: 
        ∀xs. ∀f.∀g. (map f . map g) xs = map (f . g) xs
    
    Dem.:
    Sean f y g funciones arbitrarias. Sea zs una lista arbitraria.
    Por ppio. de ind. sobre la estructura de zs.

    caso base, zs = []
            (map f . map g) [] = map (f . g) []

    caso inductivo, zs = (z:zs')
            HI) (map f . map g) zs' = map (f . g) zs'
            TI) (map f . map g) (z:zs') = map (f . g) (z:zs')


    ||caso base, zs = []||

    LADO IZQ
            (map f . map g) []
        =                           (def. (.))
            map f (map g [])
        =                           (def. map.1)
            map f []
        =                           (def. map.1)
            []
    
    LADO DER
            map (f . g) []
        =                           (def. map.1)
            []

    
    ||caso inductivo, zs = (z:zs')||

    LADO IZQ
            (map f . map g) (z:zs')
        =                           (def. (.))
            map f (map g (z:zs'))
        =                           (def. map.2)
            map f (g z : map g zs')
        =                           (def. map.2)
            f (g z) : map f (map g zs')
    
    LADO DER
            map (f . g) (z:zs')
        =                           (def. map.2)
            (f . g) z : map (f . g) zs' 
        =                           (HI)
            (f . g) z : (map f . map g) zs'
        =                           (def. (.))
            f (g z) : map f (map g zs')




c)
        ∀f.∀xs.∀ys. map f (xs ++ ys) = map f xs ++ map f ys
    
    Dem.: 
    Sea f una funcion arbitraria. Sean ws y zs listas arbitrarias.
    Por ppio. de ind. sobre la estructura de ws.

    caso base, ws = []
            map f ([] ++ zs) = map f [] ++ map f zs
    
    caso inductivo, ws = (w:ws')
            HI) map f (ws' ++ zs) = map f ws' ++ map f zs
            TI) map f ((w:ws') ++ zs) = map f (w:ws') ++ map f zs


    ||caso base, ws = []||

    LADO IZQ
            map f ([] ++ zs)
        =                           (def. ++.1)
            map f zs

    LADO DER
            map f [] ++ map f zs
        =                           (def. map.1)
            [] ++ map f zs
        =                           (def. ++.1)
            map f zs

    

    ||caso inductivo, ws = (w:ws')||

    LADO IZQ
            map f ((w:ws') ++ zs)
        =                           (def. ++.2)
            map f (w : (ws' ++ zs))
        =                           (def. map.2)
            f w : map f (ws' ++ zs)
    
    LADO DER
            map f (w:ws') ++ map f zs
        =                           (def. map.2)
            f w : map f ws' ++ map f zs
        =                           (HI)
            f w : map f (ws' ++ zs)



d)
        ∀f. concat . map (map f) = map f . concat
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀xs.∀f. (concat . map (map f)) xs = (map f . concat) xs
    
    Dem.:
    Sea f una función arbitraria. Sea xss una lista de listas.
    Por ppio. de ind. sobre la estructura de xss.

    caso base, xss = []
            (concat . map (map f)) [] = (map f . concat) []
    
    caso inductivo, xss = (xs:xss')
            HI) (concat . map (map f)) xss' = (map f . concat) xss'
            TI) (concat . map (map f)) (xs:xss') = (map f . concat) (xs:xss')

    
    ||caso base, xss = []||

    LADO IZQ
            (concat . map (map f)) []
        =                           (def. (.))
            concat (map (map f) [])
        =                           (def. map.1)
            concat []
        =                           (def. concat.1)
            []

    LADO DER
            (map f . concat) []
        =                           (def. (.))
            map f (concat [])
        =                           (def. concat.1)
            map f []
        =                           (def. map.1)
            []


    ||caso inductivo, xss = (xs:xss')||

    LADO IZQ
            (concat . map (map f)) (xs:xss')
        =                           (def. (.))
            concat (map (map f) (xs:xss'))
        =                           (def. map.2)
            concat (map f xs : map (map f) xss')
        =                           (def. concat.2)
            map f xs ++ concat (map (map f) xss')
    
    LADO DER
            (map f . concat) (xs:xss')
        =                           (def. (.))
            map f (concat (xs:xss'))
        =                           (def. concat.2)
            map f (xs ++ (concat xss'))
        =                           (prop. ∀f.∀xs.∀ys. map f (xs ++ ys) = map f xs ++ map f ys)
            map f xs ++ map f (concat xss')
        =                           (def. (.))
            map f xs ++ (map f . concat) xss'
        =                           (HI)
            map f xs ++ (concat . map (map f)) xss'
        =                           (def. (.))
            map f xs ++ concat (map (map f) xss')




e)
        foldr ((+) . suma') 0 = sum . map suma'
    Por ppio. de extensionalidad es equivalente a demostrar que:
        ∀xs. foldr ((+) . suma') 0 xs = (sum . map suma') xs
    
    Dem.:
    Sea xs una lista de pares de enteros. Por ppio. de ind. sobre la estructura de xs.

    caso base, xs = []
        foldr ((+) . suma') 0 [] = (sum . map suma') []

    caso inductivo, xs = (x:xs')
        HI) foldr ((+) . suma') 0 xs' = (sum . map suma') xs'
        TI) foldr ((+) . suma') 0 (x:xs') = (sum . map suma') (x:xs')

    
    ||caso base, xs = []||

    LADO IZQ
            foldr ((+) . suma') 0 []
        =                           (def. foldr.1)
            0

    LADO DER
            (sum . map suma') []
        =                           (def. (.))
            sum (map suma' [])
        =                           (def. map.1)
            sum []
        =                           (def. sum.1)
            0


    ||caso inductivo, xs = (x:xs')||

    LADO IZQ
            foldr ((+) . suma') 0 (x:xs')
        =                           (def. foldr.2)
            ((+) . suma') x (foldr ((+) . suma') 0 xs')
        =                           (HI)
            ((+) . suma') x ((sum . map suma') xs')
        =                           (def. (.))
            (+) (suma' x) (sum (map suma' xs'))
        =                           (equivalencia infix del operador)
            suma' x + sum (map suma' xs)

    

    LADO DER
            (sum . map suma') (x:xs')
        =                           (def. (.))
            sum (map suma' (x:xs'))
        =                           (def. map.2)
            sum (suma' x : map suma' xs')
        =                           (def. sum.2)
            suma' x + sum (map suma' xs')

        
        
f)
        ∀f.∀z. foldr f z . foldr (:) [] = foldr f z
    Por ppio. de extensionalidad es equivalente a demostrar que:
        ∀xs.∀f.∀z. (foldr f z . foldr (:) []) xs = foldr f z xs
    
    Dem.:
    Sea ws una lista arbitraria. Sea g una función. Sea e un elemento cualquiera.
    Por ppio. de ind. sobre la estructura de ws.

    caso base, ws = []
            (foldr g e . foldr (:) []) [] = foldr g e []

    caso inductivo, ws = (w:ws')
            HI) (foldr g e . foldr (:) []) ws' = foldr g e ws'
            TI) (foldr g e . foldr (:) []) (w:ws') = foldr g e (w:ws')



    ||caso base, ws = []||

            (foldr g e . foldr (:) []) []
        =                           (def. (.))
            foldr g e (foldr (:) [] [])
        =                           (def. foldr.1)
            foldr g e []

    
    ||caso inductivo, ws = (w:ws')||

    LADO IZQ
            (foldr g e . foldr (:) []) (w:ws')
        =                           (def. (.))
            foldr g e (foldr (:) [] (w:ws'))
        =                           (def. foldr.2)
            foldr g e ((:) w (foldr (:) [] ws'))
        =                           (equivalencia infix del operador)
            foldr g e (w : (foldr (:) [] ws'))
        =                           (def. foldr.2)
            g w (foldr g e (foldr (:) [] ws'))
        =                           (def. (.))
            g w ((foldr g e . foldr (:) []) ws')
        =                           (HI)
            g w (foldr g e ws')

    
    LADO DER
            foldr g e (w:ws')
        =                           (def. foldr.2)
            g w (foldr g e ws')




g)
        ∀f.∀z.∀xs.∀ys. foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs
    
    Dem.:
    Sea g una función. Sea e un elemento. Sean ws y zs listas arbitrarias.
    Por ppio. de ind. sobre la estructura de ws.

    caso base, ws = []
            foldr g e ([] ++ zs) = foldr g (foldr g e zs) []

    caso inductivo, ws = (w:ws')
            HI) foldr g e (ws' ++ zs) = foldr g (foldr g e zs) ws'
            TI) foldr g e ((w:ws') ++ zs) = foldr g (foldr g e zs) (w:ws')

    

    ||caso base, ws = []||

    LADO IZQ
            foldr g e ([] ++ zs)
        =                           (def. ++.1)
            foldr g e zs

    LADO DER
            foldr g (foldr g e zs) []
        =                           (def. foldr.1)
            foldr g e zs


    ||caso inductivo, ws = (w:ws')||

    LADO IZQ
            foldr g e ((w:ws') ++ zs)
        =                           (def. ++.2)
            foldr g e (w : ws' ++ zs)
        =                           (def. foldr.2)
            g w (foldr g e (ws' ++ zs))

    LADO DER
            foldr g (foldr g e zs) (w:ws')
        =                           (def. foldr.2)
            g w (foldr g (foldr g e zs) ws')
        =                           (HI)
            g w (foldr g e (ws' ++ zs))



h)
        (+1) . foldr (+) 0 = foldr (+) 1
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀xs. ((+1) . foldr (+) 0) xs = foldr (+) 1 xs
    
    Dem.:
    Sea ws una lista arbitraria. Por ppio. de ind. sobre la estructura de ws.

    caso base, ws = []
            ((+1) . foldr (+) 0) [] = foldr (+) 1 []

    caso inductivo, ws = (w:ws')
            HI) ((+1) . foldr (+) 0) ws' = foldr (+) 1 ws'
            TI) ((+1) . foldr (+) 0) (w:ws') = foldr (+) 1 (w:ws')

    
    ||caso base, ws = []||

    LADO IZQ
            ((+1) . foldr (+) 0) []
        =                           (def. (.))
            (+1) (foldr (+) 0 [])
        =                           (def. foldr.1)
            (+1) 0
        =                           (def. +, con y<-1, x<-0)
            1

    LADO DER
            foldr (+) 1 []
        =                           (def. foldr.1)
            1

    
    ||caso inductivo, ws = (w:ws')||

    LADO IZQ
            ((+1) . foldr (+) 0) (w:ws')
        =                           (def. (.))
            (+1) (foldr (+) 0 (w:ws'))
        =                           (def. foldr.2)
            (+1) ((+) w (foldr (+) 0 ws'))
        =                           (equivalencia infix del operador suma)
            (w + (foldr (+) 0 ws')) + 1
        =                           (prop. conmutativa y asociativa de la suma)
            1 + w + (foldr (+) 0 ws')

    LADO DER
            foldr (+) 1 (w:ws')
        =                           (def. foldr.2)
            (+) w (foldr (+) 1 ws')
        =                           (HI)
            (+) w (((+1) . foldr (+) 0) ws')
        =                           (def. (.))
            (+) w ((+1) (foldr (+) 0 ws'))
        =                           (equivalencia infix del operador suma)
            w + (1 + (foldr (+) 0 ws'))
        =                           (prop. conmutativa y asociativa de la suma)
            1 + w + (foldr (+) 0 ws')




i)
        ∀n.∀f. many n f = foldr (.) id (replicate n f)

    Dem.: 
    Sea k un entero >= 0. Sea g una función arbitraria.
    Por ppio. de ind. sobre la estructura de k.

    caso base, k = 0
            many 0 g = foldr (.) id (replicate 0 g)

    caso inductivo, k > 0
            HI) many (k-1) g = foldr (.) id (replicate (k-1) g)    
            TI) many k g = foldr (.) id (replicate k g)



    ||caso base, k = 0||

    LADO IZQ
            many 0 g
        =                           (def. many.1)
            id
    
    LADO DER
            foldr (.) id (replicate 0 g)
        =                           (def. replicate.1)
            foldr (.) id []
        =                           (def. foldr.1)
            []

    
    ||caso inductivo, k > 0||

    LADO IZQ
            many k g
        =                           (def. many.2)
            g . many (k-1) g

    LADO DER
            foldr (.) id (replicate k g)
        =                           (def. replicate.2)
            foldr (.) id (g: replicate (k-1) g)
        =                           (def. foldr.2)
            (.) g (foldr (.) id (replicate (k-1) g))
        =                           (HI)
            (.) g (many (k-1) g)
        =                           (equivalencia infix del operador (.))
            g . many (k-1) g


j)
        ∀f.∀xs.∀ys. zipWith (flip f) xs ys = map (uncurry f) (flip zip xs ys)
    
    Dem.:
    Sea g una función. Sean ws y zs listas. Por ppio. de ind. sobre la estructura de ws.

    caso base, ws = []
            zipWith (flip g) [] zs = map (uncurry g) (flip zip [] zs)

    caso inductivo, ws = (w:ws')
            HI) zipWith (flip g) ws' zs = map (uncurry g) (flip zip ws' zs)
            TI) zipWith (flip g) (w:ws') zs = map (uncurry g) (flip zip (w:ws') zs)

    
    ||caso base, ws = []||

    LADO IZQ
            zipWith (flip g) [] zs
        =                           (def. zipWith.1)
            []
    
    LADO DER
            map (uncurry g) (flip zip [] zs)
        =                           (def. flip)
            map (uncurry g) (zip zs [])
        =                           (def. zip)
            map (uncurry g) []
        =                           (def. map.1)
            []

    
    ||caso inductivo, ws = (w:ws')||

    LADO IZQ
            zipWith (flip g) (w:ws') zs
    
    LADO DER
            map (uncurry g) (flip zip (w:ws') zs)
        =                           (def. flip)
            map (uncurry g) (zip zs (w:ws'))
    
        ||subcaso zs = []||
        |LADO IZQ (continuación)
        |        zipWith (flip g) (w:ws') zs
        |    =                           (def. zipWith.2, segunda ecuación del case)
        |        []
        |
        |LADO DER (continuación)
        |        map (uncurry g) (zip [] (w:ws'))
        |    =                           (def. zip.2)
        |        map (uncurry g) []
        |    =                           (def. map.1)
        |        []

        ||subcaso zs = (z:zs')||
        |LADO IZQ (continuación)
        |        zipWith (flip g) (w:ws') (z:zs')
        |    =                           (def. zipWith.2)
        |        flip g w z : zipWith (flip g) ws' zs'
        |    =                           (def. flip)
        |        g z w : zipWith (flip g) ws' zs'
        |    =                           (HI)
        |        g z w : map (uncurry g) (flip zip ws' zs')
        |    =                           (def. flip)
        |        g z w : map (uncurry g) (zip zs' ws')
        |
        |
        |LADO DER (continuación)
        |        map (uncurry g) (flip zip (w:ws') (z:zs'))
        |    =                           (def. flip)
        |        map (uncurry g) (zip (z:zs') (w:ws'))
        |    =                           (def. zip)
        |        map (uncurry g) ((z,w) : zip zs' ws')
        |    =                           (def. map.2)
        |        (uncurry g) (z,w) : map (uncurry g) (zip zs' ws')
        |    =                           (def. uncurry)
        |        g z w : map (uncurry g) (zip zs' ws')
            





k)
        ∀f.∀g.∀h.∀z.
            si ∀x.∀y. h (f x y) = g x (h y), entonces h . foldr f z = foldr g (h z)
    Por ppio. de extensionalidad es equivalente a demostrar que:
        ∀xs. (h . foldr f z) xs = foldr g (h z) xs
    
    Dem.:
    Sean f, g y h funciones. Sean x, y, z elementos. Sea xs una lista.
    Por ppio. de ind. sobre la estructura de xs.

    caso base, xs = []
            (h . foldr f z) [] = foldr g (h z) []
        
    caso inductivo, xs = (x':xs')
            HI) (h . foldr f z) xs' = foldr g (h z) xs'
            TI) (h . foldr f z) (x':xs') = foldr g (h z) (x':xs')

    
    
    ||caso base, xs = []||

    LADO IZQ
            (h . foldr f z) []
        =                           (def. (.))
            h (foldr f z [])
        =                           (def. foldr.1)
            h z

    LADO DER 
            foldr g (h z) []
        =                           (def. foldr.1)
            h z


    ||caso inductivo xs = (x':xs')||

    LADO IZQ
            (h . foldr f z) (x':xs')
        =                           (def. (.))
            h (foldr f z (x':xs'))
        =                           (def. foldr.2)
            h (f x' (foldr f z xs'))
    
    LADO DER
            foldr g (h z) (x':xs')
        =                           (def. foldr.2)
            g x' (foldr g (h z) xs')
        =                           (HI)
            g x' ((h . foldr f z) xs')
        =                           (def. (.))
            g x' (h (foldr f z xs'))


    
    Un ejemplo de este uso podría ser:
    h = succ, f = suma, g = (+), x :: Int, y :: Int, z :: Int
    El antecedente h (f x y) = g x (h y) quedaría:
        succ (suma x y) = (+) x (succ y)
    =                           (definiciones)
        1 + (x + y) = x + (1 + y)
    =                           (conmutatividad y asociatividad)
        1 + x + y = 1 + x + y

    Entonces, (succ . foldr suma 0) = foldr (+) (succ 0)
-}



-- ================================================================================= --
--                                  EJERCICIO 9                                      --
-- ================================================================================= --
--a)
sum :: [Int] -> Int 
sum = foldr (+) 0

--b)
length :: [a] -> Int 
length = foldr ((+) . const 1) 0

--c)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] 

--d)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr h []
        where h x r = if f x then x:r else r    -- si hubiera hecho filter = \f -> ..., la variable f no estaría dentro del scope del where

--e)
find :: (a -> Bool) -> [a] -> Maybe a 
find f = foldr (\ x r -> if f x then Just x else Nothing) Nothing

--f)
any :: (a -> Bool) -> [a] -> Bool 
any = \p -> foldr ((||) . p) False

--g)
all :: (a -> Bool) -> [a] -> Bool 
all = \p -> foldr ((&&) . p) True

--h)
countBy :: (a -> Bool) -> [a] -> Int 
countBy = \f -> foldr ((+) . fromEnum . f) 0

--i)
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\x (y,n) -> if f x then (x:y,n) else (y,x:n)) ([], [])

--j)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = foldr h (\ys -> [])
        where h x g ys = case ys of 
                            (y:ys') -> f x y : g ys'
                            _       -> []

--k)
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' = \f z -> foldr (\x (y:ys) -> f x y : y : ys) [z]

--l)
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = \f -> foldr (\x ys -> if f x then x : ys else []) []

--m)
take :: Int -> [a] -> [a]
take = flip (foldr h (\n -> []))
        where h x frec n = if n>0 then x : frec (n-1) else []

--n)
drop :: Int -> [a] -> [a]
drop = flip (foldr (\x fr n -> if n==0 then x:fr n else fr (n-1)) (\n -> []))

--o)
elemAt :: Int -> [a] -> a 
elemAt = flip (foldr (\x fr n -> if n==0 then x else fr (n-1)) 
                     (\n -> error "No hay un elemento en el índice dado"))





-- ================================================================================= --
--                                  EJERCICIO 10                                     --
-- ================================================================================= --

{-
a)
    filter :: (a -> Bool) -> [a] -> [a]
    id :: (b -> b)
    ----------------------------------- a <- b, b <- Bool
    filter id :: [Bool] -> [Bool]



b)
    map                   :: (a -> b) -> ([a] -> [b])
    (\x y z -> (x, y, z)) :: c -> d -> e -> (c,d,e)
    ------------------------------------------------- a <- c, b <- (d -> e -> (c,d,e))
    map (\x y z -> (x, y, z)) :: [c] -> [d -> e -> (c,d,e)]



c)
    map :: (a -> b) -> [a] -> [b]
    (+) :: Int -> Int -> Int
    ----------------------------- a <- Int, b <- (Int -> Int)
    map (+) :: [Int] -> [Int -> Int]



d)
    filter :: (a -> Bool) -> [a] -> [a]
    fst :: (b,c) -> b
    ----------------------------------- a <- (b,c), b <- Bool
    filter fst :: [(Bool, c)] -> [(Bool, c)]



e)
    filter :: (z -> Bool) -> [z] -> [z]
    (flip const (+)) :: a -> a 
    ----------------------------------- z <- a, a <- Bool 
    filter (flip const (+)) :: [Bool] -> [Bool]

    
    flip :: (a -> b -> c) -> b -> a -> c
    const :: a' -> b' -> a' 
    ------------------------------------ a' <- a, b'<- b, c <- a
    flip const :: b -> a -> a
    (+) :: Int -> Int -> Int
    ------------------------------------ b <- (Int -> Int -> Int)
    flip const (+) :: a -> a




f)
    map :: (a -> b) -> [a] -> [b]
    const :: c -> d -> c
    ----------------------------- c <- a, b <- (d -> c)
    map const :: [a] -> [d -> a]



g)
    map :: (a -> b) -> [a] -> [b]
    twice :: (a' -> a') -> a' -> a'
    ------------------------------- a <- (a' -> a'), b <- (a' -> a')
    map twice :: [a' -> a'] -> [a' -> a']




h)
    foldr :: (a -> b -> b) -> b -> [a] -> b
    twice :: (t -> t) -> t -> t
    --------------------------------------- a <- (t -> t), b <- t
    foldr twice :: t -> [t -> t] -> t




i)
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    fst :: (s,t) -> s
    --------------------------------------------- a <- (s,t), s <- (b -> c)
    zipWith fst :: [(b -> c, t)] -> [b] -> [c]




j)
    foldr                   :: (a -> b -> b) -> b -> [a] -> b
    (\x r z -> (x,z) : r z) :: p -> (q -> [(p,q)]) -> q -> [(p,q)]
    ------------------------------------------------------------ a <- p, b <- (q -> [(p,q)])
    foldr (\x r z -> (x,z) : r z) :: (q -> [(p,q)]) -> [p] -> q -> [(p,q)]
    (const []) :: d -> [c]
    -------------------------------------------------------------------- d <- q, c <- (p,q)
    foldr (\x r z -> (x,z) : r z) (const []) :: [p] -> q -> [(p,q)]
-}