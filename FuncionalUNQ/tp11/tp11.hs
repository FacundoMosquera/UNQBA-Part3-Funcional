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