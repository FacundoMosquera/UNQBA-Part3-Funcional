------------------------------- SECCIÓN 1 -------------------------------
data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa

{-
    EJERCICIO 1 

    El conjunto inductivo es Pizza. 
    La regla base es que existe Prepizza en el conjunto.
    La regla inductiva es que si p en forma parte de Pizza -> Capa i p, donde i es un ingrediente
arbitrario, también forma parte del conjunto.
-}


{-
    EJERCICIO 2

f :: Pizza -> a 
f Prepizza   = ...
f (Capa i p) = ... i ... f p

-}


--EJERCICIO 3
--a)
cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--b)
cantidadDeAceitunas :: Pizza -> Int 
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = nroDeAceitunas i + cantidadDeAceitunas p

nroDeAceitunas :: Ingrediente -> Int 
nroDeAceitunas (Aceitunas n) = n
nroDeAceitunas _ = 0


--c)
duplicarAceitunas :: Pizza -> Pizza 
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (dobleAceitunas i) (duplicarAceitunas p)

dobleAceitunas :: Ingrediente -> Ingrediente 
dobleAceitunas (Aceitunas n) = Aceitunas (n*2)
dobleAceitunas i = i

--d)
sinLactosa :: Pizza -> Pizza 
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = construirSinQueso i (sinLactosa p)

construirSinQueso :: Ingrediente -> Pizza -> Pizza 
construirSinQueso Queso p = p 
construirSinQueso i     p = Capa i p 

--e)
aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa Prepizza   = True
aptaIntolerantesLactosa (Capa i p) = not (esQueso i) && aptaIntolerantesLactosa p

esQueso :: Ingrediente -> Bool 
esQueso Queso = True 
esQueso _     = False

{-
    ∀p. si aptaIntolerantesLactosa p = True, entonces p = sinLactosa p
    HIP) aptaIntolerantesLactosa p = True

    #Caso p = Prepizza

    LADO IZQ                                           LADO DER

        Prepizza                                           sinLactosa Prepizza
                                                        =               (def. sinLactosa, 1° ecuación)
                                                            Prepizza 



    #Caso p = (Capa i subp)

    LADO IZQ                                           LADO DER

    Capa i subp                                          sinLactosa (Capa i subp)
                                                        =                   (def. sinLactosa, 2° ecuación)
                                                            construirSinQueso i subp
                                                        =                   (def. construirSinQueso, 2° ecuación al cumplirse debido a la hipótesis) 
                                                            Capa i subp
    
-}

--f)
conDescripcionMejorada :: Pizza -> Pizza 
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = compAceitunas i (conDescripcionMejorada p)

compAceitunas :: Ingrediente -> Pizza -> Pizza 
compAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m) p)
compAceitunas i p = Capa i p


{-
    EJERCICIO 4

a) cantidadDeAceitunas Prepizza = cantidadDeAceitunas (conDescripcionMejorada Prepizza)

LADO IZQ

    cantidadDeAceitunas Prepizza
    

LADO DER 

    cantidadDeAceitunas (conDescripcionMejorada Prepizza)
=                    (def. conDescripcionMejorada, 1° ecuación)
    cantidadDeAceitunas Prepizza



b) cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas 
                                                (conDescripcionMejorada (Capa Queso Prepizza))

LADO IZQ 

        cantidadDeAceitunas (Capa Queso Prepizza)


LADO DER

        cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
    =                               (def. conDescripcionMejorada, 2° ecuación)
        cantidadDeAceitunas (compAceitunas Queso Prepizza)
    =                               (def. compAceitunas, 2° ecuación)
        cantidadDeAceitunas (Capa Queso Prepizza)
    


--c) cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)) 
                        =
     cantidadDeAceitunas (conDescripciónMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))


LADO IZQ 

        cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))


LADO DER

        cantidadDeAceitunas (conDescripciónMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
    =                                   (def. conDescripcionMejorada, 2° ecuación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza))
    =                                   (def. conDescripcionMejorada, 2° ecuación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 8) (compAceitunas Queso Prepizza))
    =                                   (def. compAceitunas, 2° ecuación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 8) (Capa Queso Prepizza))
    =                                   (def. compAceitunas, 2° ecuación)
        cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))


    
--d) cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
                                =
    cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))


LADO IZQ

        cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
    =                                           (def. cantidadDeAceitunas, 2° ecuación) 
        nroDeAceitunas (Aceitunas 9) + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
    =                                           (def. cantidadDeAceitunas, 2° ecuación)
        nroDeAceitunas (Aceitunas 9) + nroDeAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
    =                                           (def. nroDeAceitunas, 1° ecuación)
        9 + 8 + cantidadDeAceitunas (Capa Queso Prepizza)
    =                                           (def. cantidadDeAceitunas, 2° ecuación)
        9 + 8 + nroDeAceitunas Queso + cantidadDeAceitunas Prepizza
    =                                           (def. cantidadDeAceitunas, 1° ecuación)
        9 + 8 + nroDeAceitunas Queso + 0
    =                                           (def. nroDeAceitunas, 2° ecuación)
        9 + 8 + 0 + 0
    =                                           (aritmética)
        17


LADO DER

        cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
    =                                                               (def. conDescripcionMejorada, 2° ecucación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 9) (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza))))
    =                                                               (def. conDescripcionMejorada, 2° ecucación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 9) (compAceitunas (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza)))
    =                                                               (def. conDescripcionMejorada, 2° ecucación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 9) (compAceitunas (Aceitunas 8) (compAceitunas Queso (conDescripcionMejorada Prepizza))))
    =                                                               (def. conDescripcionMejorada, 1° ecucación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 9) (compAceitunas (Aceitunas 8) (compAceitunas Queso Prepizza)))
    =                                                               (def. compAceitunas, 2° ecucación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 9) (compAceitunas (Aceitunas 8) (Capa Queso Prepizza)))
    =                                                               (def. compAceitunas, 2° ecucación)
        cantidadDeAceitunas (compAceitunas (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
    =                                                               (def. compAceitunas, 1° ecucación)
        cantidadDeAceitunas (Capa (Aceitunas (9+8)) (Capa Queso Prepizza))
    =                                                               (def. cantidadDeAceitunas, 2° ecuación)
        nroDeAceitunas (Aceitunas (9+8)) + cantidadDeAceitunas (Capa Queso Prepizza)
    =                                                               (def. cantidadDeAceitunas, 2° ecucación)
        nroDeAceitunas (Aceitunas (9+8)) + nroDeAceitunas Queso + cantidadDeAceitunas Prepizza
    =                                                               (def. cantidadDeAceitunas, 1° ecucación)
        nroDeAceitunas (Aceitunas (9+8)) + nroDeAceitunas Queso + 0
    =                                                               (def. nroDeAceitunas, 1° y 2° ecuación)
        (9+8) + 0 + 0
    =                                                               (aritmética)
        17
-}  