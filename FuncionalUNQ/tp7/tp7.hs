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
compAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
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













------------------------------- SECCIÓN 2 -------------------------------
--EJERCICIO 1
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre 
            | Investigador Nombre Equipo Equipo Equipo

{-
    Planilla es un conjunto inductivo, con una regla base y una regla inductiva.
    Su regla base es: Fin está en el conjunto.
    Su regla inductiva sería: si p está en el conjunto, entonces Registro Nombre p está en el conjunto.

    Equipo es un conjunto inductivo, con una regla base y una regla inductiva.
    Su regla base sería: Becario n está en el conjunto, donde n es de tipo Nombre.
    Su regla inductiva sería: si e1, e2, e3 están en el conjunto, entonces Investigador n e1, e2, e3 están
en el conjunto (donde n es de tipo Nombre).



--EJERCICIO 2

fp :: Planilla -> a
fp Fin            = ...
fp (Registro n p) = ... n ... fp p


fe :: Equipo -> a
fe (Becario n)               = ...
fe (Investigador n e1 e2 e3) = ... n ... fe e1 ... fe e2 ... fe e3

-}

--EJERCICIO 3
--a)
largoDePlanilla :: Planilla -> Int 
largoDePlanilla Fin            = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

--b)
esta :: Nombre -> Planilla -> Bool 
esta n Fin             = False
esta n (Registro np p) = n == np || esta n p

--c)
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p              = p
juntarPlanillas (Registro n p1) p2 = Registro n (juntarPlanillas p1 p2)

--d)
nivelesJerarquicos :: Equipo -> Int 
nivelesJerarquicos (Becario n)               = 1
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + max (nivelesJerarquicos e1) (max (nivelesJerarquicos e2) (nivelesJerarquicos e3))

--e)
cantidadDeIntegrantes :: Equipo -> Int 
cantidadDeIntegrantes (Becario n)               = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

--f)
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))




{-
    EJERCICIO 4

a) ∀p. largoDePlanilla (juntarPlanillas Fin p) = largoDePlanilla Fin + largoDePlanilla p


LADO IZQ
    
        largoDePlanilla (juntarPlanillas Fin p)
    =                       (def. juntarPlanillas, 1° ecuación)
        largoDePlanilla p


LADO DER 

        largoDePlanilla Fin + largoDePlanilla p 
    =                       (def. largoDePlanilla, 1° ecuación)
        0 + largoDePlanilla p
    =                       (neutro de la suma)
        largoDePlanilla p



b) ∀p. largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p) 
                        =
        largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p


LADO IZQ 

        largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p)
    =                                   (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro "Edsger" (juntarPlanillas Fin p))
    =                                   (def. juntarPlanillas. 1° ecuación)
        largoDePlanilla (Registro "Edsger" p)
    =                                   (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla p


LADO DER

        largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p 
    =                                   (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla Fin + largoDePlanilla p
    =                                   (def. largoDePlanilla, 1° ecuación)
        1 + 0 + largoDePlanilla p
    =                                   (neutro de la suma)
        1 + largoDePlanilla p




c)  ∀p. largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
                                =
        largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p

    

LADO IZQ 

        largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) p))
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin p)))
    =                                           (def. juntarPlanillas, 1° ecuación)
        largoDePlanilla (Registro "Alan" (Registro "Edsger" p))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla (Registro "Edsger" p)
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + largoDePlanilla p
    =                                           (aritmética)
        2 + largoDePlanilla p
    

LADO DER 

        largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + largoDePlanilla Fin + largoDePlanilla p
    =                                           (def. largoDePlanilla, 1° ecuación)
        1 + 1 + 0 + largoDePlanilla p
    =                                           (aritmética)
        2 + largoDePlanilla p





d) ∀p. largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p)
                                =
       largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p


LADO IZQ 

        largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p)
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
    =                                           (def. equivalencia demostrada en el punto anterior)
        1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p



LADO DER

        largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p

-}






{-
    EJERCICIO 5


a) largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) = cantidadDeIntegrantes (Becario "Alan")


LADO IZQ 

        largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) 
    =                                   (def. planillaDeIntegrantes, 1° ecuación)
        largoDePlanilla (Registro "Alan" Fin)
    =                                   (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla Fin
    =                                   (def. largoDePlanilla, 1° ecuación)
        1 + 0 
    =                                   (aritmética)
        1


LADO DER 

        cantidadDeIntegrantes (Becario "Alan")
    =                           (def. cantidadDeIntegrantes, 1° ecuación)
        1



b) igual al a


c) ∀n. largoDePlanilla (planillaDeIntegrantes (Becario n)) = cantidadDeIntegrantes (Becario n)



LADO IZQ 

        largoDePlanilla (planillaDeIntegrantes (Becario n)) 
    =                                   (def. planillaDeIntegrantes, 1° ecuación)
        largoDePlanilla (Registro n Fin)
    =                                   (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla Fin
    =                                   (def. largoDePlanilla, 1° ecuación)
        1 + 0 
    =                                   (aritmética)
        1


LADO DER 

        cantidadDeIntegrantes (Becario n)
    =                           (def. cantidadDeIntegrantes, 1° ecuación)
        1





d) largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
                                =
   cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))


LADO IZQ

        largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
    =                                           (def. planillaDeIntegrantes, 2° ecuación)
        largoDePlanilla (Registro "Alonzo" (juntarPlanillas (planillaDeIntegrantes (Becario "Alan")) 
                                                            (juntarPlanillas (planillaDeIntegrates (Becario "Alfred")) (planillaDeIntegrantes (Becario "Stephen")))))
    =                                           (def. planillaDeIntegrantes, 1° ecuación) x3
        largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" Fin) (juntarPlanillas (Registro "Alfred" Fin) (Registro "Stephen" Fin))))
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" Fin) (Registro "Alfred" (juntarPlanillas Fin (Registro "Stephen" Fin)))))
    =                                           (def. juntarPlanillas, 1° ecuación)
        largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" Fin) (Registro "Alfred" (Registro "Stephen" Fin))))
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro "Alonzo" (Registro "Alan" (juntarPlanillas Fin (Registro "Alfred" (Registro "Stephen" Fin))))) 
    =                                           (def. juntarPlanillas, 1° ecuación)
       largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin))))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + largoDePlanilla (Registro "Alfred" (Registro "Stephen" Fin))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + 1 + largoDePlanilla (Registro "Stephen" Fin)
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + 1 + 1 + largoDePlanilla Fin
    =                                           (def. largoDePlanilla, 1° ecuación)
        1 + 1 + 1 + 1 + 0
    =                                           (aritmética)
        4



LADO DER 

        cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
    =                                           (def. cantidadDeIntegrantes, 2° ecuacion)
        1 + cantidadDeIntegrantes (Becario "Alan") + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
    =                                           (def. cantidadDeIntegrantes, 1° ecuación) x3
        1 + 1 + 1 + 1
    =                                           (aritmética)
        4











e)  para todo n :: Nombre . para todo n1 :: Nombre .
    para todo n2 :: Nombre . para todo n3 :: Nombre .
        largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3)))
                                        = 
        cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))



    LADO IZQ

        largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3)))
    =                                           (def. planillaDeIntegrantes, 2° ecuación)
        largoDePlanilla (Registro n (juntarPlanillas (planillaDeIntegrantes (Becario n1)) 
                                                            (juntarPlanillas (planillaDeIntegrates (Becario n2)) (planillaDeIntegrantes (Becario n3)))))
    =                                           (def. planillaDeIntegrantes, 1° ecuación) x3
        largoDePlanilla (Registro n (juntarPlanillas (Registro n1 Fin) (juntarPlanillas (Registro n2 Fin) (Registro n3 Fin))))
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro n (juntarPlanillas (Registro n1 Fin) (Registro n2 (juntarPlanillas Fin (Registro n3 Fin)))))
    =                                           (def. juntarPlanillas, 1° ecuación)
        largoDePlanilla (Registro n (juntarPlanillas (Registro n1 Fin) (Registro n2 (Registro n3 Fin))))
    =                                           (def. juntarPlanillas, 2° ecuación)
        largoDePlanilla (Registro n (Registro n1 (juntarPlanillas Fin (Registro n2 (Registro n3 Fin))))) 
    =                                           (def. juntarPlanillas, 1° ecuación)
       largoDePlanilla (Registro n (Registro n1 (Registro n2 (Registro n3 Fin))))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + largoDePlanilla (Registro n1 (Registro n2 (Registro n3 Fin)))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + largoDePlanilla (Registro n2 (Registro n3 Fin))
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + 1 + largoDePlanilla (Registro n3 Fin)
    =                                           (def. largoDePlanilla, 2° ecuación)
        1 + 1 + 1 + 1 + largoDePlanilla Fin
    =                                           (def. largoDePlanilla, 1° ecuación)
        1 + 1 + 1 + 1 + 0
    =                                           (aritmética)
        4



LADO DER 

        cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))
    =                                           (def. cantidadDeIntegrantes, 2° ecuacion)
        1 + cantidadDeIntegrantes (Becario n1) + cantidadDeIntegrantes (Becario n2) + cantidadDeIntegrantes (Becario n3)
    =                                           (def. cantidadDeIntegrantes, 1° ecuación) x3
        1 + 1 + 1 + 1
    =                                           (aritmética)
        4




f)      largoDePlanilla
            (planillaDeIntegrantes
                (Investigador "Oswald"
                    (Investigador "Alonzo"  (Becario "Alan")
                                            (Becario "Alfred")
                                            (Becario "Stephen"))
                    (Investigador "John"    (Becario "Brian")
                                            (Becario "Graham")
                                            (Becario "Ioan"))
                    (Investigador "Robert"  (Becario "Gordon")
                                            (Becario "John")
                                            (Becario "Raymond"))))

                        =
        cantidadDeIntegrantes 
                (Investigador "Oswald"
                    (Investigador "Alonzo"  (Becario "Alan")
                                            (Becario "Alfred")
                                            (Becario "Stephen"))
                    (Investigador "John"    (Becario "Brian")
                                            (Becario "Graham")
                                            (Becario "Ioan"))
                    (Investigador "Robert"  (Becario "Gordon")
                                            (Becario "John")
                                            (Becario "Raymond"))))                    



LADO IZQ

        largoDePlanilla (planillaDeIntegrantes (Investigador "Oswald"
                                                    (Investigador "Alonzo"  (Becario "Alan")
                                                                            (Becario "Alfred")
                                                                            (Becario "Stephen"))
                                                    (Investigador "John"    (Becario "Brian")
                                                                            (Becario "Graham")
                                                                            (Becario "Ioan"))
                                                    (Investigador "Robert"  (Becario "Gordon")
                                                                            (Becario "John")
                                                                            (Becario "Raymond"))))
                                                                            
    =                                       (def. planillaDeIntegrantes, 2° ecuación)

        largoDePlanilla (Registro "Oswald" (juntarPlanilla (planillaDeIntegrantes (Investigador "Alonzo"  (Becario "Alan")
                                                                                                          (Becario "Alfred")
                                                                                                          (Becario "Stephen")))
                                                           (juntarPlanilla (planillaDeIntegrantes (Investigador "John" (Becario "Brian")
                                                                                                                       (Becario "Graham")
                                                                                                                       (Becario "Ioan")) ))
                                                                           (planillaDeIntegrantes (Investigador "Robert"  (Becario "Gordon")
                                                                                                                          (Becario "John")
                                                                                                                          (Becario "Raymond")))))))
    
    =                                       (def. largoDePlanilla, 2° ecuación)

        1 + largoDePlanilla (juntarPlanilla (planillaDeIntegrantes (Investigador "Alonzo"  (Becario "Alan")
                                                                                           (Becario "Alfred")
                                                                                           (Becario "Stephen")))
                                            (juntarPlanilla (planillaDeIntegrantes (Investigador "John" (Becario "Brian")
                                                                                                        (Becario "Graham")
                                                                                                        (Becario "Ioan")) ))
                                                            (planillaDeIntegrantes (Investigador "Robert"  (Becario "Gordon")
                                                                                                           (Becario "John")
                                                                                                           (Becario "Raymond")))))))
    



LADO DER

        cantidadDeIntegrantes 
                (Investigador "Oswald"
                    (Investigador "Alonzo"  (Becario "Alan")
                                            (Becario "Alfred")
                                            (Becario "Stephen"))
                    (Investigador "John"    (Becario "Brian")
                                            (Becario "Graham")
                                            (Becario "Ioan"))
                    (Investigador "Robert"  (Becario "Gordon")
                                            (Becario "John")
                                            (Becario "Raymond"))))

    =                                   (def. cantidadDeIntegrantes, 2° ecuación)

        1 + cantidadDeIntegrantes (Investigador "Alonzo"  (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
          + cantidadDeIntegrantes (Investigador "John"    (Becario "Brian") (Becario "Graham") (Becario "Ioan"))
          + cantidadDeIntegrantes (Investigador "Robert"  (Becario "Gordon") (Becario "John") (Becario "Raymond"))

    =                                   (equivalente a la demostración del punto e)

        1 + largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
          + largoDePlanilla (planillaDeIntegrantes (Investigador "John"   (Becario "Brian") (Becario "Graham") (Becario "Ioan")))
          + largoDePlanilla (planillaDeIntegrantes (Investigador "Robert" (Becario "Gordon") (Becario "John") (Becario "Raymond")))
    
    =                                   (LEMA1: largoDePlantilla (planillaDeIntegrantes e1) + largoDePlanilla (planillaDeIntegrantes e2) = 
                                                    largoDePlantilla (juntarPlanilla (planillaDeIntegrantes e1) (planillaDeIntegrantes e2)))

        1 + largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
          + largoDePlanilla (juntarPlanilla (planillaDeIntegrantes (Investigador "John"   (Becario "Brian") (Becario "Graham") (Becario "Ioan"))))
                                            (planillaDeIntegrantes (Investigador "Robert" (Becario "Gordon") (Becario "John") (Becario "Raymond")))

    =                                   (LEMA1)

-}








































------------------------------- SECCIÓN 3 -------------------------------
--EJERCICIO 1
data Dungeon a = Habitacion a 
               | Pasaje (Maybe a) (Dungeon a)
               | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

{-
    Dungeon a es un conjunto inductivo con una regla base y dos reglas inductivas.
    Regla base: Habitacion a forma parte de Dungeon a.
    Regla inductiva 1: Si d forma parte de Dungeon a, entonces Pasaje (Maybe a) d también forma parte.
    Regla inductiva 2: Si d1, d2 forman parte de Dungeon a, entonces Bifurcacion (Maybe a) d1 d2 también forma parte.
-}


{-
    EJERCICIO 2

fD :: Dungeon a -> b
fD (Habitacion x)         = ...
fD (Pasaje mx d)          = ... mx ... fD d 
fD (Bifurcacion mx d1 d2) = ... mx ... fD d1 ... fD d2
-}



--EJERCICIO 3
--a)
cantidadDeBifurcaciones :: Dungeon a -> Int 
cantidadDeBifurcaciones (Habitacion x)         = 0
cantidadDeBifurcaciones (Pasaje mb d)          = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion mb d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

--b)
cantidadDePuntosInteresantes :: Dungeon a -> Int 
cantidadDePuntosInteresantes (Habitacion x)         = 0
cantidadDePuntosInteresantes (Pasaje mb d)          = 1 + cantidadDeBifurcaciones d
cantidadDePuntosInteresantes (Bifurcacion mb d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

--c)
cantidadDePuntosVacios :: Dungeon a -> Int 
cantidadDePuntosVacios (Habitacion x)         = 0
cantidadDePuntosVacios (Pasaje mb d)          = (1 - unoSiHay mb) + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion mb d1 d2) = (1 - unoSiHay mb) + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

unoSiHay :: Maybe a -> Int 
unoSiHay (Just _) = 1 
unoSiHay _        = 0


--d)
cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon e (Habitacion x)         = unoSi (e==x)
cantidadDePuntosCon e (Pasaje mb d)          = unoSi (contieneA mb e) + cantidadDePuntosCon e d
cantidadDePuntosCon e (Bifurcacion mb d1 d2) = unoSi (contieneA mb e) + cantidadDePuntosCon e d1 + cantidadDePuntosCon e d2


contieneA :: Eq a => Maybe a -> a -> Bool 
contieneA (Just x) e = e == x
contieneA _        _ = False

unoSi :: Bool -> Int 
unoSi True  = 1
unoSi False = 0


--e)
esLineal :: Dungeon a -> Bool 
esLineal (Habitacion _)        = True
esLineal (Pasaje _ d)          = esLineal d
esLineal (Bifurcacion _ d1 d2) = False  

--f)
llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe e (Habitacion x)         = e == x
llenoDe e (Pasaje mb d)          = contieneA mb e && llenoDe e d
llenoDe e (Bifurcacion mb d1 d2) = contieneA mb e && llenoDe e d1 && llenoDe e d2




-- EJERCICIO 4
data Tesoro = Cofre | Oro | Joyas


{-
a)  ∀x :: a. cantidadDePuntosVacios (Habitacion x) = 0
 
 Dem.
        cantidadDePuntosVacios (Habitacion x)
    =                           (def. cantidadDePuntosVacios, 1° ecuación)
        0








b)

    cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) = 1

Dem.

        cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                       (def. cantidadDePuntosVacios, 2° ecuación)
        (1 - unoSiHay Nothing) + cantidadDePuntosVacios (Habitacion Joyas)
    =                                       (LEMA punto a)
        (1 - unoSiHay Nothing) + 0
    =                                       (def. unoSiHay, 2° ecuación)
        (1 - 0) + 0
    =                                       (aritmética)
        1






c)  ∀x :: a, ∀y :: a. cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x)) = 0

Dem. 

        cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    =                                       (def. cantidadDePuntosVacios, 2° ecuación)
        (1 - unoSiHay (Just y)) + cantidadDePuntosVacios (Habitacion x)
    =                                       (LEMA punto a)
        (1 - unoSiHay (Just y)) + 0
    =                                       (def. unoSiHay, 1° ecuación)
        (1 - 1) + 0
    =                                       (aritmética)
        0







d)  cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing    (Habitacion Joyas))
                                                (Pasaje (Just Oro) (Habitacion Cofre))) = 2


Dem.

        cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas))
                                                    (Pasaje (Just Oro) (Habitacion Cofre)))

    =                                       (def. cantidadDePuntosVacios, 3° ecuación)

        (1 - unoSiHay Nothing) + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
                               + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))

    =                                       (LEMA punto b)

        (1 - unoSiHay Nothing) + 1 + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
    =                                       (LEMA punto c)
        (1 - unoSiHay Nothing) + 1 + 0
    =                                       (def. unoSiHay, 2° ecuación)
        (1 - 0) + 1 + 0
    =                                       (aritmética)
        2










e)  ∀z :: a, ∀y :: a, ∀x :: a.
        cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z))
                                                    (Pasaje (Just y) (Habitacion x)))  = 2


Dem. 

        cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z))
                                                    (Pasaje (Just y) (Habitacion x)))

    =                                       (def. cantidadDePuntosVacios, 3° ecuación)            

        (1 - unoSiHay Nothing) + cantidadDePuntosVacios (Pasaje Nothing (Habitacion z))
                               + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))

    =                                       (LEMA del punto c)
        (1 - unoSiHay Nothing) + cantidadDePuntosVacios (Pasaje Nothing (Habitacion z)) + 0
    =                                       (LEMA1)
        (1 - unoSiHay Nothing) + 1 + 0
    =                                       (def. unoSiHay, 2° ecuación)
        (1 - 0) + 1 + 0
    =                                       (aritmética)
        2



LEMA1: ∀x :: a. cantidadDePuntosVacios (Pasaje Nothing (Habitacion x)) = 1

Dem. 

        cantidadDePuntosVacios (Pasaje Nothing (Habitacion x))
    =                                       (def. cantidadDePuntosVacios, 2° ecuación)
        (1 - unoSiHay Nothing) + cantidadDePuntosVacios (Habitacion x)
    =                                       (def. cantidadDePuntosVacios, 1° ecuación)
        (1 - unoSiHay Nothing) + 0
    =                                       (def. unoSiHay, 2° ecuación)
        (1 - 0) + 0
    =                                       (aritmética)
        1








f) cantidadDePuntosVacios (Bifurcacion (Just Cofre) 
                                       (Bifurcacion Nothing (Pasaje Nothing    (Habitacion Joyas))
                                                            (Pasaje (Just Oro) (Habitacion Cofre)))
                                       (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro))
                                                            (Pasaje Nothing    (Habitacion Joyas))))
                                        = 4



Dem. 

        cantidadDePuntosVacios (Bifurcacion (Just Cofre) 
                                       (Bifurcacion Nothing (Pasaje Nothing    (Habitacion Joyas))
                                                            (Pasaje (Just Oro) (Habitacion Cofre)))
                                       (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro))
                                                            (Pasaje Nothing    (Habitacion Joyas))))

    =                                       (def. cantidadDePuntosVacios, 3° ecuación)

        (1 - unoSiHay (Just Cofre)) + cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing    (Habitacion Joyas))
                                                                                  (Pasaje (Just Oro) (Habitacion Cofre)))
                                    + cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro))
                                                                                  (Pasaje Nothing    (Habitacion Joyas)))
    
    =                                       (LEMA2, ∀x :: Maybe a, ∀d1 :: Dungeon a, ∀d2 :: Dungeon a. cantidadDePuntosVacios (Bifurcacion x d1 d2)
                                                                                             = cantidadDePuntosVacios (Bifurcacion x d2 d1))

        (1 - unoSiHay (Just Cofre)) + cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing    (Habitacion Joyas))
                                                                                  (Pasaje (Just Oro) (Habitacion Cofre)))
                                    + cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing    (Habitacion Joyas))        
                                                                                  (Pasaje (Just Oro) (Habitacion Oro)))

    =                                       (LEMA del punto e) x2
        (1 - unoSiHay (Just Cofre)) + 2 + 2
    =                                       (def. unoSiHay, 1° ecuación)
        (1 - 1) + 2 + 2
    =                                       (aritmética)
        4                 
-}






