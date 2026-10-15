-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --

data EA = Const Int | BOp BinOp EA EA 
data BinOp = Sum | Mul

data ExpA = Cte Int 
          | Suma ExpA ExpA
          | Prod ExpA ExpA

evalExpA :: ExpA -> Int 
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

--a)
evalEA :: EA -> Int 
evalEA (Const n)       = n
evalEA (BOp bop e1 e2) = evalBinOp bop (evalEA e1) (evalEA e2)

evalBinOp :: BinOp -> (Int -> Int -> Int)
evalBinOp Sum = (+)
evalBinOp Mul = (*)

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)       = Cte n
ea2ExpA (BOp bop e1 e2) = binOp2ExpAC bop (ea2ExpA e1) (ea2ExpA e2)

binOp2ExpAC :: BinOp -> (ExpA -> ExpA -> ExpA)
binOp2ExpAC Sum = Suma
binOp2ExpAC Mul = Prod

expA2ea :: ExpA -> EA 
expA2ea (Cte n)      = (Const n)
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)


{-
b)
    i)
            ea2ExpA . expA2ea = id
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀x. (ea2ExpA . expA2ea) x = id x
        Sea e un elemento de ExpA. Por ppio. de ind. sobre la estructura de e.

        caso base, e = Cte n
                (ea2ExpA . expA2ea) (Cte n) = id (Cte n)

        caso inductivo 1, e = (Suma e1 e2)
                HI1) (ea2ExpA . expA2ea) e1 = id e1
                HI2) (ea2ExpA . expA2ea) e2 = id e2
                TI)  (ea2ExpA . expA2ea) (Suma e1 e2) = id (Suma e1 e2)

        caso inductivo 2, e = (Prod e1 e2)
                HI1) (ea2ExpA . expA2ea) e1 = id e1
                HI2) (ea2ExpA . expA2ea) e2 = id e2
                TI)  (ea2ExpA . expA2ea) (Prod e1 e2) = id (Prod e1 e2)


        ||caso base, e = Cte n||

        LADO IZQ
                (ea2ExpA . expA2ea) (Cte n)
            =                       (def. (.))
                ea2ExpA (expA2ea (Cte n))
            =                       (def. expA2ea.1)
                ea2ExpA (Const n)
            =                       (def. ea2ExpA.1)
                Cte n

        LADO DER 
                id (Cte n)
            =                       (def. id)
                Cte n

        
        ||caso inductivo 1, e = Suma e1 e2||
        
        LADO IZQ
                (ea2ExpA . expA2ea) (Suma e1 e2)
            =                       (def. (.))
                ea2ExpA (expA2ea (Suma e1 e2))
            =                       (def. expA2ea.2)
                ea2ExpA (BOp Sum (expA2ea e1) (expA2ea e2))
            =                       (def. ea2ExpA.2)
                binOp2ExpAC Sum (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
            =                       (def. biOp2ExpAC.1)
                Suma (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
            =                       (def. (.))
                Suma ((ea2ExpA . expA2ea) e1) ((ea2ExpA . expA2ea) e2)
            =                       (HI1, HI2)
                Suma (id e1) (id e2)
            =                       (def. id)
                Suma e1 e2

        LADO DER
                id (Suma e1 e2)
            =                       (def. id)
                Suma e1 e2


        ||caso inductivo 2, e = Prod e1 e2||
        
        LADO IZQ
                (ea2ExpA . expA2ea) (Prod e1 e2)
            =                       (def. (.))
                ea2ExpA (expA2ea (Prod e1 e2))
            =                       (def. expA2ea.2)
                ea2ExpA (BOp Mul (expA2ea e1) (expA2ea e2))
            =                       (def. ea2ExpA.2)
                binOp2ExpAC Mul (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
            =                       (def. biOp2ExpAC.2)
                Prod (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
            =                       (def. (.))
                Prod ((ea2ExpA . expA2ea) e1) ((ea2ExpA . expA2ea) e2)
            =                       (HI1, HI2)
                Prod (id e1) (id e2)
            =                       (def. id)
                Prod e1 e2

        LADO DER
                id (Prod e1 e2)
            =                       (def. id)
                Prod e1 e2
            

    




    ii)
            expA2ea . ea2ExpA = id
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀x. (expA2ea . ea2ExpA) x = id x
        
        Dem.:
        Sea e un elemento de EA. Por ppio. de ind. sobre la estructura de e.

        caso base, e = Const n
                (expA2ea . ea2ExpA) (Const n) = id (Const n)

        caso inductivo, e = BOp bo e1 e2
                HI1) (expA2ea . ea2ExpA) e1 = id e1
                HI2) (expA2ea . ea2ExpA) e2 = id e2  
                TI)  (expA2ea . ea2ExpA) (BOp bo e1 e2) = id (BOp bo e1 e2)



        ||caso base, e = Const n||
        
        LADO IZQ  
                (expA2ea . ea2ExpA) (Const n)
            =                       (def. (.))
                expA2ea (ea2ExpA (Const n))
            =                       (def. ea2ExpA.1)
                expA2ea (Cte n)
            =                       (def. expA2ea.1)
                Const n
        

        LADO DER
                id (Const n)
            =                       (def. id)
                Const n


    
    ||caso inductivo, e = BOp bo e1 e2||
    
    LADO IZQ
            (expA2ea . ea2ExpA) (BOp bo e1 e2)
        =                           (def. (.))
            expA2ea (ea2ExpA (BOp bo e1 e2))
        
        ||subcaso bo = Sum||
                expA2ea (ea2ExpA (BOp Sum e1 e2))
            =                           (def. ea2ExpA.2)
                expA2ea (binOp2ExpAC Sum (ea2ExpA e1) (ea2ExpA e2))
            =                           (def. binOp2ExpAC.1)
                expA2ea (Suma (ea2ExpA e1) (ea2ExpA e2))
            =                           (def. expA2ea.2)
                BOp Sum (expA2ea (ea2ExpA e1) (expA2ea (ea2ExpA e2)))
            =                           (def. (.))
                BOp Sum ((expA2ea . ea2ExpA) e1) ((expA2ea . ea2ExpA) e2)
            =                           (HI1, HI2)
                BOp Sum (id e1) (id e2)
            =                           (def. id)
                BOp Sum e1 e2
            =                           (def. id)
                id (BOp Sum e1 e2)
        
        ||subcaso bo = Mul||
                expA2ea (ea2ExpA (BOp Mul e1 e2))
            =                           (def. ea2ExpA.2)
                expA2ea (binOp2ExpAC Mul (ea2ExpA e1) (ea2ExpA e2))
            =                           (def. binOp2ExpAC.2)
                expA2ea (Prod (ea2ExpA e1) (ea2ExpA e2))
            =                           (def. expA2ea.3)
                BOp Mul (expA2ea (ea2ExpA e1) (expA2ea (ea2ExpA e2)))
            =                           (def. (.))
                BOp Mul ((expA2ea . ea2ExpA) e1) ((expA2ea . ea2ExpA) e2)
            =                           (HI1, HI2)
                BOp Mul (id e1) (id e2)
            =                           (def. id)
                BOp Mul e1 e2
            =                           (def. id)
                id (BOp Mul e1 e2)



    iii)
            evalExpA . ea2ExpA = evalEA
        Por ppio. de extensionalidad, es equivalente a demostrar que: 
            ∀x. (evalExpA . ea2ExpA) x = evalEA x
        
        Dem.:
        Sea e un elemento de EA. Por ppio. de ind. sobre la estructura de e.

        caso base e = Const n
                (evalExpA . ea2ExpA) (Const n) = evalEA (Const n)
        
        caso inductivo e = BOp bo e1 e2
                HI1) (evalExpA . ea2ExpA) e1 = evalEA e1
                HI2) (evalExpA . ea2ExpA) e2 = evalEA e2
                TI)  (evalExpA . ea2ExpA) (BOp bo e1 e2) = evalEA (BOp bo e1 e2)

        
        ||caso base e = Const n||

        LADO IZQ
                (evalExpA . ea2ExpA) (Const n)
            =                       (def. (.))
                evalExpA (ea2ExpA (Const n))
            =                       (def. ea2ExpA.1)
                evalExpA (Cte n)
            =                       (def. evalExpA.1)
                n
        
        LADO DER
                evalEA (Const n)
            =                       (def. evalEA.1)
                n


        ||caso inductivo, e = BOp bo e1 e2||

                (evalExpA . ea2ExpA) (BOp bo e1 e2)
            =                       (def. (.))
                evalExpA (ea2ExpA (BOp bo e1 e2))
            
            ||subcaso bo = Sum||
                
            LADO IZQ
                    evalExpA (ea2ExpA (BOp Sum e1 e2))
                =                       (def. ea2ExpA.2)
                    evalExpA (binOp2ExpAC Sum (ea2ExpA e1) (ea2ExpA e2))
                =                       (def. binOp2ExpAC.1)
                    evalExpA (Suma (ea2ExpA e1) (ea2ExpA e2))
                =                       (def. evalExpA.2)
                    evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)
                =                       (def. (.))
                    (evalExpA . ea2ExpA) e1 + (evalExpA . ea2ExpA) e2
                =                       (HI1, HI2)
                    evalEA e1 + evalEA e2
            
            LADO DER
                    evalEA (BOp Sum e1 e2)
                =                       (def. evalEA.2)
                    evalBinOp Sum (evalEA e1) (evalEA e2)
                =                       (def. evalBinOp.1)
                    (+) (evalEA e1) (evalEA e2)
                =                       (equivalencia de función infix)
                    evalEA e1 + evalEA e2
            


            ||subcaso bo = Mul||
            
            LADO IZQ 
                    evalExpA (ea2ExpA (BOp Mul e1 e2))
                =                       (def. ea2ExpA.2)
                    evalExpA (binOp2ExpAC Mul (ea2ExpA e1) (ea2ExpA e2))
                =                       (def. binOp2ExpAC.2)
                    evalExpA (Prod (ea2ExpA e1) (ea2ExpA e2))
                =                       (def. evalExpA.3)
                    evalExpA (ea2ExpA e1) * evalExpA (ea2ExpA e1)
                =                       (def. (.))
                    (evalExpA . ea2ExpA) e1 * (evalExpA . ea2ExpA) e2
                =                       (HI1, HI2)
                    evalEA e1 * evalEA e2
            

            LADO DER
                    evalEA (BOp Mul e1 e2)
                =                       (def. evalEA)
                    evalBinOp Mul (evalEA e1) (evalEA e2)
                =                       (def. evalBinOp.2)
                    (*) (evalEA e1) (evalEA e2)
                =                       (infix de la función (*))
                    evalEA e1 * evalEA e2 

    





    iv)
            evalEA . expA2ea = evalExpA
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀x. (evalEA . expA2ea) x = evalExpA x
        
        Dem.:
        Sea e un elemento de ExpA. Por ppio. de ind. sobre la estructura de e.

        caso base, e = Cte n
                (evalEA . expA2ea) (Cte n) = evalExpA (Cte n)
        
        caso inductivo 1, e = (Suma e1 e2)
                HI1) (evalEA . expA2ea) e1 = evalExpA e1
                HI2) (evalEA . expA2ea) e2 = evalExpA e2
                TI)  (evalEA . expA2ea) (Suma e1 e2) = evalExpA (Suma e1 e2)

        caso inductivo 2, e = (Prod e1 e2)
                HI1) (evalEA . expA2ea) e1 = evalExpA e1
                HI2) (evalEA . expA2ea) e2 = evalExpA e2
                TI)  (evalEA . expA2ea) (Prod e1 e2) = evalExpA (Prod e1 e2)





        ||caso base, e = Cte n||

        LADO IZQ    
                (evalEA . expA2ea) (Cte n)
            =                       (def. (.))
                evalEA (expA2ea (Cte n))
            =                       (def. expA2ea.1)
                evalEA (Const n)
            =                       (def. evalEA)
                n
        

        LADO DER
                evalExpA (Cte n)
            =                       (def. evalExpA.1)
                n

        
        ||caso inductivo 1, e = Suma e1 e2||

        LADO IZQ
                (evalEA . expA2ea) (Suma e1 e2)
            =                       (def. (.))
                evalEA (expA2ea (Suma e1 e2))
            =                       (def. expA2ea.2)
                evalEA (BOp Sum (expA2ea e1) (expA2ea e2))
            =                       (def. evalEA.2)
                evalBinOp Sum (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
            =                       (def. evalBinOp)
                (+) (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
            =                       (def. (.))
                (+) ((evalEA . expA2ea) e1) ((evalEA . expA2ea) e2)
            =                       (HI1, HI2)
                (+) (evalExpA e1) (evalExpA e2)
            =                       (equivalencia infix operador)
                evalExpA e1 + evalExpA e2
        

        LADO DER
                evalExpA (Suma e1 e2)
            =                       (def. evalExpA.2)
                evalExpA e1 + evalExpA e2

        


        ||caso inductivo 2, e = Prod e1 e2||
        
        LADO IZQ    
                (evalEA . expA2ea) (Prod e1 e2)
            =                       (def. (.))
                evalEA (expA2ea (Prod e1 e2))
            =                       (def. expA2ea.3)
                evalEA (BOp Mul (expA2ea e1) (expA2ea e2))
            =                       (def. evalEA.2)
                evalBinOp Mul (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
            =                       (def. evalBinOp.2)
                (*) (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
            =                       (def. (.))
                (*) ((evalEA . expA2ea) e1) ((evalEA . expA2ea) e2)
            =                       (HI1, HI2)
                (*) (evalExpA e1) (evalExpA e2)
            =                       (equivalencia operador infix)
                evalExpA e1 * evalExpA e2
        

        LADO DER
                evalExpA (Prod e1 e2)
            =                       (def. evalExpA.3)
                evalExpA e1 * evalExpA e2
-}






-- ================================================================================= --
--                                  EJERCICIO 2                                      --
-- ================================================================================= --

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

--a)
cantidadDeHojas :: Arbol a b -> Int 
cantidadDeHojas (Hoja _)       = 1
cantidadDeHojas (Nodo _ t1 t2) = cantidadDeHojas t1 + cantidadDeHojas t2

cantidadDeNodos :: Arbol a b -> Int 
cantidadDeNodos (Hoja _)       = 0
cantidadDeNodos (Nodo _ t1 t2) = 1 + cantidadDeNodos t1 + cantidadDeNodos t2

cantidadDeConstructores :: Arbol a b -> Int 
cantidadDeConstructores (Hoja _) = 1
cantidadDeConstructores (Nodo _ t1 t2) = 1 + cantidadDeConstructores t1 + cantidadDeConstructores t2

ea2Arbol :: EA -> Arbol BinOp Int 
ea2Arbol (Const n)      = Hoja n
ea2Arbol (BOp bo e1 e2) = Nodo bo (ea2Arbol e1) (ea2Arbol e2)



{-b)
        ∀t:: Arbol a b. cantidadDeHojas t + cantidadDeNodos t = cantidadDeConstructores t
    
    Dem.:
    Sea t' un elemento de Arbol a b. Por ppio. de ind. sobre la estructura de t'.

    caso base, t' = Hoja x
            cantidadDeHojas (Hoja x) + cantidadDeNodos (Hoja x) = cantidadDeConstructores (Hoja x)
    
    caso inductivo, t' = (Nodo y t1 t2)
            HI1) cantidadDeHojas t1 + cantidadDeNodos t1 = cantidadDeConstructores t1
            HI2) cantidadDeHojas t2 + cantidadDeNodos t2 = cantidadDeConstructores t2
            TI)  cantidadDeHojas (Nodo y t1 t2) + cantidadDeNodos (Nodo y t1 t2)
                        = cantidadDeConstructores (Nodo y t1 t2)
        
    
    ||caso base, t' = Hoja x||

    LADO IZQ
            cantidadDeHojas (Hoja x) + cantidadDeNodos (Hoja x)
        =                           (def. cantidadDeHojas.1)
            1 + cantidadDeNodos (Hoja x)
        =                           (def. cantidadDeNodos.1)
            1 + 0
        =                           (aritmética)
            1
    

    LADO DER 
            cantidadDeConstructores (Hoja x)
        =                           (def. cantidadDeConstructores.1)
            1
    

    ||caso inductivo, t' = (Nodo y t1 t2)||

    LADO IZQ
            cantidadDeHojas (Nodo y t1 t2) + cantidadDeNodos (Nodo y t1 t2)
        =                           (def. cantidadDeHojas.2)
            cantidadDeHojas t1 + cantidadDeHojas t2 + cantidadDeNodos (Nodo y t1 t2)
        =                           (def. cantidadDeNodos.2)
            cantidadDeHojas t1 + cantidadDeHojas t2 + 1 + cantidadDeNodos t1 + cantidadDeNodos t2
        =                           (conmutatividad de la suma)
            1 + cantidadDeHojas t1 + cantidadDeNodos t1 + cantidadDeHojas t2 + cantidadDeNodos t2
        =                           (HI1)
            1 + cantidadDeConstructores t1 + cantidadDeHojas t2 + cantidadDeNodos t2
        =                           (HI2)
            1 + cantidadDeConstructores t1 + cantidadDeConstructores t2


    LADO DER
            cantidadDeConstructores (Nodo y t1 t2)
        =                           (def. cantidadDeConstructores.2)
            1 + cantidadDeConstructores t1 + cantidadDeConstructores t2


-}
