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






-- ================================================================================= --
--                                  EJERCICIO 3                                      --
-- ================================================================================= --

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--a)
sumarT :: Tree Int -> Int 
sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2


sizeT :: Tree a -> Int 
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2


anyT :: (a -> Bool) -> Tree a -> Bool 
anyT f EmptyT          = False
anyT f (NodeT x t1 t2) = f x || anyT f t1 || anyT f t2


countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT          = 0
countT f (NodeT x t1 t2) = fromEnum (f x) + countT f t1 + countT f t2


countLeaves :: Tree a -> Int 
countLeaves EmptyT          = 1
countLeaves (NodeT x t1 t2) = countLeaves t1 + countLeaves t2


heightT :: Tree a -> Int 
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + (heightT t1 `max` heightT t2)


inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x t1 t2) = inOrder t1 ++ [x] ++ inOrder t2


listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : zipLevels (listPerLevel t1) (listPerLevel t2)
                            where zipLevels xss      []       = xss
                                  zipLevels []       yss      = yss
                                  zipLevels (xs:xss) (ys:yss) = (xs ++ ys) : zipLevels xss yss



mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)


levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN n (NodeT x t1 t2) = if n==0 then [x] else (levelN (n-1) t1) ++ (levelN (n-1) t2)


ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = let l1 = ramaMasLarga t1
                                   l2 = ramaMasLarga t2
                                   in if length l1 > length l2 then [x] ++ l1 else [x] ++ l2


todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = let c1     = todosLosCaminos t1 
                                      c2     = todosLosCaminos t2
                                      finAca = if null c1 || null c2 then [[x]] else []
                                    in finAca ++ (concatATodas x c1) ++ (concatATodas x c2) 

concatATodas :: a -> [[a]] -> [[a]]
concatATodas e []       = []
concatATodas e (xs:xss) = (e:xs) : concatATodas e xss

{-
(NodeT 3 (NodeT 120 EmptyT EmptyT) 
         (NodeT 10 (NodeT 20 (NodeT 399 EmptyT EmptyT) 
                             (NodeT 50  EmptyT EmptyT))
                    EmptyT))
-}


{-

b)
    i)
            heightT = length . ramaMasLarga
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀t. heightT t = (length . ramasMasLarga) t
        
        Dem.:
        Sea t' un elemento de Tree a. Por ppio. de ind. sobre la estructura de t'.

        caso base, t' = EmptyT
                heightT EmptyT = (length . ramaMasLarga) EmptyT

        caso inductivo, t' = (NodeT x t1 t2)
                HI1) heightT t1 = (length . ramasMasLarga) t1
                HI2) heightT t2 = (length . ramasMasLarga) t2
                TI)  heightT (NodeT x t1 t2) = (length . ramasMasLarga) (NodeT x t1 t2)


        
        ||caso base, t'= EmptyT||
        
        LADO IZQ
                heightT EmptyT
            =                       (def. heighT.1)
                0
        
        LADO DER
                (length . ramaMasLarga) EmptyT
            =                       (def. (.))
                length (ramaMasLarga EmptyT)
            =                       (def. ramaMasLarga.1)
                length []
            =                       (def. length)
                0

        
        ||caso inductivo, t' = (NodeT x t1 t2)||
        
        LADO IZQ
                heightT (NodeT x t1 t2)
            =                       (def. heightT.2)
                1 + (heighT t1 `max` heighT t2)
                       


        LADO DER
                (length . ramaMasLarga) (NodeT x t1 t2)
            =                       (def. (.))
                length (ramaMasLarga (NodeT x t1 t2))
            =                       (def. ramaMasLarga.2)
                length (length l1 > length l2 then [x] ++ l1 else [x] ++ l2)  (donde l1 = ramaMasLarga t1 y l2 = ramaMasLarga t2)
            =                       (reemplazo variables)
                length (length (ramaMasLarga t1) > length (ramaMasLarga t2) 
                    then [x] ++ ramaMasLarga t1 else [x] ++ ramaMasLarga t2)
            =                       (def. (.))
                length ((length . ramaMasLarga) t1) > (length . ramaMasLaraga) t2
                    then [x] ++ ramaMasLarga t1 else [x] ++ ramaMasLarga t2)
            =                       (HI1, HI2)
                length (height t1 > height t2) then [x] ++ ramaMasLarga t1 else [x] ++ ramaMasLarga t2

            ||caso heightT t1 > heightT t2||
            
            LADO IZQ
                =                       (def. max)
                    1 + heighT t1

            LADO DER
                =                       (rama then del if)
                    length ([x] ++ ramaMasLarga t1) 
                =                       (def. ++.2)
                    length (x : [] ++ ramaMasLarga t1)
                =                       (def. ++.1)
                    length (x : ramaMasLarga t1)
                =                       (def. length.2)
                    1 + length (ramaMasLarga t1)
                =                       (def. (.))
                    1 + (length . ramaMasLarga) t1
                =                       (HI1)
                    1 + heightT t1

            ||caso heigthT t1 <= heightT2||
            
            LADO IZQ
                =                       (def. max)
                    1 + heightT t2
            
            LADO DER
                =                       (rama else del if)
                    length ([x] ++ ramaMasLarga t2) 
                =                       (def. ++.2)
                    length (x : [] ++ ramaMasLarga t2)
                =                       (def. ++.1)
                    length (x : ramaMasLarga t2)
                =                       (def. length.2)
                    1 + length (ramaMasLarga t2)
                =                       (def. (.))
                    1 + (length . ramaMasLarga) t2
                =                       (HI2)
                    1 + heightT t2

    

    ii)
            reverse . inOrder = inOrder . mirrorT
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀t. (reverse . inOrder) t = (inOrder . mirrorT) t

        Dem.:
        Sea t' un elemento de Tree a. Por ppio. de ind. sobre la estructura de t'.

        caso base, t' = EmptyT
                (reverse . inOrder) EmptyT = (inOrder . mirrorT) EmptyT
        
        caso inductivo, t' = (NodeT x t1 t2)
                HI1) (reverse . inOrder) t1 = (inOrder . mirrorT) t1
                HI2) (reverse . inOrder) t2 = (inOrder . mirrorT) t2
                TI)  (reverse . inOrder) (NodeT x t1 t2) = (inOrder . mirrorT) (NodeT x t1 t2)
        


        ||caso base, t' = EmptyT||

        LADO IZQ
                (reverse . inOrder) EmptyT
            =                       (def. (.))
                reverse (inOrder EmptyT)
            =                       (def. inOrder.1)
                reverse []
            =                       (def. reverse.1)
                []
        
        LADO DER
                (inOrder . mirrorT) EmptyT
            =                       (def. (.))
                inOrder (mirrorT EmptyT)
            =                       (def. mirrorT.1)
                inOrder EmptyT
            =                       (def. inOrder.1)
                []
        


        ||caso inductivo, t' = (NodeT x t1 t2)||

        LADO IZQ
                (reverse . inOrder) (NodeT x t1 t2)
            =                       (def. (.))
                reverse (inOrder (NodeT x t1 t2))
            =                       (def. inOrder.2)
                reverse (inOrder t1 ++ [x] ++ inOrder t2)
            =                       (asociatividad ++)
                reverse (inOrder t1 ++ ([x] ++ inOrder t2))
            =                       (Prop. tp8: reverse (xs ++ ys) = reverse ys ++ reverse xs)
                reverse ([x] ++ inOrder t2) ++ reverse (inOrder t1)
            =                       (Prop. tp8: reverse (xs ++ ys) = reverse ys ++ reverse xs)
                reverse (inOrder t2) ++ reverse [x] ++ reverse (inOrder t1)
            =                       (def. reverse.2)
                reverse (inOrder t2) ++ reverse [] ++ [x] ++ reverse (inOrder t1)
            =                       (def. reverse.1)
                reverse (inOrder t2) ++ [] ++ [x] ++ reverse (inOrder t1)
            =                       (def. ++.1)
                reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)

        
        LADO DER
                (inOrder . mirrorT) (NodeT x t1 t2)
            =                       (def. (.))
                inOrder (mirrorT (NodeT x t1 t2))
            =                       (def. mirrorT.2)
                inOrder (NodeT x (mirrorT t2) (mirrorT t1))
            =                       (def. inOrder.2)
                inOrder (mirrorT t2) ++ [x] ++ inOrder (mirrorT t1)
            =                       (def. (.))
                (inOrder . mirrorT) t2 ++ [x] ++ (inOrder. mirrorT) t1
            =                       (HI1, HI2)
                (reverse . inOrder) t2 ++ [x] ++ (reverse . inOrder) t1
            =                       (def. (.))
                reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)
            

-}




-- ================================================================================= --
--                                  EJERCICIO 4                                      --
-- ================================================================================= --

data AppList a = Single a
               | Append (AppList a) (AppList a) 


--a)
lenAL :: AppList a -> Int 
lenAL (Single x)   = 1
lenAL (Append l r) = lenAL l + lenAL r  


consAL :: a -> AppList a -> AppList a
consAL e (Single x)   = Append (Single e) (Single x)
consAL e (Append l r) = Append (consAL e l) r


headAL :: AppList a -> a --Esta sí debería ser recursiva estructural.
headAL (Single x)   = x
headAL (Append l r) = headAL l


tailAL :: AppList a -> AppList a
tailAL (Single x)   = error "No puede resultar en una lista vacía."
tailAL (Append l r) = case l of 
                        (Single _) -> r
                        _          -> Append (tailAL l) r


snocAL :: AppList a -> a -> AppList a 
snocAL (Single x)   y = Append (Single x) (Single y)
snocAL (Append l r) y = Append l (snocAL r y)


lastAL :: AppList a -> a --también debería ser recursiva estructural.
lastAL (Single x)   = x
lastAL (Append l r) = lastAL r


initAL :: AppList a -> AppList a
initAL (Single x)   = error "No puede resultar en una lista vacía."
initAL (Append l r) = case r of
                        (Single _) -> l
                        _          -> Append l (initAL r)


reverseAL :: AppList a -> AppList a
reverseAL (Single x)   = Single x
reverseAL (Append l r) = Append (reverseAL r) (reverseAL l)


elemAL :: Eq a => a -> AppList a -> Bool 
elemAL x (Single y)   = x == y
elemAL x (Append l r) = elemAL x l || elemAL x r


appendAL :: AppList a -> AppList a -> AppList a
appendAL app (Single x)   = Append app (Single x)
appendAL app (Append l r) = Append (appendAL app l) r 


appListToList :: AppList a -> [a]
appListToList (Single x)   = [x]
appListToList (Append l r) = appListToList l ++ appListToList r


{-  Ej. para testear

representando [10,20,30,40,50]
r1:
(Append (Append (Single 10) 
               (Append (Single 20) 
                       (Append (Single 30) (Single 40)))) 
        (Single 50))

r2:
(Append (Append (Append (Single 10) (Single 20)) 
               (Single 30)) 
       (Append (Single 40) 
               (Single 50)))


representando [1, 2]

Append (Single 1) (Single 2)
-}


{-
b)
    i)
            ∀xs.∀ys. lenAL (appendAL xs ys) = lenAL xs + lenAL ys
        
        Dem.:
        Sean ws y zs elementos de AppList a. Por ppio. de ind. sobre la estructura de zs.

        caso base, zs = (Single x)
                lenAL (appendAL ws (Single x)) = lenAL ws + lenAL (Single x)

        caso inductivo, zs = (Append l r)
                HI1) lenAL (appendAL ws l) = lenAL ws + lenAL l
                HI2) lenAL (appendAL ws r) = lenAL ws + lenAL r
                TI)  lenAL (appendAL ws (Append l r)) = lenAL ws + lenAL (Append l r)

        

        ||caso base, zs = (Single x)||
        
                lenAL (appendAL ws (Single x))
            =                       (def. appendAL.1)
                lenAL (Append ws (Single x))
            =                       (def. lenAL.2)
                lenAL ws + lenAL (Single x)


        ||caso inductivo, zs = (Append l r)||

        LADO IZQ
                lenAL (appendAL ws (Append l r))
            =                       (def. appendAL.2)
                lenAL (Append (appendAL ws l) r)
            =                       (def. lenAL.2)
                lenAL (appendAL ws l) + lenAL r
            =                       (HI1)
                lenAL ws + lenAL l + lenAL r

        LADO DER
                lenAL ws + lenAL (Append l r)
            =                       (def. lenAL.2)
                lenAL ws + lenAL l + lenAL r


    
    ii)
            reverseAL . reverseAL = id
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀t. (reverseAl . reverseAL) t = id t
        
        Dem.:
        Sea t' un elemento de AppList a. Por ppio. de ind. sobre la estructura de t'.

        caso base, t' = (Single x)
                (reverseAl . reverseAL) (Single x) = id (Single x)
        
        caso inductivo, t' = (Append l r)
                HI1) (reverseAL . reverseAL) l = id l
                HI2) (reverseAL . reverseAL) r = id r
                TI)  (reverseAL . reverseAL) (Append l r) = id (Append l r)


        ||caso base, t' = (Single x)||

        LADO IZQ
                (reverseAL . reverseAL) (Single x)
            =                       (def. (.))
                reverseAL (reverseAL (Single x))
            =                       (def. reverseAL.1)
                reveseAL (Single x)
            =                       (def. reverseAL.1)
                Single x

        LADO DER
                id (Single x)
            =                       (def. id)
                Single x
        

        ||caso inductivo, t' = (Append l r)||

        LADO IZQ
                (reverseAL . reverseAL) (Append l r)
            =                       (def. (.))
                reverseAL (reverseAL (Append l r))
            =                       (def. reverseAL.2)
                reverseAL (Append (reverseAL r) (reverseAL l))
            =                       (def. reverseAL.2)
                (Append (reverseAL (reverseAL l) (reverseAL (reverseAL r)))
            =                       (def. (.))
                Append ((reveseAL . reverseAL) l) ((reveseAL . reverseAL) r)
            =                       (HI1, HI2)
                Append (id l) (id r)
            =                       (def. id)
                Append l r
        
        LADO DER
                id (Append l r)
            =                       (def. id)
                Append l r

    

    iii)
            (reverseAL .) . flip consAL . reverseAL = snocAL
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀t.∀e. ((reverseAL .) . flip consAL . reverseAL) t e = snocAL t e
        
        Dem.:
        Sea t' un elemento de AppList a. Sea e' un elemento de a. 
        Por ppio. de ind. sobre la estructura de t'.

        caso base, t' = (Single x)
                ((reverseAL .) . flip consAL . reverseAL) (Single x) e' = snocAL (Single x) e'
        

        caso inductivo, t' = (Append l r)
                HI1) ((reverseAL .) . flip consAL . reverseAL) l e' = snocAL l e'
                HI2) ((reverseAL .) . flip consAL . reverseAL) r e' = snocAL r e'
                TI)  ((reverseAL .) . flip consAL . reverseAL) (Append l r) e' = snocAL (Append l r) e'


        ||caso base, t' = (Single x)||

        LADO IZQ
                ((reverseAL .) . flip consAL . reverseAL) (Single x) e'
            =                       (def. (.), f <- (reverseAL .) . flip consAL, g <- reveseAL, x <- Single x)
                ((reverseAL .) . flip consAL) (reverseAL (Single x)) e'
            =                       (def. (.), f'<- (reverseAL .), g' <- flip consAL, x' <- (reverseAL (Single x)))
                (reverseAL .) (flip consAL (reverseAL (Single x))) e'
            =                       (def. (.), f''<- reverseAL, g''<- flip consAL (reverseAL (Single x)), x'' <- e')
                reverseAL (flip consAL (reverseAL (Single x)) e')
            =                       (def.  reverseAL.1)
                reverseAL (flip consAL (Single x) e')
            =                       (def. flip)
                reverseAL (consAL e' (Single x))
            =                       (def. consAL)
                reverseAL (Append (Single e') (Single x))
            =                       (def. reverseAL.2)
                Append (reverseAL (Single x)) (reverseAL (Single e'))
            =                       (def. reverseAL.1)
                Append (Single x) (Single e')


        LADO DER
                snocAL (Single x) e'
            =                       (def. snocAL.1)
                Append (Single x) (Single e')

        


        ||caso inductivo, t' = (Append l r)||

        LADO IZQ
                ((reverseAL .) . flip consAL . reverseAL) (Append l r) e'
            =                       (def. (.), f<-(reverseAL .) . flip consAL, g<- reverseAL, x <- (Append l r))
                ((reverseAL .) . flip consAL) (reverseAL (Append l r)) e'
            =                       (def. (.), f'<-(reverseAL .), g'<-flip consAL, x'<-reverseAL (Append l r))
                reverseAL . (flip consAL (reverseAL (Append l r))) e'
            =                       (def. (.), f''<-reverseAL, g''<-flip consAL (reverseAL (Append l r)), x''<- e')
                reverseAL (flip consAL (reverseAL (Append l r)) e')
            =                       (def. flip)
                reverseAL (consAL e' (reverseAL (Append l r)))
            =                       (def. reverseAL.2)
                reverseAL (consAL e' (Append (reverseAL r) (reverseAL l)))
            =                       (def. consAL.2)
                reverseAL (Append (consAL e' (reverseAL r)) (reverseAL l))
            =                       (def. reverseAL.2)
                Append (reverseAL (reverseAL l)) (reverseAL (consAL e' (reverseAL r)))
            =                       (def. (.))
                Append ((reverseAL . reverseAL) l) (reverseAL (consAL e' (reverseAL r)))
            =                       (prop. inciso (ii) reverseAL . reverseAL = id )
                Append (id l) (reverseAL (consAL e' (reverseAL r)))
            =                       (def. id)
                Append l (reverseAL (consAL e' (reverseAL r)))


        LADO DER
                snocAL (Append l r) e'
            =                       (def. snocAL.2)
                Append l (snocAL r e')
            =                       (HI2)
                Append l (((reverseAL .) . flip consAL . reverseAL) r e')
            =                       (def. (.), f<-(reverseAL .) . flip consAL, g<-reverseAL, x<-r)
                Append l (((reverseAL .) . flip consAL) (reverseAL r) e')
            =                       (def. (.), f'<-(reverseAL .), g'<-flip consAL, x'<-(reverseAL r))
                Append l ((reverseAL .) (flip consAL (reverseAL r)) e')
            =                       (def. (.), f''<-reverseAL, g''<-flip consAL (reverseAL r), x''<-e')
                Append l (reverseAL (flip consAL (reverseAL r)) e')
            =                       (def. flip)
                Append l (reverseAL (consAL e' (reverseAL r)))
-}