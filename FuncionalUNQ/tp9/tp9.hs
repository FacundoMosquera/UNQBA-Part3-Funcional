-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --

data EA = Const Int | BOp BinOp EA EA 
data BinOp = Sum | Mul

data ExpA = Cte Int 
          | Suma ExpA ExpA
          | Prod ExpA ExpA

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
-}