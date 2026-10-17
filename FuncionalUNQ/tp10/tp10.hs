-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --
import Memoria

data NExp = Var Variable
          | NCte Int 
          | NBOp NBinOp NExp NExp 

data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String


evalNExp :: NExp -> Memoria -> Int 
evalNExp (Var v)          mem = case cuantoVale v mem of
                                Nothing -> error ("Variable " ++ v ++ " no declarada")
                                Just x  -> x
evalNExp (NCte n)         mem = n
evalNExp (NBOp bop e1 e2) mem = evalNBinOp bop (evalNExp e1 mem) (evalNExp e2 mem)


evalNBinOp :: NBinOp -> (Int -> Int -> Int)
evalNBinOp Add = (+)
evalNBinOp Sub = (-)
evalNBinOp Mul = (*)
evalNBinOp Div = div
evalNBinOp Mod = mod
evalNBinOp Pow = (^)


cfNExp :: NExp -> NExp
cfNExp (Var v)          = Var v
cfNExp (NCte n)         =  NCte n
cfNExp (NBOp bop e1 e2) = simplificarNExpBin bop (cfNExp e1) (cfNExp e2)


simplificarNExpBin :: NBinOp -> NExp -> NExp -> NExp
simplificarNExpBin bop (NCte n) (NCte m) = NCte (evalNBinOp bop n m)
simplificarNExpBin bop e1        e2      = NBOp bop e1 e2



{-
            evalNExp . cfNExp = evalNExp
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀x.∀m. (evalNExp . cfNExp) x m = evalNExp x m
        
        Dem.:
        Sea e un elemento de NExp. Sea m' un elemento de Memorira. 
        Por ppio. de ind. sobre la estructura de e.

        caso base 1, e = (Var v)
                (evalNExp . cfNExp) (Var v) m' = evalNExp (Var v) m'


        caso base 2, e = (NCte n)
                (evalNExp . cfNExp) (NCte n) m' = evalNExp (NCte n) m'


        caso inductivo, e = (NOBp bop e1 e2)
                HI1) (evalNExp . cfNExp) e1 m' = evalNExp e1 m'
                HI2) (evalNExp . cfNExp) e2 m' = evalNExp e2 m'
                TI)  (evalNExp . cfNExp) (NBOp bop e1 e2) m' = evalNExp (NBOp bop e1 e2) m' 



        ||caso base 1, e = (Var v)||

                (evalNExp . cfNExp) (Var v) m'
            =                           (def. (.))
                evalNExp (cfNExp (Var v)) m'
            =                           (def. cfNExp.1)
                evalNExp (Var v) m'
        

        ||caso base 2, e = (NCte n)||
        
                (evalNExp . cfNExp) (NCte n) m'
            =                           (def. (.))
                evalNExp (cfNExp (NCte n)) m'
            =                           (def. cfNExp.2)
                evalNExp (NCte n) m'

        
        ||caso inductivo, e = (NBOp bop e1 e2)||
        
        LADO IZQ
                (evalNExp . cfNExp) (NBOp bop e1 e2) m'
            =                           (def. (.))
                evalNExp (cfNExp (NBOp bop e1 e2)) m'
            =                           (def. cfNExp.3)
                evalNExp (simplificarNExpBin bop (cfNExp e1) (cfNExp e2)) m'
            
            ||subcaso (cfNExp e1) = (NCte k) y (cfNExp e2) = (NCte r)||
            |LADO IZQ
            |        evalNExp (simplificarNExpBin bop (NCte k) (NCte r)) m'
            |    =                           (def. simplificarNExpBin.3)
            |        evalNExp (NCte (evalNBinOp bop k r))
            |    =                           (def. evalNExp.2)
            |        evalNBinOp bop k r                          
            |
            |
            |LADO DER (subcaso)
            |        evalNExp (NBOp bop e1 e2) m'
            |    =                           (def. evalNExp.3)
            |        evalNBinOp bop (evalNExp e1 m') (evalNExp e2 m')
            |    =                           (HI1, HI2)
            |        evalNBinOp bop ((evalNExp . cfNExp) e1 m') ((evalNExp . cfNExp) e2 m') 
            |    =                           (def. (.))                                        
            |        evalNBinOp bop (evalNExp (cfNExp e1) m') (evalNExp (cfNExp e2) m') 
            |    =                           (subcaso)
            |        evalNBinOp bop (evalNExp (NCte k) m') (evalNExp (NCte r) m')
            |    =                           (def. evalNExp.2)
            |        evalNBinOp bop k r


            ||subcaso (cfNExp e1) != (NCte n) || (cfNExp e2) != (Ntce n)||
            |LADO IZQ (continuación)
            |    =                           (def. simplificarNExpBin.2)
            |        evalNExp (NBOp bop (cfNExp e1) (cfNExp e2)) m'
            |    =                           (def. evalNExp.3)
            |        evalNBinOp bop (evalNExp (cfNExp e1) m') (evalNExp (cfNExp e2) m')
            |    =                           (def. (.))
            |        evalNBinOp bop ((evalNExp . cfNExp) e1 m') ((evalNExp . cfNExp) e2 m')
            |    =                           (HI1, HI2)
            |        evalNBinOp bop (evalNExp e1 m') (evalNExp e2 m')
            |    =                           (def. evalNExp.3)
            |        evalNExp (NBOp bop e1 e2) m'
-}

