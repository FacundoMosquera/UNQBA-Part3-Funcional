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




-- ================================================================================= --
--                                  EJERCICIO 2                                      --
-- ================================================================================= --
data BExp = BCte Bool 
          | Not BExp 
          | And BExp BExp 
          | Or  BExp BExp
          | ROp RelOp NExp NExp 

data RelOp = Eq | NEq -- Equal y NotEqual
           | Gt | GEq -- Greater y GreaterOrEqual
           | Lt | LEq -- Lower y LowerOrEqual


evalBExp :: BExp -> Memoria -> Bool 
evalBExp (BCte b)        mem = b
evalBExp (Not be)        mem = not (evalBExp be mem)
evalBExp (And e1 e2)     mem = evalBExp e1 mem && evalBExp e2 mem
evalBExp (Or e1 e2)      mem = evalBExp e1 mem || evalBExp e2 mem
evalBExp (ROp rop n1 n2) mem = evalROP rop (evalNExp n1 mem) (evalNExp n2 mem)

evalROP :: RelOp -> (Int -> Int -> Bool)
evalROP Eq  = (==)
evalROP NEq = (/=)
evalROP Gt  = (>) 
evalROP GEq = (>=)
evalROP Lt  = (<)
evalROP LEq = (<=)


cfBExp :: BExp -> BExp 
cfBExp (BCte b)        = BCte b
cfBExp (Not be)        = cfNot (cfBExp be)
cfBExp (And e1 e2)     = cfAnd (cfBExp e1) (cfBExp e2)
cfBExp (Or e1 e2)      = cfOr (cfBExp e1) (cfBExp e2)
cfBExp (ROp rop n1 n2) = cfRop rop (cfNExp n1) (cfNExp n2)


cfNot :: BExp -> BExp 
cfNot (BCte b) = BCte (not b)
cfNot bexp     = Not bexp


cfAnd :: BExp -> BExp -> BExp
cfAnd (BCte b) bexp2 = if b then bexp2 else BCte False
cfAnd bexp1     bexp2 = And bexp1 bexp2


cfOr :: BExp -> BExp -> BExp
cfOr (BCte b) bexp2 = if b then BCte True else bexp2
cfOr bexp1     bexp2 = Or bexp1 bexp2


cfRop :: RelOp -> NExp -> NExp -> BExp 
cfRop rop (NCte n) (NCte k) = BCte (evalROP rop n k)
cfRop rop nexp1    nexp2    = ROp rop nexp1 nexp2

{-
            evalBExp . cfBExp = evalBExp
        Por ppio. de extensionalidad, es equivalente a demostrar que:
           ∀x.∀m. (evalBExp . cfBExp) x m = evalBExp x m

        Dem.:
        Sea e un elemento de BExp. Sea m' un elemento de Memoria. 
        Por ppio. de ind. sobre la estructura de e.

        caso base 1, e = (BCte b)
                  (evalBExp . cfBExp) (BCte b) m' = evalBExp (BCte b) m'
        
        caso base 2. e = (ROp rop n1 n2)
                (evalBExp . cfBExp) (ROp rop n1 n2) m' = evalBExp (ROp rop n1 n2) m'

        caso inductivo 1, e = (Not be)
                HI) (evalBExp . cfBExp) be m' = evalBExp be m'
                TI) (evalBExp . cfBExp) (Not be) m' = evalBExp (Not be) m'

        caso inductivo 2, e = (And e1 e2)
                HI1) (evalBExp . cfBExp) e1 m' = evalBExp e1 m'
                HI2) (evalBExp . cfBExp) e2 m' = evalBExp e2 m'
                TI)  (evalBExp . cfBExp) (And e1 e2) m' = evalBExp (And e1 e2) m'
        
        caso inductivo 3, e = (Or e1 e2)
                HI1) (evalBExp . cfBExp) e1 m' = evalBExp e1 m'
                HI2) (evalBExp . cfBExp) e2 m' = evalBExp e2 m'
                TI)  (evalBExp . cfBExp) (Or e1 e2) m' = evalBExp (Or e1 e2) m'

        



        ||caso base 1, e = (BCte b)||

                (evalBExp . cfBExp) (BCte b) m'
            =                           (def. (.))
                evalBExp (cfBExp (BCte b)) m'
            =                           (def. cfBExp.1)
                evalBExp (BCte b) m'


        ||caso base 2, e = (ROp rop n1 n2)||

                (evalBExp . cfBExp) (ROp rop n1 n2) m'
            =                           (def. (.))
                evalBExp (cfBExp (ROp rop n1 n2)) m'
            =                           (def. cfBExp.5)
                evalBExp (cfRop rop (cfNExp n1) (cfNExp n2)) m'
            
            ||subcaso (cfNExp n1) = (NCte k) y (cfNExp n2) = (NCte l)||
            |LADO IZQ (continuación)
            |        evalBExp (cfRop rop (NCte k) (NCte l)) m'
            |    =                           (def. cfRop.1)
            |        evalBExp (BCte (evalROP rop k l)) m'
            |    =                           (def. evalBExp.1)
            |        evalROP rop k l
            |    
            |LADO DER 
            |        evalBExp (ROp rop n1 n2) m'
            |    =                           (def. evalBExp.5)
            |        evalROP rop (evalNExp n1 m') (evalNExp n2 m')
            |    =                           (prop. ej1: ∀x.∀m. (evalNExp . cfNExp) x m = evalNExp x m)
            |        evalROP rop ((evalNExp . cfNExp) n1 m') ((evalNExp . cfNExp) n2 m')
            |    =                           (def. (.))
            |        evalROP rop (evalNExp (cfNExp n1) m') (evalNExp (cfNExp n2) m')
            |    =                           (subcaso)
            |        evalROP rop (evalNExp (NCte k) m') (evalNExp (NCte l) m')
            |    =                           (def. evalNExp.1)
            |        evalROP rop k l

            ||subcaso (cfNExp n1) != (NCte k) o (cfNExp n2) != (NCte l)||
            |LADO IZQ (continuación)
            |        evalBExp (cfRop rop (cfNExp n1) (cfNExp n2)) m'
            |    =                           (def. cfRop.2)
            |        evalBExp (ROp rop (cfNExp n1) (cfNExp n2)) m'
            |    =                           (def. evalBExp. 5)
            |        evalROP rop (evalNExp (cfNExp n1) m') (evalNExp (cfNExp n2) m')
            |    =                           (def. (.))
            |        evalROP rop ((evalNExp . cfNExp) n1 m') ((evalNExp . cfNExp) n2 m')
            |    =                           (prop. ej1: ∀x.∀m. (evalNExp . cfNExp) x m = evalNExp x m)
            |        evalROP rop (evalNExp n1 m') (evalNExp n2 m')
            |    =                           (def. evalBExp.5)
            |        evalBExp (ROp rop n1 n2) m'


        ||caso inductivo 1, e = (Not be)||

        LADO IZQ
                (evalBExp . cfBExp) (Not be) m'
            =                           (def. (.))
                evalBExp (cfBExp (Not be)) m'
            =                           (def. cfBExp.2)
                evalBExp (cfNot (cfBExp be)) m'

            ||subcaso (cfBExp be) = (BCte b)||
            |
            |LADO IZQ (continuación)
            |        evalBExp (cfNot (BCte b)) m'
            |    =                           (def. cfNot.1)
            |        evalBExp (BCte (not b)) m'
            |    =                           (def. evalBExp.1)
            |        not b
            |
            |LADO DER
            |        evalBExp (Not be) m'
            |    =                           (def. evalBExp.2)
            |        not (evalBExp be m')
            |    =                           (HI)
            |        not ((evalBExp . cfBExp) be m')
            |    =                           (def. (.))
            |        not (evalBExp (cfBExp be) m')
            |    =                           (subcaso)
            |        not (evalBExp (BCte b) m')
            |    =                           (def. evalBExp.1)
            |        not b


            ||subcaso (cfBExp be) != (BCte b)||
            |
            |LADO IZQ (continuación)
            |        evalBExp (cfNot (cfBExp be)) m'
            |    =                           (def. cfNot.2)
            |        evalBExp (Not (cfBExp be)) m'
            |    =                           (def. evalBExp.2)
            |        not (evalBExp (cfBExp be) m')
            |    =                           (def. (.))
            |        not ((evalBExp . cfBExp) be m')
            |    =                           (HI)
            |        not (evalBExp be m')
            |    =                           (def. evalBExp.2)
            |        evalBExp (Not be) m'

        
        ||caso inductivo 2, e = (And e1 e2)||
        
        LADO IZQ
                (evalBExp . cfBExp) (And e1 e2) m'
            =                           (def. (.))
                evalBExp (cfBExp (And e1 e2)) m'
            =                           (def. cfBExp.3)
                evalBExp (cfAnd e1 e2) m'
            
            ||subcaso e1 = (BCte b)||
            |LADO IZQ
            |        evalBExp (cfAnd (BCte b) e2) m'
            |    =                           (def. cfAnd.1)
            |        evalBExp (if b then e2 else BCte False) m'
            |    
            |    ||subsubcaso b = True||
            |    |LADO IZQ
            |    |        evalBExp (if True then e2 else BCte False) m'
            |    |    =                           (rama then del if)
            |    |        evalBExp e2 m'
            |    |    
            |    |LADO DER
            |    |        evalBExp (And e1 e2) m'
            |    |    =                           (subsubcaso)
            |    |        evalBExp (And (BCte True) e2) m'
            |    |    =                           (def. evalBExp.3)
            |    |        (evalBExp (BCtre True) m') && (evalBExp e2 m')
            |    |    =                           (def. evalBExp.1)
            |    |        True && (evalBExp e2 m')
            |    |    =                           (Ley de Identidad &&)
            |    |        evalBExp e2 m'
            |    
            |    
            |    ||subsubcaso b = False||
            |    |LADO IZQ
            |    |        evalBExp (if False then e2 else BCte False) m'
            |    |    =                           (rama else del if)
            |    |        evalBExp (BCte False) m'
            |    |    =                           (def. evalBExp.1)
            |    |        False
            |    |    
            |    |LADO DER
            |    |        evalBExp (And e1 e2) m'
            |    |    =                           (subsubcaso)
            |    |        evalBExp (And (BCte False) e2) m'
            |    |    =                           (def. evalBExp.3)
            |    |        (evalBExp (BCte False) m') && (evalBExp e2 m')
            |    |    =                           (def. evalBExp.1)
            |    |        False && (evalBExp e2 m')
            |    |    =                           (def. &&)
            |    |        False     


            ||subcaso e1 != (BCte b)||
            |
            |LADO IZQ
            |        evalBExp (cfBExp (And e1 e2)) m'
            |    =                           (def. cfBExp.3)
            |        evalBExp (cfAnd e1 e2) m'
            |    =                           (def. cfAnd.2)
            |        evalBExp (And e1 e2) m'
                



        ||caso inductivo 3, e = (Or e1 e2)||

        LADO IZQ
                (evalBExp . cfBExp) (Or e1 e2) m'
            =                           (def. (.))
                evalBExp (cfBExp (Or e1 e2)) m'
            =                           (def. cfBExp.4)
                evalBExp (cfOr e1 e2) m'
            
            ||subcaso e = (BCte b')||
            |
            |LADO IZQ
            |        evalBExp (cfOr (BCte b') e2) m'
            |    =                           (def. cfBExp.1)
            |        evalBExp (if b' then BCte True else e2) m'
            |    
            |    ||subsubcaso b' = True||
            |    |
            |    |LADO IZQ
            |    |    =                           (rama then del if)
            |    |        evalBExp (BCte True) m'
            |    |    =                           (def. evalBExp.1)
            |    |        True
            |    |
            |    |LADO DER
            |    |        evalBExp (Or e1 e2) m'
            |    |    =                           (subsubcaso)
            |    |        evalBExp (Or (BCte True) e2) m'
            |    |    =                           (def. evalBExp.4)
            |    |        (evalBExp (BCte True) m') || (evalBExp e2 m')
            |    |    =                           (def. evalBExp.1)
            |    |        True || (evalBExp e2 m')
            |    |    =                           (def.||)
            |    |        True
            |
            |    ||subsubcaso b' = False||
            |    |
            |    |LADO IZQ
            |    |    =                           (rama else del if)
            |    |        evalBExp e2 m'
            |    |
            |    |LADO DER
            |    |        evalBExp (Or e1 e2) m'
            |    |    =                           (def. evalBExp.4)
            |    |        (evalBExp e1 m') || (evalBExp e2 m')
            |    |    =                           (subsubcaso)
            |    |        (evalBExp (BCte False) m') || (evalBExp e2 m')
            |    |    =                           (def. evalBExp.1)
            |    |        False || (evalBExp e2 m')
            |    |    =                           (def.||)
            |    |        evalBExp e2 m'


            ||subcaso e != (BCte b')||
            |
            |LADO IZQ
            |        evalBExp (cfOr e1 e2) m'
            |    =                           (def. cfOr.2)
            |        evalBExp (Or e1 e2) m'
            
                
-}


-- ================================================================================= --
--                                  EJERCICIO 3                                      --
-- ================================================================================= --
data Programa = Prog Bloque 
type Bloque   = [Comando]
data Comando  = Assign Nombre NExp
              | If BExp Bloque Bloque
              | While BExp Bloque
type Nombre   = String


evalProg :: Programa -> Memoria -> Memoria
evalProg (Prog bl) = evalBloque bl 

evalBloque :: Bloque -> Memoria -> Memoria
evalBloque []     mem = mem
evalBloque (c:cs) mem = let mem' = evalComando c mem
                        in evalBloque cs mem'


evalComando :: Comando -> Memoria -> Memoria 
evalComando (Assign n e)      mem = recordar n (evalNExp e mem) mem
evalComando (If be blq1 blq2) mem = if (evalBExp be mem) 
                                then evalBloque blq1 mem 
                                else evalBloque blq2 mem
evalComando (While be blq)    mem = evalComando (If be (blq ++ [While be blq]) []) mem


optimizeCf :: Programa -> Programa
optimizeCf (Prog cs) = Prog (optimizeCS cs) 


optimizeCS :: Bloque -> Bloque
optimizeCS []     = []
optimizeCS (c:cs) = optimizeC c ++ optimizeCS cs


optimizeC :: Comando -> Bloque 
optimizeC (Assign x ne)   = [Assign x ne]
optimizeC (If be cs1 cs2) = cfIF (cfBExp be) (optimizeCS cs1) (optimizeCS cs2) 
optimizeC (While be cs)   = cfWhile (cfBExp be) (optimizeCS cs)


cfIF :: BExp -> Bloque -> Bloque -> Bloque
cfIF (BCte True)  cs1 cs2 = cs1
cfIF (BCte False) cs1 cs2 = cs2
cfIF be cs1 cs2           = [If be cs1 cs2]


cfWhile :: BExp -> Bloque -> Bloque 
cfWhile (BCte False) cs = []
cfWhile be cs           = [While be cs]


{-
    [
        (Assign "f" (NCte 1)),
        (If (ROp Eq (NBOp Add (NCte 3) (NCte 3)) (NCte 6)) [Assign "f" (NCte 3)] [Assign "f" (NBOp Add (NCte 10) (Var "f"))]),
        (Assign "f" (NBOp Add (NCte 1) (Var "f")))
    ]
-}



