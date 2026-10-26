-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --
data ExpA = Cte Int 
          | Suma ExpA ExpA 
          | Prod ExpA ExpA 

--a)
foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n)      = c n
foldExpA c s p (Suma e1 e2) = s (foldExpA c s p e1) (foldExpA c s p e2)
foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1) (foldExpA c s p e2)

--b)
cantidadDeCeros :: ExpA -> Int 
cantidadDeCeros = foldExpA (fromEnum . (==0)) (+) (+)


noTieneNegativosExplicitosExpA :: ExpA -> Bool 
noTieneNegativosExplicitosExpA = allExpA (>=0)

allExpA :: (Int -> Bool) -> ExpA -> Bool
allExpA p = foldExpA p (&&) (&&) 


simplificarExpA' :: ExpA -> ExpA 
simplificarExpA' = foldExpA Cte sumaSimplificada prodSimplificada


prodSimplificada :: ExpA -> ExpA -> ExpA
prodSimplificada (Cte 1) n       = n
prodSimplificada (Cte 0) n       = Cte 0
prodSimplificada m       (Cte 1) = m
prodSimplificada m       (Cte 0) = Cte 0
prodSimplificada m       n       = Prod m  n

sumaSimplificada :: ExpA -> ExpA -> ExpA
sumaSimplificada (Cte 0) n       = n 
sumaSimplificada m       (Cte 0) = m
sumaSimplificada m       n       = Suma m n


evalExpA' :: ExpA -> Int 
evalExpA' = foldExpA id (+) (*)


showExpA :: ExpA -> String 
showExpA = foldExpA showC showS showP 
        where showC n     = "(Cte " ++ (show n) ++ ")"
              showS s1 s2 = "(Suma " ++ s1 ++ " " ++ s2 ++ ")"
              showP s1 s2 = "(Prod " ++ s1 ++ " " ++ s2 ++ ")"


{-
c)
            evalExpA = evalExpA'
      Po ppio de extensionalidad, es equivalente a demostrar que: 
            ∀x. evalExpA x = evalExpA' x
      
      Dem.: 
      Sea e un elemento de ExpA. Por ppio. de ind. sobre la estructura de ExpA. 

      caso base, e = (Cte n)
            evalExpA (Cte n) = evalExpA' (Cte n)

      caso inductivo 1, e = (Suma e1 e2)
            HI1) evalExpA e1 = evalExpA' e1
            HI2) evalExpA e2 = evalExpA' e2
            TI)  evalExpA (Suma e1 e2) = evalExpA' (Suma e1 e2)

      caso inductivo 2, e = (Prod e1 e2)
            HI1) evalExpA e1 = evalExpA' e1
            HI2) evalExpA e2 = evalExpA' e2
            TI)  evalExpA (Prod e1 e2) = evalExpA' (Prod e1 e2)


      ||caso base, e = (Cte n)||
      
      LADO IZQ 
                  evalExpA (Cte n)
            =                                         (def. evalExpA.1)
                  n 
      
      LADO DER
                  evalExpA' (Cte n)
            =                                         (def. evalExpA)
                  foldExpA id (+) (*) (Cte n)
            =                                         (def. foldExpA.1)
                  id n
            =                                         (def. id)
                  n

      
      ||caso inductivo 1, e = (Suma e1 e2)||

      LADO IZQ 
                  evalExpA (Suma e1 e2)
            =                                         (def. evalExpA.2)
                  evalExpA e1 + evalExpA e2
            =                                         (HI1, HI2)
                  evalExpA' e1 + evalExpA' e2
      
      LADO DER
                  evalExpA' (Suma e1 e2)
            =                                         (def. evalExpA)
                  foldExpA id (+) (*) (Suma e1 e2)
            =                                         (def. foldExpA.2)
                  (+) (foldExpA id (+) (*) e1) (foldExpA id (+) (*) e1)
            =                                         (def. evalExpA')
                  (+) (evalExpA' e1) (evalExpA' e2)
            =                                         (equivalencia infix del operador)
                  evalExpA' e1 + evalExpA' e2

      
      ||caso inductivo 2, e = (Prod e1 e2)||

      LADO IZQ 
                  evalExpA (Prod e1 e2)
            =                                         (def. evalExpA.3)
                  evalExpA e1 * evalExpA e2
            =                                         (HI1, HI2)
                  evalExpA' e1 * evalExpA' e2
      
      LADO DER
                  evalExpA' (Prod e1 e2)
            =                                         (def. evalExpA')
                  foldExpA id (+) (*) (Prod e1 e2)
            =                                         (def. foldExpA.3)
                  (*) (foldExpA id (+) (*) e1) (foldExpA id (+) (*) e1)
            =                                         (def. evalExpA')
                  (*) (evalExpA' e1) (evalExpA' e2)
            =                                         (equivalencia infix del operador)
                  evalExpA' e1 * evalExpA' e2
-}


--d)
recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b 
recExpA c s p (Cte n) = c n 
recExpA c s p (Suma e1 e2) = s e1 e2 (recExpA c s p e1) (recExpA c s p e2)
recExpA c s p (Prod e1 e2) = p e1 e2 (recExpA c s p e1) (recExpA c s p e2)

--e)
cantDeSumaCeros :: ExpA -> Int 
cantDeSumaCeros = recExpA (const 0) 
                         (\e1 e2 n1 n2 -> (fromEnum (opConConstante 0 e1 e2)) + n1 + n2 )
                         (\e1 e2 n1 n2 -> n1 + n2)


opConConstante :: Int -> ExpA -> ExpA -> Bool 
opConConstante n (Cte m) _  = n==m
opConConstante n _  (Cte m) = n==m
opConConstante n _  _       = False 


cantDeProdUnos :: ExpA -> Int 
cantDeProdUnos = recExpA (const 0)
                         (\e1 e2 n1 n2 -> n1 + n2)
                         (\e1 e2 n1 n2 -> (fromEnum (opConConstante 1 e1 e2)) + n1 + n2)



-- ================================================================================= --
--                                  EJERCICIO 2                                      --
-- ================================================================================= --
data EA = Const Int | BOp BinOp EA EA 
data BinOp = Sum | Mul

--a)
foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA f g (Const n)       = f n
foldEA f g (BOp bop e1 e2) = g bop (foldEA f g e1) (foldEA f g e2)

--b)
noTieneNegativosExplicitosEA :: EA -> Bool 
noTieneNegativosExplicitosEA = allEA (>=0) (\op -> True)

allEA :: (Int -> Bool) -> (BinOp -> Bool) -> EA -> Bool 
allEA p q = foldEA p ((\b1 b2 b3 -> b1 && b2 && b3) . q)


simplificarEA' :: EA -> EA 
simplificarEA' = foldEA Const simplificarOpEA

simplificarOpEA :: BinOp -> EA -> EA -> EA 
simplificarOpEA Mul e1 e2 = simplificarMulEA e1 e2 
simplificarOpEA Sum e1 e2 = simplificarSumEA e1 e2 

simplificarMulEA :: EA -> EA -> EA 
simplificarMulEA (Const 1) e = e
simplificarMulEA e (Const 1) = e
simplificarMulEA e1 e2 = if esConstanteDe 0 e1 || esConstanteDe 0 e2 then Const 0 else BOp Mul e1 e2  

simplificarSumEA :: EA -> EA -> EA 
simplificarSumEA (Const 0) e = e 
simplificarSumEA e (Const 0) = e 
simplificarSumEA e1 e2       = BOp Sum e1 e2 


esConstanteDe :: Int -> EA -> Bool 
esConstanteDe n (Const m) = n==m 
esConstanteDe _ _         = False


evalEA' :: EA -> Int 
evalEA' = foldEA id evalBOp 

evalBOp :: BinOp -> (Int -> Int -> Int)
evalBOp Mul = (*)
evalBOp Sum = (+)


showEA :: EA -> String 
showEA = foldEA (\n -> "(Const " ++ show n ++ ")")
                (\bop s1 s2 -> "(" ++ s1 ++ " " ++ showBOp bop ++ " " ++ s2 ++ ")")

showBOp :: BinOp -> String 
showBOp Mul = "*"
showBOp Sum = "+"

ea2ExpA' :: EA -> ExpA 
ea2ExpA' = foldEA Cte sumaOrProd
      where sumaOrProd Sum = Suma 
            sumaOrProd Mul = Prod


data ABTree  a b   = Leaf b | Node a (ABTree a b) (ABTree a b)

ea2Arbol' :: EA -> ABTree BinOp Int 
ea2Arbol' = foldEA Leaf Node 

{-
c)
            evalEA' = evalEA
      Por ppio. de extensionalidad es equivalente a demostrar que:
            ∀x. evalEA' x = evalEA x
      
      Dem.:
      Sea e un elemento de EA. Por ppio. de ind. sobre la estructura de e.

      caso base, e = (Const n)
            evalEA' (Const n) = evalEA (Const n)
      
      caso inductivo, e = (BOp bop e1 e2)
            HI1) evalEA' e1 = evalEA e1 
            HI2) evalEA' e2 = evalEA e2 
            TI)  evalEA' (BOp bop e1 e2) = evalEA' (BOp bop e1 e2)

      
      ||caso base, e = (Const n)||

      LADO IZQ 
                  evalEA' (Const n)
            =                                         (def. evalEA')
                  foldEA id evalBOp (Const n)
            =                                         (def. foldEA.1)
                  id n
            =                                         (def. id)
                  n
      

      LADO DER
                  evalEA (Const n)
            =                                         (def. evalEA.1)
                  n



      ||caso inductivo, e = (BOp bop e1 e2)||

      LADO IZQ
                  evalEA' (BOp bop e1 e2)
            =                                         (def. evalEA')
                  foldEA id evalBOp (BOp bop e1 e2)
            =                                         (def. foldEA.2)
                  evalBOp bop (foldEA id evalBOp e1) (foldEA id evalBOp e2)
            =                                         (def. evalEA')
                  evalBOp bop (evalEA' e1) (evalEA' e2)

      LADO DER 
                  evalEA (BOp bop e1 e2)
            =                                         (def. evalEA.2)
                  evalBOp bop (evalEA e1) (evalEA e2)
            =                                         (HI1, HI2)
                  evalBOp bop (evalEA' e1) (evalEA' e2)
-}
