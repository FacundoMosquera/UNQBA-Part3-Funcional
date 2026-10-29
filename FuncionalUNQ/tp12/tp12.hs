import Prelude hiding (Right, Left, product)
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

-- ================================================================================= --
--                                  EJERCICIO 3                                      --
-- ================================================================================= --
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

--a)
foldT :: (a -> b -> b -> b) -> b -> Tree a -> b 
foldT f z EmptyT          = z
foldT f z (NodeT x t1 t2) = f x (foldT f z t1) (foldT f z t2)


--b)
mapT :: (a -> b) -> Tree a -> Tree b 
mapT f = foldT (NodeT . f) EmptyT


sumT :: Tree Int -> Int 
sumT = foldT (\n n1 n2 -> n + n1 + n2) 0


sizeT :: Tree a -> Int 
sizeT = foldT (\x n1 n2 -> 1 + n1 + n2) 0 


heighT :: Tree a -> Int 
heighT = foldT (\x n1 n2 -> 1 + max n1 n2) 0


preOrder :: Tree a -> [a]
preOrder = foldT (\x zs ys -> x : zs ++ ys) []


inOrder :: Tree a -> [a]
inOrder = foldT (\x zs ys -> zs ++ [x] ++ ys) []


postOrder :: Tree a -> [a]
postOrder = foldT (\x zs ys -> zs ++ ys ++ [x]) []


mirrorT :: Tree a -> Tree a 
mirrorT = foldT (flip . NodeT) EmptyT


countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT (\x n1 n2 -> (fromEnum . f) x + n1 + n2) 0


partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT juntarParticiones ([],[])
      where juntarParticiones x (ts1, fs1) (ts2, fs2) = if f x
                                                        then (x:ts1++ts2, fs1++fs2)
                                                        else (ts1++ts2,x:fs1++fs2)


zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c 
zipWithT f = foldT zipArboles (\t -> EmptyT)
      where zipArboles x g1 g2 tb = case tb of 
                                    (NodeT y tb1 tb2) -> NodeT (f x y) (g1 tb1) (g2 tb2)
                                    EmptyT            -> EmptyT


caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT (\x l1 l2 -> if length l1 > length l2 then x:l1 else x:l2) []


todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT (\x l1 l2 -> appendToAll x l1 l2) []
            where appendToAll x xss yss = let finAca = if null xss || null yss then [[x]] else []
                                          in finAca ++ map (x:) (xss ++ yss) 


todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT (\x ls1 ls2 -> [x] : zipLevels ls1 ls2) []

zipLevels :: [[a]] -> [[a]] -> [[a]]
zipLevels = foldr h id
      where h xs g yss = case yss of
                          (ys:yss') -> (xs ++ ys) : g yss'
                          []        -> xs : g []            


nivelN :: Tree a -> Int -> [a]
nivelN = foldT f (\n -> [])
      where f x g1 g2 n = if n==0 then [x] else g1 (n-1) ++ g2 (n-1)



t1 :: Tree Int 
t1 = NodeT 5 (NodeT 5 EmptyT (NodeT 5 EmptyT EmptyT))
            EmptyT
t2 :: Tree Int
t2 = NodeT 2 (NodeT 2 EmptyT EmptyT)
             (NodeT 2 EmptyT EmptyT)

t3 :: Tree Int 
t3 = NodeT 5 (NodeT 7 EmptyT 
                      (NodeT 1  EmptyT 
                                EmptyT))
             (NodeT 2 (NodeT 3  (NodeT 42 EmptyT EmptyT) 
                                EmptyT)
                      (NodeT 15 EmptyT 
                                EmptyT))

bst :: Tree Int 
bst = NodeT 5 (NodeT 4 (NodeT 2 EmptyT EmptyT) EmptyT)
              (NodeT 7 EmptyT EmptyT)



--c)
recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT z f EmptyT          = z
recT z f (NodeT x t1 t2) = f x t1 t2 (recT z f t1) (recT z f t2)


--d)
insertT :: Ord a => a -> Tree a -> Tree a 
--INV. REP.: El árbol es un BST.
insertT e = recT (NodeT e EmptyT EmptyT) h 
      where h x t1 t2 r1 r2 = if e==x 
                              then NodeT e t1 t2 
                              else if e<x 
                                    then NodeT x r1 t2
                                    else NodeT x t1 r2


caminoHasta :: Eq a => a -> Tree a -> [a]
--PRE.: El elemento existe en el árbol
caminoHasta x = recT [] f
      where f y t1 t2 r1 r2 = if x==y then [x] 
                              else if elem x r1 then y:r1
                                                else y:r2

caminoHasta' :: Eq a => a -> Tree a -> [a]
caminoHasta' x t = case maybeCamino x t of
                  Nothing -> error "El elemento no existe en el árbol"
                  Just xs -> xs

maybeCamino :: Eq a => a -> Tree a -> Maybe [a]
maybeCamino x = foldT h Nothing 
      where h y (Just xs) _ = Just (y:xs)
            h y _ (Just ys) = Just (y:ys)
            h y _  _        = if x==y then Just [y] else Nothing  


{-
e)
i)
            ∀f. sizeT . mapT f = sizeT
      Por ppio. de extensionalidad es equivalente a demostrar que:
            ∀t.∀f. (sizeT . mapT f) t = sizeT t 
      
      Dem.: 
      Sea t' un elemento de Tree a. Por ppio. de ind. sobre la estructura de t'.

      caso base, t' = EmptyT
                  (sizeT . mapT f) EmptyT = sizeT EmptyT

      caso inductivo, t' = (NodeT x t1 t2)
                  HI1) (sizeT . mapT f) t1 = sizeT t1
                  HI2) (sizeT . mapT f) t2 = sizeT t2
                  TI)  (sizeT . mapT f) (NodeT x t1 t2) = sizeT (NodeT x t1 t2)

      
      ||caso base, t' = EmptyT||

                  (sizeT . mapT f) EmptyT
            =                                         (def. (.))
                  sizeT (mapT f EmptyT)
            =                                         (def. mapT.1)
                  sizeT EmptyT

      
      ||caso inductivo, t' = (NodeT x t1 t2)||

      LADO IZQ
                  (sizeT . mapT f) (NodeT x t1 t2)
            =                                         (def. (.))
                  sizeT (mapT f (NodeT x t1 t2))
            =                                         (def.mapT)
                  sizeT (foldT (NodeT . f) EmptyT (NodeT x t1 t2))
            =                                         (def. foldT.2)
                  sizeT ((NodeT . f) x (foldT (NodeT . f) EmptyT t1) (foldT (NodeT . f) EmptyT t2))
            =                                         (def. mapT)
                  sizeT ((NodeT . f) x (mapT f t1) (mapT f t2))
            =                                         (def. (.))
                  sizeT (NodeT (f x) (mapT f t1) (mapT f t2))
            =                                         (def. sizeT)
                  foldT (\x n1 n2 -> 1 + n1 + n2) 0 (NodeT (f x) (mapT f t1) (mapT f t2))
            =                                         (def. foldT.2)
                  (\x n1 n2 -> 1 + n1 + n2) (f x) (foldT (\x n1 n2 -> 1 + n1 + n2) 0 (mapT f t1))
                                                  (foldT (\x n1 n2 -> 1 + n1 + n2) 0 (mapT f t2))
            =                                         (def. sizeT)
                  (\x n1 n2 -> 1 + n1 + n2) (f x) (sizeT (mapT f t1)) (sizeT (mapT f t2))
            =                                         (def. beta)
                  1 + (sizeT (mapT f t1)) + (sizeT (mapT f t2))
            =                                         (def. (.))
                  1 + ((sizeT . mapT f) t1) + ((sizeT . mapT f) t2)
            =                                         (HI1, HI2)
                  1 + sizeT t1 + sizeT t2
      

      LADO DER
                  sizeT (NodeT x t1 t2)
            =                                         (def. sizeT)
                  foldT (\x n1 n2 -> 1 + n1 + n2) 0 (NodeT x t1 t2)
            =                                         (def. foldT.2)
                  (\x n1 n2 -> 1 + n1 + n2) x (foldT (\x n1 n2 -> 1 + n1 + n2) 0 t1)
                                              (foldT (\x n1 n2 -> 1 + n1 + n2) 0 t2)
            =                                         (def. sizeT)
                  (\x n1 n2 -> 1 + n1 + n2) x (sizeT t1) (sizeT t2)
            =                                         (def. beta)
                  1 + sizeT t1 + sizeT t2






ii)
            ∀f.∀g. mapT f . mapT g = mapT (f . g)
      Por ppio. de extensionalidad es equivalente a demostrar que: 
            ∀t. ∀f.∀g. (mapT f . mapT g) t = mapT (f . g) t
      
      Dem.:
      Sean f y g funciones. Sea t' un elemento de Tree a.
      Por ppio. de ind. sobre la estructura de t'.

      caso base, t' = EmptyT
                  (mapT f . mapT g) EmptyT = mapT (f . g) EmptyT

      caso inductivo, t' = (NodeT x t1 t2)
                  HI1) (mapT f . mapT g) t1 = mapT (f . g) t1
                  HI2) (mapT f . mapT g) t2 = mapT (f . g) t2
                  TI)  (mapT f . mapT g) (NodeT x t1 t2) = mapT (f . g) (NodeT x t1 t2)


      
      ||caso base, t' = EmptyT||

      LADO IZQ
                  (mapT f . mapT g) EmptyT
            =                                         (def. (.))
                  mapT f (mapT g EmptyT)
            =                                         (def. mapT)}
                  mapT f ((foldT (NodeT . g) EmptyT) EmptyT)
            =                                         (def. foldT.1)
                  mapT f Empty
            =                                         (def. mapT)
                  foldT (NodeT . f) EmptyT EmptyT
            =                                         (def. foldT.1)
                  EmptyT

      LADO DER
                  mapT (f . g) EmptyT
            =                                         (def. mapT)
                  foldT (NodeT . (f . g)) EmptyT EmptyT
            =                                         (def. foldT.1)
                  EmptyT



      ||caso inductivo, t' = (NodeT x t1 t2)||

      LADO IZQ
                  (mapT f . mapT g) (NodeT x t1 t2)
            =                                         (def. (.))
                  mapT f (mapT g (NodeT x t1 t2))
            =                                         (def. mapT)
                  foldT (NodeT . f) EmptyT (foldT (NodeT . g) EmptyT (NodeT x t1 t2))
            =                                         (def. foldT.2)
                  foldT (NodeT . f) EmptyT ((NodeT . g) x (foldT (NodeT . g) EmptyT t1)
                                                          (foldT (NodeT . g) EmptyT t2))
            =                                         (def. mapT)
                  foldT (NodeT . f) EmptyT ((NodeT . g) x (mapT g t1) (mapT g t2))
            =                                         (def. (.))
                  foldT (NodeT . f) EmptyT ((NodeT (g x) (mapT g t1) (mapT g t2))
            =                                         (def. foldT.2)
                  (NodeT . f) (g x) (foldT (NodeT . f) EmptyT (mapT g t1)) 
                                    (foldT (NodeT . f) EmptyT (mapT g t2))
            =                                         (def. mapT)
                  (NodeT . f) (g x) (mapT f (mapT g t1)) (mapT f (mapT g t2))
            =                                         (def. (.))
                  NodeT (f (g x)) ((mapT f . mapT g) t1) ((mapT f . mapT g) t2)
            =                                         (HI1, HI2)
                  NodeT (f (g x)) (mapT (f . g) t1) (mapT (f . g) t2)
      

      LADO DER
                  mapT (f . g) (NodeT x t1 t2)
            =                                         (def. mapT)
                  foldT (NodeT . (f . g)) EmptyT (NodeT x t1 t2)
            =                                         (def. foldT.2)
                  NodeT ((f . g) x) (foldT (NodeT . (f . g)) EmptyT t1) (foldT (NodeT . (f . g)) EmptyT t2)
            =                                         (def. mapT)
                  NodeT ((f . g) x) (mapT (f . g) t1) (mapT (f . g) t2)
            =                                         (def. (.))
                  NodeT (f (g x)) (mapT (f . g) t1) (mapT (f . g) t2)








iii)
            foldT NodeT EmptyT = id 
      Por ppio. de extensionalidad es equivalente a demostrar que:
            ∀t. foldT NodeT EmptyT t = id t
      
      Dem.: 
      Sea t' un elemento de Tree a. Por ppio. de ind. sobre la estructura de t'.

      caso base, t' = EmptyT
                  foldT NodeT EmptyT EmptyT = id EmptyT

      caso inductivo, t' = (NodeT x t1 t2)
                  HI1) foldT NodeT EmptyT t1 = id t1
                  HI2) foldT NodeT EmptyT t2 = id t2
                  TI)  foldT NodeT EmptyT (NodeT x t1 t2) = id (NodeT x t1 t2)

      
      ||caso base, t' = EmptyT||

                  foldT NodeT EmptyT EmptyT
            =                                         (def. foldT.1)
                  EmptyT
            =                                         (def. id)
                  id EmptyT

      
      ||caso inductivo, t' = (NodeT x t1 t2)||

                  foldT NodeT EmptyT (NodeT x t1 t2)
            =                                         (def. foldT.2)
                  NodeT x (foldT NodeT EmptyT t1) (foldT NodeT EmptyT t2)
            =                                         (HI1, HI2)
                  NodeT x (id t1) (id t2)
            =                                         (def. id)
                  NodeT x t1 t2
            =                                         (def. id)
                  id (NodeT x t1 t2)

-}




-- ================================================================================= --
--                                  EJERCICIO 4                                      --
-- ================================================================================= --
type Record f v = [(f,v)]

ejemplo :: Record String String 
ejemplo = [("nombre","Grace"),("apellido","H"),("edad", "42"),("género","F"),("salario","120")]

type Table f v = [Record f v]

personas :: Table String String
personas = [
      [("nombre","Graciela"),("apellido","H") ,("edad", "42"),("género","F"),("salario","120")], 
      [("nombre","Alonso"),("apellido","Ch") ,("edad", "35"),("género","M"),("salario","250")], 
      [("nombre","Adela"),("apellido","G"), ("edad", "39"),("género","F"),("salario","130")], 
      [("nombre","Fer"),("apellido","R"), ("edad", "22"),("género","X"),("salario","100")], 
      [("nombre","Felipe"),("apellido","W"),("edad", "50"),("género","M"),("salario","280")]
      ]


--a)
select :: (Record f v -> Bool) -> Table f v -> Table f v 
select = filter

--b)
project :: (f -> Bool) -> Table f v -> Table f v 
project g = map (filter (g . fst))
 
--c)
conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool 
conjunct f g = (uncurry (&&) . appFork (f,g))


appFork :: (a -> b, a -> c) -> a -> (b, c)
appFork (f, g) x = (f x, g x)


--d)
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f = foldr (\x g ys -> map (f x) ys ++ g ys) (\ys -> [])


--e)
product :: Table f v -> Table f v -> Table f v 
product = crossWith (++)


--f)
similar :: Eq f => Record f v -> Record f v
similar = foldr removeOccur []
      where removeOccur (f,v) rs = case lookUp f rs of 
                                    Nothing -> (f,v):rs
                                    Just _  -> rs 


lookUp :: Eq f => f -> Record f v -> Maybe v
lookUp = flip (foldr (\p g f -> if fst p==f then Just (snd p) else g f) (\f-> Nothing))



-- ================================================================================= --
--                                  EJERCICIO 5                                      --
-- ================================================================================= --
data Query f v = Table [Record f v] --Table (Table f v)
               | Product (Query f v) (Query f v)
               | Projection (f -> Bool) (Query f v)
               | Selection (Record f v -> Bool) (Query f v)

--a)
foldQ :: ([Record f v] -> b) -> (b -> b -> b) -> 
            ((f -> Bool) -> b -> b) -> ((Record f v -> Bool) -> b -> b) -> Query f v -> b 
foldQ t pd pj s (Table rs)       = t rs
foldQ t pd pj s (Product q1 q2)  = pd (foldQ t pd pj s q1) (foldQ t pd pj s q2)
foldQ t pd pj s (Projection f q) = pj f (foldQ t pd pj s q) 
foldQ t pd pj s (Selection  f q) = s f (foldQ t pd pj s q) 


--b)
tables :: Query f v -> [Table f v]
tables = foldQ (:[]) (++) (flip const) (flip const)


execute :: Query f v  -> Table f v 
execute = foldQ id product project select 


compact :: Query f v  -> Query f v  
compact = foldQ Table Product compProjection compSelection 

compProjection :: (f -> Bool) -> Query f v -> Query f v  
compProjection f (Projection g q) = Projection (\x -> f x && g x) q 
compProjection f query            = Projection f query


compSelection :: (Record f v -> Bool) -> Query f v -> Query f v 
compSelection f (Selection g q) = Selection (\x -> f x && g x) q 
compSelection f query           = Selection f query



{-
b)
            execute . compact = execute 
      Por ppio. de extensionalidad es equivalente a demostrar que:
            ∀q. (execute . compact) q = execute q
      
      Dem.:
      Sea q' un elemento de Query f v. Por ppio. de ind. sobre la estructura de q'.

      caso base, q' = (Table rs)
                  (execute . compact) (Table rs) = execute (Table rs)

      caso inductivo 1, q' = (Product q1 q2)
                  HI1) (execute . compact) q1 = execute q1
                  HI2) (execute . compact) q2 = execute q2
                  TI)  (execute . compact) (Product q1 q2) = execute (Product q1 q2)

      caso inductivo 2, q' = (Projection f q)
                  HI) (execute . compact) q = execute q
                  TI) (execute . compact) (Projection f q) = execute (Projection f q)

      caso inductivo 3, q' = (Selection f q)
                  HI) (execute . compact) q = execute q
                  TI) (execute . compact) (Selection f q) = execute (Selection f q)


      ||caso base, q' = (Table rs)||

                  (execute . compact) (Table rs)
            =                                         (def. (.))
                  execute (compact (Table rs))
            =                                         (def. compact)
                  execute (foldQ Table Product compProjection compSelection (Table rs))
            =                                         (def. foldQ.1)
                  execute (Table rs)
      

      ||caso inductivo 1, q' = (Product q1 q2)||

      LADO IZQ      
                  (execute . compact) (Product q1 q2) 
            =                                         (def. (.))
                  execute (compact (Product q1 q2))
            =                                         (def. compact)
                  execute (foldQ Table Product compProjection compSelection (Product q1 q2))
            =                                         (def. foldQ.2)
                  execute (Product (foldQ Table Product compProjection compSelection q1) (foldQ Table Product compProjection compSelection q2))
            =                                         (def. compact)
                  execute (Product (compact q1) (compact q2))
            =                                         (def. execute)
                  foldQ id product project select (Product (compact q1) (compact q2))
            =                                         (def. foldQ.2)
                  product (foldQ id product project select (compact q1)) (foldQ id product project select (compact q2)) 
            =                                         (def. execute)
                  product (execute (compact q1)) (execute (compact q2))
            =                                         (def. (.))
                  product ((execute . compact) q1) ((execute . compact) q2)
            =                                         (HI1,HI2)
                  product (execute q1) (execute q2)

      LADO DER
                  execute (Product q1 q2)
            =                                         (def. execute)
                  foldQ id product project select (Product q1 q2)
            =                                         (def. foldQ.2)
                  product (foldQ id product project select q1) (foldQ id product project select q2)
            =                                         (def. execute)
                  product (execute q1) (execute q2)

      
      ||caso inductivo 2, q' = (Projection f q)||

      LADO IZQ
                  (execute . compact) (Projection f q)
            =                                         (def. (.))
                  execute (compact (Projection f q))
            =                                         (def. compact)
                  execute (foldQ Table Product compProjection compSelection (Projection f q))
            =                                         (def. foldQ.3)
                  execute (compProjection f (foldQ Table Product compProjection compSelection q))
            =                                         (def. compact)
                  execute (compProjection f (compact q))
      
      LADO DER
                  execute (Projection f q)
            =                                         (def. execute)
                  foldQ id product project select (Projection f q)
            =                                         (def. foldQ.3)
                  project f (foldQ id product project select q)
            =                                         (def. execute)
                  project f (execute q)
            =                                         (HI)
                  project f ((execute . compact) q)
            
            ||subcaso compact q = (Projection g q1)||
            |
            |LADO IZQ (continuación)
            |            execute (compProjection f (Projection g q1))
            |      =                                         (def. compProjection)
            |            execute (Projection (\x -> f x && g x) q1)
            |      =                                         (def. execute)
            |            foldQ id product project select (Projection (\x -> f x && g x) q1)
            |      =                                         (def. foldQ.3)
            |            project (\x -> f x && g x) (foldQ id product project select q1)
            |      =                                         (def. execute)
            |            project (\x -> f x && g x) (execute q1)
            |
            |LADO DER (continuación)
            |            project f ((execute . compact) q)
            |      =                                         (def. (.))
            |            project f (execute (compact q))
            |      =                                         (subcaso)
            |            project f (execute (Projection g q1))
            |      =                                         (def. execute)
            |            project f (foldQ id product project select (Projection g q1))
            |      =                                         (def. foldQ.3)
            |            project f (project g (foldQ id product project select q1))
            |      =                                         (def. execute)
            |            project f (project g (execute q1))
            |      =                                         (def. project)
            |            map (filter (f . fst)) (map (filter (g . fst)) (execute q1))
            |      =                                         (def (.))
            |            (map (filter (f . fst)) . map (filter (g . fst))) (execute q1)
            |      =                                         (prop. ∀f.∀g. map f . map g = map (f . g))
            |            map (filter (f . fst) . filter (g . fst))) (execute q1)
            |      =                                         (prop. ∀f.∀g. filter f . filter g = filter (\x -> f x && g x))
            |            map (filter (\x -> (f . fst) x && (g . fst) x)) (execute q1)
            |      =                                         (prop. (\x -> (f . fst) x && (g . fst) x) = (\x -> f x && g x) . fst )
            |            map (filter ((\x -> f x && g x) . fst)) (execute q1)
            |      =                                         (def. project)
            |            project (\x -> f x && g x) (execute q1)
            
            ||subcaso compact q != (Projection g q1)||
            |
            |LADO IZQ (continuación)
            |            execute (compProjection f (compact q))
            |      =                                         (def. compProjection.2)
            |            execute (Projection f (compact q))
            |      =                                         (def. execute)
            |            foldQ id product project select (Projection f (compact q))
            |      =                                         (def. foldQ.3)
            |            project f (foldQ id product project select (compact q))
            |      =                                         (def. (.))
            |            project f ((foldQ id product project select . compact) q)
            |      =                                         (HI)
            |            project f (foldQ id product project select q)
            |      =                                         (def. foldQ.3)
            |            foldQ id product project select (Projection f q)
            |      =                                         (def. execute)
            |            execute (Projection f q)
                  
            
      ||caso inductivo 3 se resuelve exactamente igual que el 2, pero con selection||

            
-}


-- ================================================================================= --
--                                  EJERCICIO 6                                      --
-- ================================================================================= --
data Dir = Left | Straight | Right deriving Show
data Mapa a = Cofre [a]
            | Nada (Mapa a)
            | Bifurcacion [a] (Mapa a) (Mapa a)

------------------------------ Ejemplos para testear ------------------------------
mapa :: Mapa Int
mapa = Bifurcacion [] (Nada (Cofre [1,2])) (Cofre [3])

mapa2 :: Mapa Int
mapa2 = Bifurcacion [1, 3] (Nada (Bifurcacion [5] (Cofre [7, 8]) 
                                                  (Nada (Nada (Cofre []))))) 
                           (Bifurcacion [] (Nada (Nada (Cofre [10, 12])))     
                                           (Bifurcacion [9, 11] (Cofre []) 
                                                                (Cofre [20, 21])))
-----------------------------------------------------------------------------------

--a)
foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b 
foldM f g h (Cofre xs)             = f xs
foldM f g h (Nada m)               = g (foldM f g h m)
foldM f g h (Bifurcacion xs m1 m2) = h xs (foldM f g h m1) (foldM f g h m2)


recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b 
recM f g h (Cofre xs)             = f xs
recM f g h (Nada m)               = g m (recM f g h m)
recM f g h (Bifurcacion xs m1 m2) = h xs m1 m2 (recM f g h m1) (recM f g h m2)


--b)
objects :: Mapa a -> [a]
objects = foldM id id (\l1 l2 l3 -> l1 ++ l2 ++ l3)


mapM :: (a -> b) -> Mapa a -> Mapa b 
mapM f = foldM (Cofre . map f) Nada (Bifurcacion . map f)


has :: (a -> Bool) -> Mapa a -> Bool
has p = foldM (any p) id (\xs b1 b2 -> any p xs || b1 || b2)


hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt p = foldM dirCofre dirNada dirBif 
      where dirCofre xs ds     = case ds of 
                                  [] -> any p xs 
                                  _  -> False
            dirNada fr ds      = case ds of
                                 (Straight:ds') -> fr ds' 
                                 _              -> False 
            dirBif xs r1 r2 ds = case ds of 
                                 (Left:ds')  -> r1 ds' 
                                 (Right:ds') -> r2 ds'
                                 []          -> any p xs
                                 _           -> False


longestPath :: Mapa a -> [Dir]
longestPath = foldM (const []) (Straight:) (\xs l1 l2 -> if length l1 > length l2 then Left:l1 else Right:l2)


objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath = subst collect longestPath


collect :: Mapa a -> [Dir] -> [a]
collect = foldM const collN collB 
      where collN r ds = case ds of 
                          (Straight:ds') -> r ds'
                          _              -> []
            collB xs r1 r2 ds = case ds of
                                (Left:ds')  -> xs ++ r1 ds'
                                (Right:ds') -> xs ++ r2 ds'  
                                _           -> xs 


subst :: (a -> b -> c) -> (a -> b) -> a -> c 
subst f g x = f x (g x)


allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (const []) (appendToAll Straight) (\xs dsl dsr -> appendToAll Left dsl ++ appendToAll Right dsr)
      where appendToAll x []  = [[x]]
            appendToAll x xss = map (x:) xss 


objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM (:[]) ([]:) (\xs l1 l2 -> xs : zipLevels l1 l2)

{-
c)
            ∀x. has (==x) = any (elem x) . objectsPerLevel
      Por ppio. de extensionalidad es equivalente a demostrar que:
            ∀m. ∀x. has (==x) m = (any (elem x) . objectsPerLevel) m
      
      Dem.:
      Sea e un elemento. Sea m' un elemento de Mapa a.
      Por ppio. de ind. sobre la estructura de m'.

      caso base, m' = (Cofre xs)
                  has (==e) (Cofre xs) = (any (elem e) . objectsPerLevel) (Cofre xs)

      caso inductivo, m' = (Nada ms)
                  HI) has (==e) ms = (any (elem e) . objectsPerLevel) ms
                  TI) has (==e) (Nada ms) = (any (elem e) . objectsPerLevel) (Nada ms)

      caso inductivo, m' = (Bifurcacion xs m1 m2)
                  HI1) has (==e) m1 = (any (elem e) . objectsPerLevel) m1
                  HI2) has (==e) m2 = (any (elem e) . objectsPerLevel) m2
                  TI)  has (==e) (Bifurcacion xs m1 m2) = (any (elem e) . objectsPerLevel) (Bifurcacion xs m1 m2)

      
      ||caso base, m' = (Cofre xs)||

      LADO IZQ
                  has (==e) (Cofre xs)
            =                                         (def. has)
                  foldM (any (==e)) id (\xs b1 b2 -> any p xs || b1 || b2) (Cofre xs)
            =                                         (def. foldM.1)
                  any (==e) xs
      
      LADO DER
                  (any (elem e) . objectsPerLevel) (Cofre xs)
            =                                         (def. (.))
                  any (elem e) (objectsPerLevel (Cofre xs))
            =                                         (def. objectsPerLevel)
                  any (elem e) (foldM (:[]) ([]:) (\xs l1 l2 -> xs : zipLevels l1 l2) (Cofre xs))
            =                                         (def. foldM.1)
                  any (elem e) (:[]) xs
            =                                         (def. (:))
                  any (elem e) [xs]

-}



-- ================================================================================= --
--                                  EJERCICIO 7                                      --
-- ================================================================================= --
data GTree a = GNode a [GTree a]
      deriving Show


------------------------------ Ejemplos para testear ------------------------------
gtree :: GTree Int 
gtree = GNode 1 [GNode 10 [], 
                 GNode 20 [GNode 200 [], 
                           GNode 210 [GNode 2100 []], 
                           GNode 220 []], 
                 GNode 30 [GNode 300 []] ]

gtree42 :: GTree Int 
gtree42 = GNode 42 [GNode 42 [GNode 42 []], GNode 42 []]
-----------------------------------------------------------------------------------

{-
a)
Dar tipo y definir las tres versiones de foldGT y recGT, que expresan los
esquemas de recursión estructural y primitiva, respectivamente, para la
estructura GTree.
ATENCIÓN: recordar que no siguen la secuencia dada para tipos recurslvos
con recursión directa, porque la recursión en GTree NO es directa. Estas
versiones deben tener en cuenta el esquema de recursión sobre listas
correspondiente, como se trató en clase teórica.
-}

foldGT0 :: (a -> [b] -> b) -> GTree a -> b
foldGT0 f (GNode x ts) = f x (map (foldGT0 f) ts)

foldGT1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GTree a -> b 
foldGT1 f g z (GNode x ts) = f x (foldr (g . foldGT1 f g z) z ts) 

foldGT :: (a -> c -> b) -> ([b] -> c) -> GTree a -> b 
foldGT f g (GNode x ts) = f x (g (map (foldGT f g) ts))


recGT0 :: (a -> [GTree a] -> [b] -> b) -> GTree a -> b
recGT0 f (GNode x ts) = f x ts (map (recGT0 f) ts)

recGT1 :: (a -> [GTree a] -> c -> b) -> (b -> [b] -> c -> c) -> c -> GTree a -> b
recGT1 f g z (GNode x ts) = f x ts (recr z g (map (recGT1 f g z) ts))

recGT :: (a -> [GTree a] -> c -> b) -> ([b] -> c) -> GTree a -> b 
recGT f g (GNode x ts) = f x ts (g (map (recGT f g) ts))


recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
recr z f []     = z 
recr z f (x:xs) = f x xs (recr z f xs)



--b)
mapGT :: (a -> b) -> GTree a -> GTree b 
mapGT f = foldGT (GNode . f) id 

mapGT0 :: (a -> b) -> GTree a -> GTree b 
mapGT0 f = foldGT0 (GNode . f)

mapGT1 :: (a -> b) -> GTree a -> GTree b 
mapGT1 f = foldGT1 (GNode . f) (:) []


sumGT :: GTree Int -> Int 
sumGT = foldGT (+) sum


sizeGT :: GTree a -> Int 
sizeGT = foldGT ((+) . const 1) sum


heightGT :: GTree a -> Int 
heightGT = foldGT ((+) . const 1) (\ns -> if null ns then 0 else maximum ns)


flat :: [[a]] -> [a]
flat = foldr (++) []

preOrderGT :: GTree a -> [a]
preOrderGT = foldGT (:) flat


postOrderGT :: GTree a -> [a]
postOrderGT = foldGT (flip (++) . (:[])) flat


mirrorGT :: GTree a -> GTree a
mirrorGT = foldGT GNode reverse


countByGT :: (a -> Bool) -> GTree a -> Int 
countByGT p = foldGT ((+) . fromEnum . p) sum


partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
partitionGT p = foldGT1 (\x (t,f) -> if p x then (x:t,f) else (t,x:f))
                        (\(t1,f1) (t2,f2) -> (t1++t2, f1++f2))
                        ([],[])


zipWithGT :: (a -> b -> c) -> GTree a -> GTree b -> GTree c
zipWithGT f = foldGT (\x r (GNode y ty) -> GNode (f x y) (r ty))
                     zipFX
            where zipFX = foldr (\f r xs -> case xs of 
                                             []      -> []
                                             (x:xs') -> f x : r xs')
                                (\xs -> [])


caminoMasLargoGT :: GTree a -> [a]
caminoMasLargoGT = foldGT (:) (\xss -> if null xss then [] else maxList xss)
            where maxList = recr (error "La lista esta vacía.") (\xs xss r -> if null xss 
                                                                              then xs
                                                                              else if length xs > length r
                                                                                    then xs 
                                                                                    else r)


todosLosCaminosGT :: GTree a -> [[a]]
todosLosCaminosGT = foldGT (\x xss -> if null xss then [[x]] else map (x:) xss) (foldr (++) [])


todosLosNivelesGT :: GTree a -> [[a]]
todosLosNivelesGT = foldGT (\x xss -> [x] : xss) (foldr zipLevels [])


caminoHastaGT :: Eq a => a -> GTree a -> [a]
caminoHastaGT x =  (extraerCamino . maybeCaminoGT x)
      where extraerCamino m = case m of   
                              Nothing -> error "El elemento no está en el árbol."
                              Just xs -> xs

maybeCaminoGT :: Eq a => a -> GTree a -> Maybe [a]
maybeCaminoGT x = foldGT (\y mb -> if x==y then Just [y] else case mb of
                                                              Just xs -> Just (y:xs)
                                                              Nothing -> Nothing) (foldr g Nothing)
      where g (Just xs) mb2 = Just xs
            g m2 (Just xs)  = Just xs 
            g _  _          = Nothing


nivelNGT :: GTree a -> Int -> [a]
nivelNGT = foldGT (\x r n -> if n==0 then [x] else r (n-1))
                  (\fs n -> foldr ((++) . flip apply n) [] fs)

apply :: (a -> b) -> a -> b 
apply f x = f x


