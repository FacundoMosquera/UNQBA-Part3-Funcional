import Data.Char (ord)
twice :: (a -> a) -> a -> a
twice f x =  f (f x)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

doble :: Int -> Int 
doble x = x + x 

uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f = g 
        where g p = f (swap p)

apply :: (a -> b) -> (a -> b)
apply f = g 
        where g x = f x

flip :: (b -> a -> c) -> a -> b -> c 
flip f = h 
        where h x = k 
                where k y = (f y) x

appFork :: (a -> b, a -> c) -> (a -> (b, c))
appFork (f, g) = h 
                where h x = (f x, g x) 

appDup :: ((a,a) -> b) -> a -> b
appDup f = g 
          where g x = f (x,x)

twice' :: (a -> a) -> a -> a 
twice' = appDup (uncurry (.))

compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

compose12 :: (d->c) -> (a->b->d) -> a -> b -> c 
compose12 f g x y = f (g x y)

data Arbol   a     = Hoja a | Nodo  a (Arbol a) (Arbol a)
data Tree    a     = EmptyT | NodeT a (Tree a) (Tree a)
data TipTree a     = Tip  a | Branch (TipTree a) (TipTree a)
data ABTree  a b   = Leaf b | Node a (ABTree a b) (ABTree a b)
data ExpTree a b c = Atom c
                   | BinOp a (ExpTree a b c) (ExpTree a b c)
                   | UnOp  b (ExpTree a b c)
data QuadTree a    = LeafQ a | NodeQ (QuadTree a) (QuadTree a)
                                     (QuadTree a) (QuadTree a)

--Arbol a es un caso particular de ABTree a b, cuando los elementos de los nodos y las hojas son del mismo tipo
type ArbolD a = ABTree a a

separateT :: ABTree a b -> (Tree a, TipTree b)
separateT (Leaf x)       = (EmptyT, Tip x)
separateT (Node x t1 t2) = let (tr1,tt1) = separateT t1 
                               (tr2,tt2) = separateT t2
                               in (NodeT x tr1 tr2, Branch tt1 tt2)

--fuseT :: (Tree a, TipTree b) -> ABTree a b

type KnowledgeTree s a = ABTree (s -> Bool) a

-- De ejemplo -------
type Data = String 
type Answer = String
---------------------

decide :: KnowledgeTree Data Answer -> (Data -> Answer)
decide (Leaf a)       d = a
decide (Node f k1 k2) d = if f d then decide k1 d
                                 else decide k2 d

-------------------------------------------------------
data Sintoma = Fiebre | Decaimiento | PerdidaDelGusto 
        deriving Eq
data Diagnostico = Gripe | ActivarProtocolo | ConsultarAlMedico | EntregarCertHabilitante

autoDoctor :: KnowledgeTree [Sintoma] Diagnostico
autoDoctor = Node f1 (Node f2 (Leaf ActivarProtocolo)
                              (Leaf Gripe))
                     (Node f3 (Leaf EntregarCertHabilitante)
                              (Leaf ConsultarAlMedico))
           where f1 sintomas = Fiebre `elem` sintomas
                 f2 sintomas = Decaimiento `elem` sintomas || PerdidaDelGusto `elem` sintomas
                 f3 sintomas = null sintomas

----------------- Representación de árboles mediante funciones -----------------
data FTree a = FT ([Dir] -> Maybe a)
data Dir = Izq | Der

t2ft :: Tree a -> FTree a 
t2ft EmptyT          = FT (\ds -> Nothing) 
t2ft (NodeT x t1 t2) = FT (\ds -> genNodeFT x (t2ft t1) (t2ft t2) ds)
                     where genNodeFT x _       _       []        = Just x
                           genNodeFT _ (FT f1) _       (Izq:ds') = f1 ds'
                           genNodeFT _ _       (FT f2) (Der:ds') = f2 ds'

{-
t2ft (NodeT 20 (NodeT 10 EmptyT EmptyT)
               (NodeT 30 EmptyT EmptyT))
        = 
            FT (\ds -> case ds of
                        []    -> Just 20
                        [Izq] -> Just 10
                        [Der] -> Just 30
                        _     -> Nothing)

        La función ES la estructura

(Hashing es un árbol)

-}


ft2t :: FTree a -> Tree a              -- No es recursión estructural (Puede generar árboles infintos)
ft2t (FT f) = generateFrom f []
        where generateFrom f ds = case f ds of 
                                    Nothing -> EmptyT
                                    Just x  -> NodeT x (generateFrom f (ds ++ [Izq]))
                                                       (generateFrom f (ds ++ [Der]))
-- Tree a es "estructuralmente" equivalente a un subconjunto de ([Dir] -> Maybe a)


theAnswerToLifeUniverseAndEverythingElseTree =
        NodeT 42 theAnswerToLifeUniverseAndEverythingElseTree
                 theAnswerToLifeUniverseAndEverythingElseTree

-- ft2t (FT (\_ -> Just 42)) = theAnswerToLifeUniverseAndEverythingElseTree

----------------------- Árboles generales (Rose trees) -----------------------
data GTree a = GNode a [GTree a] 
        deriving Show

gt :: GTree Int 
gt = GNode 1 [ GNode 2 [GNode 5 [], GNode 6 []], 
               GNode 3 [GNode 7 []], 
               GNode 4 []]

sumGT' :: GTree Int -> Int 
sumGT' (GNode x ts) = x + (sum . map sumGT) ts

sumGT'' :: GTree Int -> Int 
sumGT'' (GNode x ts) = x + foldr ((+) . sumGT') 0 ts

foldGT0 :: (a -> [b] -> b) -> GTree a -> b --No es estructural pq no hay garantía que h trate a la lista estructuralmente
foldGT0 h (GNode x ts) = h x (map (foldGT0 h) ts)

foldGT1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GTree a -> b --Esta función sí es completamente estructural pero es complicada.
foldGT1 g f z (GNode x ts) = g x (foldr f z (map (foldGT1 g f z) ts))

foldGT :: (a -> c -> b) -> ([b] -> c) -> GTree a -> b  --Esta función no es estructural, pero es fácil de que lo sea.
foldGT g h (GNode x ts) = g x (h (map (foldGT g h) ts))--Solamente hay poner como condición que h sea estructural.


sumGT0 :: GTree Int -> Int
sumGT0 = foldGT0 (\n ns -> n + sum ns)

sumGT1 :: GTree Int -> Int 
sumGT1 = foldGT1 (+) (+) 0 

sumGT :: GTree Int -> Int
sumGT = foldGT (+) sum

mirrorGT :: GTree a -> GTree a 
mirrorGT = foldGT GNode reverse

depthGT :: GTree a -> Int 
depthGT = foldGT ((+) . const 1) (\ns -> if null ns then 0 else maximum ns)


recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
recr z f []     = z 
recr z f (x:xs) = f x xs (recr z f xs)


--Mi solución
scoreOfAString :: String -> Int
scoreOfAString ss = let ascii = map ord ss
                    in foldr (\(a,b) n -> abs (a-b) + n) 0 (pairWith ascii)

pairWith :: [a] -> [(a,a)]
pairWith = recr [] (\x xs pairs -> case xs of
                                []       -> []
                                (x':xs') -> (x,x'):pairs)  

--solución del video
sos :: String -> Int 
sos = sum . map abs . mapAdjacent (-) . map ord

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f = recr [] (\x xs bs -> case xs of 
                                     (x':xs') -> f x x' : bs
                                     []       -> [])
--se puede evitar uno de los maps usando el operador .:, que aplica una operacion unaria al resultado de haber
--aplicado una operación binaria.
sos' :: String -> Int 
sos' = sum . mapAdjacent (abs .: (-)) . map ord


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d 
(.:) f g x y = f (g x y) 
