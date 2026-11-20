import Data.Char (ord)
import Prelude hiding (flip)
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

----------------------------------------------------------------
maxBy :: Ord b => (a -> b) -> a -> a -> a 
maxBy f x y = if f x >= f y 
                then x 
                else y

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b 
foldT z f EmptyT          = z 
foldT z f (NodeT x t1 t2) = f x (foldT z f t1) (foldT z f t2) 

ramaMasLarga :: Tree a -> [a]
ramaMasLarga = foldT [] (\x rmi rmd -> maxBy length (x:rmi) (x:rmd))

----------------------- Clase teórica 13 -----------------------
data E = Cte Int | Div E E

raiseError :: Maybe a
raiseError = Nothing

evalE :: E -> Int --original
evalE (Cte n)     = n 
evalE (Div e1 e2) = let v1 = evalE e1
                    in let v2 = evalE e2 
                        in v1 `div` v2

evalM :: E -> Maybe Int --error
evalM (Cte n)     = Just n
evalM (Div e1 e2) = case evalM e1 of 
                     Nothing -> Nothing 
                     Just v1 -> case evalM e2 of 
                                Nothing -> Nothing 
                                Just v2 -> if v2==0 
                                           then raiseError 
                                           else Just (v1 `div` v2)

type Output a = (a, Screen)
type Screen = String

printf :: String -> Screen 
printf msg = msg

formatDiv x y d = show x ++ "/" ++ show y ++ "=" ++ show d ++ "\n"

evalP :: E -> Output Int --imprimir
evalP (Cte n)     = (n, "")
evalP (Div e1 e2) = let (v1,p1) = evalP e1
                     in let (v2,p2) = evalP e2 
                         in let p3 = printf (formatDiv v1 v2 (v1 `div` v2)) 
                             in (v1 `div` v2, p1++p2++p3)


type StateT a = Mem -> (a, Mem)
type Mem = (Variable, Int)
type Variable = String

inc :: Variable -> Mem -> Mem 
inc "nDiv" ("nDiv", d) = ("nDiv", d+1)


evalST :: E -> StateT Int  --estado global
evalST (Cte n)     =  \mem -> (n, mem)
evalST (Div e1 e2) = \s0 -> let (v1, s1) = evalST e1 s0 
                             in let (v2, s2) = evalST e2 s1 
                                 in let s3 = inc "nDiv" s2 
                                     in ((v1 `div` v2), s3)


--Después de aplicar la técnica de los recuadros en evalM...
returnM :: a -> Maybe a 
returnM x = Just x 

bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
bindM m k = case m of 
             Nothing -> Nothing 
             Just v  -> k v


evalM1 :: E -> Maybe Int 
evalM1 (Cte n)     = returnM n
evalM1 (Div e1 e2) = 
        bindM (evalM1 e1)
              (\v1 -> bindM (evalM1 e2)
                            (\v2 -> if v2==0
                                    then raiseError
                                    else returnM (v1 `div` v2)))

-- bindM es la función más importante dentro de la definición. Decide si k se ejecuta o no.
-- Usando a bindM con forma infix (con el objetivo de hacerlo ver secuencial):
evalM2 :: E -> Maybe Int
evalM2 (Cte n)     = returnM n
evalM2 (Div e1 e2) = 
        evalM2 e1 `bindM` \v1 -> 
        evalM2 e2 `bindM` \v2 -> 
        if v2==0 
        then raiseError 
        else returnM (v1 `div` v2)

-- Otro caso
returnId :: Int -> Int 
returnId x = x 

bindId :: Int -> (Int -> Int) -> Int
bindId = \m k -> let v = m 
                  in k v

evalE1 :: E -> Int
evalE1 (Cte n)     = returnId n
evalE1 (Div e1 e2) = 
        evalE1 e1 `bindId` \v1 ->
        evalE1 e2 `bindId` \v2 -> 
        returnId (v1 `div` v2)

{-
Una mónada es un tipo paramétrico M a con operaciones básicas

return :: a -> M a                  --return
(>>=) :: M a -> (a -> M b) -> M b   --bind

que satisfacen las siguientes propiedades:
* ∀e.∀k. return e >>= k = k e 
* ∀m. m >>= \x -> return x = m
* ∀m.∀n.∀p. si x no aparece en p, entonces m >>= (\x -> n >>= \y -> p) = (m >>= \x -> n) >>= \y -> p

Una mónada incorpora *efectos* a un valor.
        - El tipo M a incorpora la información necesaria.
        - return agrega el efecto nulo.
        - (>>=) combina efectos con dependencia de datos.
Es una forma de abstraer comportamientos específicos en un cómputo.
        - Es una idea similar al pattern Strategy en OOP. 

¿Pero cuáles efectos combina una mónada?
- Cada mónada provee una o varias operaciones adicionales que la diferencian de otras mónadas.
        - Maybe provee raiseError
        - Output provee printf
        - StateT provee inc
Estas operaciones adicionales expresan los efectos específicos en cada caso.
-}

eval :: Monad m => E -> m Int 
eval (Cte n)     = return n 
eval (Div e1 e2) =
        eval e1 >>= \v1 -> 
        eval e2 >>= \v2 -> 
        return (v1 `div` v2)

{-
La clase Monad es predefinida.
Permite usar notación especial para mónadas.
        - La do-notation permite escribir en forma más "imperativa".

eval :: Monad m => E -> m Int 
eval (Cte n)     = return n 
eval (Div e1 e2) = do v1 <- eval e1 
                      v2 <- eval e2
                      return (v1 `div` v2)

Se expresa la función que es 2do argumento de (>>=) como una "asignación" (pero sigue siendo parámetro).

Un cómputo monádico empieza con la palabra clave do:

- La expresión                |   La expresión
        m >>= \x -> n         |         m >>= \_ -> n 
se escribe                    | se escribe
        do x <- m             |         do m
           n                  |            n

Si hay varios do seguidos, solo hace falta el primero.
El return queda sin cambios.

¿Cómo utilizar una mónada?
- Los valores monádicos solamente se combinan con otros valores monádicos.
- Se puede usar una mónada (perdiendo el efecto), pero no volver luego a la misma mónada.
        - Todo lo que requiera efecto se "traslada" hacia dentro de la mónada.
        - El uso de la mónada se puede realizar al final, para obtener el resultado.
-}


type Gramos = Int 
type Nombre = String 
data Sabor = Dulce | Neutro | Salado | Amargo | Acido 


data Receta = Ingrediente Nombre Gramos [Sabor]
            | Sustituto ([Sabor] -> Bool) Receta Receta 
            | Mezcla Receta Receta 
            | Montaje Receta Receta Receta 


foldReceta :: (Nombre -> Gramos -> [Sabor] -> b) -> (([Sabor] -> Bool) -> b -> b -> b) -> (b -> b -> b) -> (b -> b -> b -> b) -> Receta -> b 
foldReceta f g h p (Ingrediente n gr ss) = f n gr ss
foldReceta f g h p (Sustituto c o a)     = g c (foldReceta f g h p o) (foldReceta f g h p a)
foldReceta f g h p (Mezcla r1 r2)        = h (foldReceta f g h p r1) (foldReceta f g h p r2)
foldReceta f g h p (Montaje r1 r2 r3)    = p (foldReceta f g h p r1) (foldReceta f g h p r2) (foldReceta f g h p r3)


recReceta :: (Nombre -> Gramos -> [Sabor] -> b) -> (([Sabor] -> Bool) -> Receta -> Receta -> b -> b -> b) -> 
               (Receta -> Receta -> b -> b -> b) -> (Receta -> Receta -> Receta -> b -> b -> b -> b) -> Receta -> b
recReceta f g h p (Ingrediente n gr ss) = f n gr ss
recReceta f g h p (Sustituto c o a)     = g c o a (recReceta f g h p o) (recReceta f g h p a)
recReceta f g h p (Mezcla r1 r2)        = h r1 r2 (recReceta f g h p r1) (recReceta f g h p r2)
recReceta f g h p (Montaje r1 r2 r3)    = p r1 r2 r3 (recReceta f g h p r1) (recReceta f g h p r2) (recReceta f g h p r3)


sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = undefined

updateFAll :: Eq a => (b -> b -> b) -> [(a,b)] -> [(a,b)] -> [(a,b)]
updateFAll f l1 l2 = foldr (\(k,v) r xs -> updateF f k v (r xs)) id l2 l1

updateF :: Eq a => (b -> b -> b) -> a -> b -> [(a,b)] -> [(a,b)]
updateF f k v = recr [(k,v)] (\(k',v') xs r -> if k==k'
                                                then (k, f v v') : xs
                                                else (k', v') : r)



listaDeCompras :: Receta -> [(Nombre, Gramos)]
listaDeCompras r = sortBy (\p1 p2 -> compare (fst p1) (fst p2)) (listaDesordenada r)




listaDesordenada :: Receta -> [(Nombre, Gramos)]
listaDesordenada = foldReceta f g h p 
        where f n gr ss  = [(n, gr)]
              g c l1 l2  = updateFAll (+) l1 l2 
              h l1 l2    = updateFAll (+) l1 l2 
              p l1 l2 l3 = updateFAll (+) (updateFAll (+) l1 l2) l3


alcanzaDespensa :: [(Nombre, Gramos)] -> Receta -> Bool 
alcanzaDespensa d r = fst (maybeDespensa d r)

maybeDespensa :: [(Nombre, Gramos)] -> Receta -> (Bool, [(Nombre, Gramos)])
maybeDespensa d = flip (foldReceta f g h p) d
        where f n gr ss d = case lookup n d of 
                             Nothing -> (False, d)
                             Just x  -> if x >= gr 
                                        then (True, updateF const n (x-gr) d)
                                        else (False, d)
              g c fr1 fr2 d = let (b,d') = fr1 d 
                              in if b 
                                 then fr2 d'
                                 else (b,d')

              h fr1 fr2 d  = let (b,d') = fr1 d 
                              in if b
                                  then fr2 d'
                                  else (b,d')

              p fr1 fr2 fr3 d = case fr1 d of 
                                 (False, _) -> (False, d)
                                 (True, d') -> case fr2 d' of 
                                                (False, _) -> (False, d')
                                                (True, d'') -> fr3 d''