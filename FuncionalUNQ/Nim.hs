data Nim = Empty -- juego vacío
         | Heap Int Nim -- pila con n fichas (n > 0)
         deriving Show

type Move = (Int, Int)  -- jugada: de la fila i remover k fichas

data GameTree = Nil 
              | Node (Move, GameTree) -- (jugada, hijos - jugadas del contricante)
                     GameTree         -- hermanos (otras jugadas propias)
            deriving Show


-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --
foldNim :: (Int -> b -> b) -> b -> Nim -> b 
foldNim f z Empty      = z
foldNim f z (Heap k n) = f k (foldNim f z n)

recNim :: (Int -> Nim -> b -> b) -> b -> Nim -> b 
recNim f z Empty      = z
recNim f z (Heap k n) = f k n (recNim f z n)


foldGame :: (Move -> b -> b -> b) -> b -> GameTree -> b 
foldGame f z Nil              = z
foldGame f z (Node (m,hi) he) = f m (foldGame f z hi) (foldGame f z he)

recGame :: (Move -> GameTree -> b -> GameTree -> b -> b) -> b -> GameTree -> b 
recGame f z Nil              = z
recGame f z (Node (m,hi) he) = f m hi (recGame f z hi) he (recGame f z he)



-- ================================================================================= --
--                                  EJERCICIO 2                                      --
-- ================================================================================= --
--a)
heaps :: Nim -> Int 
heaps = foldNim ((+) . const 1) 0 


--b)
chips :: Nim -> Int 
chips = foldNim (+) 0


--c)
maxHeap :: Nim -> Int 
maxHeap = recNim (\n nim k -> case nim of 
                               Empty -> n 
                               _     -> max n k) (error "El Nim está vacío.")

maxHeap' :: Nim -> Int 
maxHeap' = foldNim max 0


--d)
alongside :: Nim -> Nim -> Nim
alongside = foldNim (\k fr nim -> Heap k (fr nim)) id


--e) 
gameHeight :: GameTree -> Int 
gameHeight = foldGame (\m hs hb -> 1 + hs `max` hb) 0


--f)
branches :: GameTree -> [[Move]]
branches = foldGame (\m ls lb -> if null ls
                                    then [[m]] ++ lb
                                    else map (m:) ls ++ lb) []



-- ================================================================================= --
--                                  EJERCICIO 3                                      --
-- ================================================================================= --
--a)
turn :: Nim -> Move -> Maybe Nim 
turn = recNim g (const Nothing) 
        where g k nim fr (0,n) = if k < n then Nothing else Just (compNim (Heap (k-n) nim))
              g k nim fr (m,n) = if m < 0 then Nothing else case fr (m-1,n) of 
                                                              Nothing -> Nothing 
                                                              Just ns -> Just (Heap k ns)
              compNim (Heap 0 n) = n 
              compNim n          = n

--b)
moves :: Nim -> [Move]
moves = foldNim (\k ls -> genMoves k ++ map (\(i,f) -> (i+1,f)) ls) []
        where genMoves 1 = [(0,1)]
              genMoves n = (0,n) : genMoves (n-1)


--c)
solve :: Nim -> GameTree 
solve n = genGT n (moves n)


genGT :: Nim -> [Move] -> GameTree 
genGT n = foldr (\move gt -> case turn n move of 
                              Nothing  -> gt 
                              Just nim -> Node (move, solve nim) gt) Nil


-- ================================================================================= --
--                                  EJEMPLO TEST                                     --
-- ================================================================================= --
n1 :: Nim 
n1 = Heap 3 Empty

n2 :: Nim 
n2 = Heap 2 (Heap 1 Empty)

gt :: GameTree
gt = Node ((0,1), (Node ((0,1), (Node ((1,1), Nil) Nil)) (Node ((1,1), (Node ((0,1), Nil) Nil)) Nil)))
          (Node ((0,2), Node ((1,1), Nil) Nil) (Node ((1,1), Node ((0,1), (Node ((0,1), Nil) Nil)) (Node ((0,2), Nil) Nil) ) Nil))