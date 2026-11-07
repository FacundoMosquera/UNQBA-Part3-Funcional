data PredExp a = Pred (a -> Bool)
               | LOp LogicOp (PredExp a) (PredExp a)
               | Not (PredExp a)

data LogicOp = AND | OR


data SetExp a = Empty 
              | Unit a 
              | Union (SetExp a) (SetExp a)


liftB :: (Bool -> Bool) -> (a -> Bool) -> (a -> Bool)
liftB f p x = f (p x)


liftB2 :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> (a -> Bool)
liftB2 f p1 p2 x = f (p1 x) (p2 x)


evalPE :: PredExp a -> (a -> Bool)
evalPE (Pred p)        = p
evalPE (LOp lop p1 p2) = uncurry (evalLOP lop) . appFork (evalPE p1, evalPE p2) 
evalPE (Not p)         = (not . evalPE p)


appFork :: (a -> b, a -> c) -> a -> (b,c)
appFork (f,g) x = (f x, g x)


evalLOP :: LogicOp -> (Bool -> Bool -> Bool)
evalLOP AND = (&&)
evalLOP OR  = (||)


sExp2pExp :: Eq a => SetExp a -> PredExp a 
sExp2pExp Empty         = Pred (const False) 
sExp2pExp (Unit x)      = Pred (==x)
sExp2pExp (Union s1 s2) = LOp OR (sExp2pExp s1) (sExp2pExp s1)


evalSE :: Eq a => SetExp a -> (a -> Bool)
evalSE Empty         = const False
evalSE (Unit x)      = (==x)
evalSE (Union s1 s2) = (\x -> evalSE s1 x || evalSE s2 x)


mapSE :: (a -> b) -> SetExp a -> SetExp b 
mapSE f Empty         = Empty
mapSE f (Unit x)      = Unit (f x) 
mapSE f (Union s1 s2) = Union (mapSE f s1) (mapSE f s2)


mapPE :: (a -> b) -> PredExp b -> PredExp a 
mapPE f (Pred p)        = Pred (p . f)
mapPE f (LOp lop p1 p2) = LOp lop (mapPE f p1) (mapPE f p2)
mapPE f (Not p)         = Not (mapPE f p)


foldPE :: ((a -> Bool) -> b) -> (LogicOp -> b -> b -> b) -> (b -> b) -> PredExp a -> b
foldPE f g h (Pred p)        = f p
foldPE f g h (LOp lop p1 p2) = g lop (foldPE f g h p1) (foldPE f g h p2)
foldPE f g h (Not p)         = h (foldPE f g h p)


foldSE :: (a -> b) -> (b -> b -> b) -> b -> SetExp a -> b 
foldSE f g z Empty         = z
foldSE f g z (Unit x)      = f x
foldSE f g z (Union s1 s2) = g (foldSE f g z s1) (foldSE f g z s2)