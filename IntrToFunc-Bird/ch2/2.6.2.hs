data Jane = MkJane Int 

newtype Dick = MkDick Int

mkNewJane :: Int -> Jane 
mkNewJane n = MkJane n 

mkNewDick :: Int -> Dick 
mkNewDick n = MkDick n

infinity :: Int 
infinity = 1 + infinity

jane = mkNewJane infinity
dick = mkNewDick infinity

terminated :: Either Jane Dick -> Bool 
terminated (Left j)  = j `seq` True 
terminated (Right d) = d `seq` True  --Cuando evalúe a Dick va a fallar