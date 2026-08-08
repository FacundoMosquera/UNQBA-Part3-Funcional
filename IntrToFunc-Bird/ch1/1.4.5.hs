appliedToOne :: (Integer -> Integer) -> Integer 
appliedToOne f = f 1

idF :: (Integer -> Integer) -> (Integer -> Integer)
idF f = f