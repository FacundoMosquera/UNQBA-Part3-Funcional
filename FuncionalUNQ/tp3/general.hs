derive :: (Float -> Float) -> (Float -> Float)
derive f x = (f (x+h) - f x) / h where h = 0.000001

deriveN :: Int -> (Float -> Float) -> (Float -> Float)
deriveN 0 f = f 
deriveN n f = deriveN (n-1) (derive f)

deriveN' :: Int -> (Float -> Float) -> (Float -> Float)
deriveN' 0 f = f 
deriveN' n f = derive (deriveN (n-1) f)

doble :: Num a => a -> a 
doble x = x + x

uderive :: (Float -> Float, Float) -> Float 
uderive (f,x) = (f (x+h) - f x) / h where h = 0.000001

uderiveN :: (Int, Float -> Float, Float) -> Float
uderiveN (0,f,x) = f x
uderiveN (n,f,x) = let df = \x -> uderive (f, x)
                    in uderiveN (n-1, df, x)
