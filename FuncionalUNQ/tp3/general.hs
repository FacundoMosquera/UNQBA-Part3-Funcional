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



{-
    compose compose compose :: (b -> c) -> (z -> a -> b) -> z -> a -> c    #HIPOTESIS CALCULADA A OJO 



    compose :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
    compose :: (b2 -> c2) -> (a2 -> b2) -> a2 -> c2 
    ----------------------------------------------- b1 <- b2 -> c2, c1 <- (a2 -> b2) -> a2 -> c2
    compose compose :: (a1 -> b2 -> c2) -> a1 -> (a2 -> b2) -> a2 -> c2


    compose compose :: (a1 -> b2 -> c2) -> a1 -> (a2 -> b2) -> a2 -> c2
    compose :: (b -> c) -> (a -> b) -> (a -> c)
    ------------------------------------------------------------- a1 <- b -> c, b2 <- a -> b, c2 -> a -> c
    compose compose compose :: (b -> c) -> (a2 -> a -> b) -> a2 -> a -> c

-}