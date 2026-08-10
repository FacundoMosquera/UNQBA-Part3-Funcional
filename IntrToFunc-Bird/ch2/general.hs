import Prelude hiding (fromEnum, Enum, Eq, (==))

class Eq a where 
    (==), (!=) :: a -> a -> Bool

instance Eq Bool where
    (==) x y = (x && y) || (not x && not y)
    (!=) x y = not (x == y)


data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat 

class Enum a where
    fromEnum :: a -> Int 
    toEnum :: Int -> a

instance Enum Day where
    fromEnum Sun = 0 
    fromEnum Mon = 1 
    fromEnum Tue = 2
    fromEnum Wed = 3
    fromEnum Thu = 4 
    fromEnum Fri = 5 
    fromEnum Sat = 6

    toEnum 0 = Sun  
    toEnum 1 = Mon  
    toEnum 2 = Tue
    toEnum 3 = Wed
    toEnum 4 = Thu  
    toEnum 5 = Fri  
    toEnum 6 = Sat

{- 
    instance Eq Day where
        x == y = (fromEnum x == fromEnum y) 

    No compila porque no declaré Eq Int para que se pueda realizar la operación.    
-}