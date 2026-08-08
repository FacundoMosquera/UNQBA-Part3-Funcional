import Prelude hiding (Eq, (==))

class Eq a where 
    (==), (!=) :: a -> a -> Bool

instance Eq Bool where
    (==) x y = (x && y) || (not x && not y)
    (!=) x y = not (x == y)
