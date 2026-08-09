import Prelude hiding (Eq)

class Eq a where 
    (!=) :: a -> a -> Bool
    (!=) x y = True

data Triangle = Scalene | Isosceles | Equilateral 

instance Eq Triangle 
