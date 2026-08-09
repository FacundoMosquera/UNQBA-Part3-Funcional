data Triangle = Failure | Isosceles | Equilateral | Scalene 

analyse :: (Int, Int, Int) -> Triangle 
analyse (x,y,z) 
        | a + b <= c = Failure
        | a == c     = Equilateral
        | (a == b) || (b == c) = Isosceles
        | otherwise = Scalene
        where (a,b,c) = sort3 (x,y,z)

sort3 :: (Int, Int, Int) -> (Int, Int, Int)
sort3 (x,y,z) = 
        let (x1, y1) = sort2 (x, y)
            (y2, c)  = sort2 (y1, z)
            (a, b)   = sort2 (x1, y2)
            in (a, b, c)

sort2 :: (Int, Int) -> (Int, Int)
sort2 (x,y) = if x < y then (x,y) else (y,x)