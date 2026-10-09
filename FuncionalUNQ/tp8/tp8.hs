import Prelude hiding (unzip, zip, reverse, (++), any, all, elem, concat, product, sum, length)
-- EJERCICIO 1
--a)
length :: [a] -> Int 
length []     = 0
length (x:xs) = 1 + length xs

--b)
sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) = x + sum xs

--c)
product :: [Int] -> Int 
product []     = 1
product (x:xs) = x * product xs

--d)
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

--e)
elem :: Eq a => a -> [a] -> Bool 
elem x []     = False
elem x (y:ys) = (x==y) || elem x ys

--f)
all :: (a -> Bool) -> [a] -> Bool 
all f []     = True
all f (x:xs) = f x && all f xs

--g)
any :: (a -> Bool) -> [a] -> Bool 
any f []     = False
any f (x:xs) = f x || any f xs

--h)
count :: (a -> Bool) -> [a] -> Int 
count f []     = 0
count f (x:xs) = (fromEnum . f) x + count f xs

--i)
subset :: Eq a => [a] -> [a] -> Bool 
subset [] ys     = True
subset (x:xs) ys = any (x==) ys && subset xs ys

subset' :: Eq a => [a] -> [a] -> Bool 
subset' xs ys = all (flip elem ys) xs

--j)
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (++) xs ys

--k)
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

--l)
zip :: [a] -> [b] -> [(a,b)]
zip []     ys = []
zip (x:xs) ys = case ys of  
                (z:zs) -> (x,z) : zip xs zs
                _      -> []

--m)
unzip :: [(a,b)] -> ([a],[b])
unzip []       = ([],[])
unzip (xy:xys) = let (x,y)    = xy
                     (xs, ys) = unzip xys
                     in (x:xs, y:ys)

