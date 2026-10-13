import Prelude hiding (unzip, zip, reverse, (++), any, all, elem, concat, product, sum, length)
-- ====================== SECCIÓN 1 ====================== --

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


-- EJERCICIO 2
{-
a)
    ∀xs.∀ys. length (xs ++ ys) = length xs + length ys

    Sea xs un elemento arbitrario de [a]. Se demostrará la propiedad utilizando el 
    ppio. de inducción sobre la estructura de xs.

    ||| CASO xs = [] |||

    LADO IZQ         
            length ([] ++ ys)
        =                   (def. ++)
            length ys

    LADO DER 
            length [] + length ys
        =                   (def. length.1)
            0 + length ys
        =                   (aritmética)
            length ys

    
    ||| CASO xs = (z:zs) |||
        HI) length (zs ++ ys) = length zs + length ys
        TI) length ((z:zs) ++ ys) = length (z:zs) + length ys

    LADO IZQ 
            length((z:zs) ++ ys)
        =                   (def. ++.2)
            length(z : (zs ++ ys))
        =                   (def. length.2)
            1 + length (zs ++ ys)

    
    LADO DER 
            length (z:zs) + length ys
        =                   (def. length.2)
            1 + length zs + length ys
        =                   (HI)
            1 + length (zs ++ ys)




b)
    ∀xs.∀ys.∀zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

    Sean xs, ys, zs elementos arbitrarios de [a]. Por ppio. de ind. sobre la estructura de xs.

    ||| CASO xs = [] |||

    LADO IZQ 
            ([] ++ ys) ++ zs
        =                   (def. ++.1, ys' <- ys)
            ys ++ zs


    LADO DER 
            [] ++ (ys ++ zs)
        =                   (def. ++.1, ys' <- (ys ++ zs))
            ys ++ zs
        


    ||| CASO xs = (x:xs') |||
        HI) (xs' ++ ys) ++ zs = xs' ++ (ys ++ zs)
        TI) ((x:xs') ++ ys) ++ zs = (x:xs') ++ (ys ++ zs) 

    LADO IZQ 
            ((x:xs') ++ ys) ++ zs
        =                   (def. ++.2)
            (x : (xs' ++ ys)) ++ zs
        =                   (def.++.2)
            x : ((xs' ++ ys) ++ zs)
        

    LADO DER 
            (x:xs') ++ (ys ++ zs)
        =                   (def. ++.2)
            x : (xs' ++ (ys ++ zs))
        =                   (HI)
            x : ((xs' ++ ys) ++ zs)
        =                   (def. ++.2)




c)
        count (const True) = length
    Por ppio. de extensionalidad es equivalente a demostrar:
        ∀xs. count (const True) xs = length xs
    Sea xs un elemento arbitrario de [a]. Por ppio. de ind. sobre xs:


    ||| CASO xs = [] |||

    LADO IZQ
            count (const True) []
        =                   (def. count.1)
            0

    
    LADO DER
            length []
        =                   (def. length.1)
            0




    ||| CASO xs = (x:xs') |||
        HI) count (const True) xs' = length xs'
        TI) count (const True) (x:xs') = length (x:xs')
    
    LADO IZQ 
            count (const True) (x:xs')
        =                   (def. count.2)
            if (const True x) then 0 + count (const True) xs' else 1 + count (const True) xs'
        =                   (def. const)
            if True then 1 + count (const True) xs' else 0 + count (const True) xs'
        =                   (rama then)
            1 + count (const True) xs'
    

    LADO DER
            length (x:xs')
        =                   (def. length.2)
            1 + length xs'
        =                   (HI)
            1 + count (const True) xs'

    

d)
        elem = any . (==)
    Por ppio. de extensionalidad, es equivalente a ∀x.∀xs. elem x xs = (any . (==)) x xs
    Sea z un elemento cualquiera y zs un elemento arbitrario de [a]. Por ppio. de ind. de zs.

    ||| CASO z = [] ||| 
        
        elem z [] = (any . (==)) z []

    
    LADO IZQ 
            elem z []
        =                   (def. elem.1)
            False


                          
    LADO DER 
            (any . (==)) z []
        =                   (def. (.))
            any ((==) z) []
        =                   (def. any.1)
            False


    ||| CASO zs = (y:ys) |||
        HI) elem z ys = (any . (==)) z ys
        TI) elem z (y:ys) = (any . (==)) z (y:ys)

    
    LADO IZQ 
            elem z (y:ys) 
        =                   (def. elem.2)
            z == y || elem z ys
        =                   (HI)
            z== y || (any . (==)) z ys


    LADO DER    
            (any . (==)) z (y:ys) 
        =                   (def. (.))
            any ((==) z) (y:ys)
        =                   (def. any.2)
            ((==) z) y || any ((==) z) ys
        =                   (infix op)
            z == y || (any . (==)) z ys


    


e)
        ∀x. any (elem x) = elem x . concat
    Por ppio, de extensionalidad es equivalente a desmostrar que:
        ∀xss.∀x. any (elem x) xss = (elem x . concat) xss
    Sea e un elemento arbitrario y l una lista de listas del mismo tipo de e.
    Por ppio. de inducción sobre la estructura de l.

    ||| CASO BASE l = [] |||     any (elem e) [] = (elem e . concat) []

    LADO IZQ
            any (elem e) []
        =                   (def. any.1)
            False


    LADO DER
            (elem e . concat) []
        =                   (def. (.))
            (elem e (concat []))
        =                   (def. concat.1)
            elem e []
        =                   (def. elem.1)
            False



    ||| CASO INDUCTIVO l = (zs:zss) ||| any (elem e) (zs:zss) = (elem e . concat) (zs:zss)
        HI) any (elem e) zss = (elem e . concat) zss
        TI) any (elem e) (zs:zss) = (elem e . concat) (zs:zss)

    LADO IZQ
            any (elem e) (zs:zss)
        =                   (def. any)
            elem e zs || any (elem e) zss
        =                   (HI)
            elem e zs || (elem e . concat) zss
        =                   (def. (.))
            elem e zs || elem e (concat zss)


    LADO DER 
            (elem e . concat) (zs:zss)
        =                   (def. (.))
            elem e (concat (zs:zss))
        =                   (def. concat)
            elem e (zs ++ concat zss)
        =                   (LEMA. ∀x∀xs∀ys. elem x (xs ++ ys) = elem x xs || elem x ys)
            elem e zs || elem e (concat zss)
        
    =================================================================================
    LEMA: ∀x∀xs∀ys. elem x (xs ++ ys) = elem x xs || elem x ys)
    =================================================================================
    Sea k un elemento arbitrario y sean ws, zs listas del mismo tipo que k.
    Por ppio. de inducción sobre ws.
        elem k (ws ++ zs) = elem k ws || elem k zs
    
    ||| CASO BASE ws = [] |||  

            elem k ([] ++ zs) = elem k [] || elem k zs
    
    LADO IZQ 
            elem k ([] ++ zs) 
        =                       (def. ++)
            elem k zs

    LADO DER 
            elem [] ws || elem k zs
        =                       (def. elem.1)
            False || elem k zs
        =                       (Ley lógica de identidad)
            elem k zs

    
    ||| CASO INDUCTIVO ws = (w:ws') |||
        HI) elem k (ws' ++ zs) = elem k ws' || elem k zs
        TI) elem k ((w:ws') ++ zs) = elem k (w:ws') || elem k zs

    LADO IZQ 
            elem k ((w:ws') ++ zs) 
        =                       (def. ++.2)
            elem k (w: ws' ++ zs)
        =                       (def. elem.2)
            k == w || elem k (ws' ++ zs)
        

    LADO DER
            elem k (w:ws') || elem k zs
        =                       (def. elem.2)
            k == w || elem k ws' || elem k zs
        =                       (HI)
            k == w || elem k (ws' ++ zs)


        
f)
        ∀xs.∀ys. subset xs ys = all (flip elem ys) xs
    Sea ws y zs listas arbitrarias del mismo tipo.
    Por ppio. de inducción sobre la estructura de ws.
    
    ||| CASO BASE ws = [] |||

        subset [] zs = all (flip elem zs) []
    
    LADO IZQ 
            subset [] zs
        =                       (def. subset.1)
            True

    LADO DER
            all (flip elem zs) []
        =                       (def. all.1)
            True


    ||| CASO INDUCTIVO ws = (w:ws') |||
        HI) subset ws' zs = all (flip elem zs) ws'
        TI) subset (w:ws') zs = all (flip elem zs) (w:ws')

    LADO IZQ
            subset (w:ws') zs
        =                       (def. subset.2)
            any (w==) zs && subset ws' zs
        =                       (HI)
            any (w==) zs && all (flip elem zs) ws'
        
    LADO DER
            all (flip elem zs) (w:ws')
        =                       (def. all.2)
            flip elem zs w && all (flip elem zs) ws'
        =                       (def. flip)
            elem w zs && all (flip elem zs) ws'
        =                       (LEMA2. ∀x.∀xs. elem x xs = any (x==) xs)
            any (w==) zs && all (flip elem zs) ws'

    =================================================================================
    LEMA2:  ∀x.∀xs. elem x xs = any (x==) xs
    =================================================================================
        Sea e un elemento arbitrario. Sea ys una lista arbitraria del mismo tipo que e.
        Por ppio. de inducción sobre la estructura de ys.
        
        ||| CASO BASE ys = [] |||

                elem e [] = any (e==) []

        LADO IZQ 
                elem e []
            =                   (def. elem.1)
                False
        

        LADO DER 
                any (e==) []
            =                   (def. any.1)
                False


        ||| CASO INDUCTIVO ys = (y:ys') |||
            HI) elem e ys' = any (e==) ys'
            TI) elem e (y:ys') = any (e==) (y:ys')    

        LADO IZQ 
                elem e (y:ys')
            =                   (def. elem.2)
                e == y || elem e ys'
            =                   (HI)
                e == y || any (e==) ys'

        LADO DER 
                any (e==) (y:ys')
            =                   (def. any.2)
                (e==) y || any (e==) ys'
            =                   (equivalencia section)
                e == y || any (e==) ys'


g)
        all null = null . concat
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀xss. all null xss = (null . concat) xss
    Sea yss una lista de listas arbitraria. Por ppio. de ind. sobre yss.

    ||| CASO BASE yss = [] |||
        
            all null [] = (null . concat) []


    LADO IZQ 
            all null []
        =                       (def. all.1)
            True
    
    LADO DER 
            (null . concat) []
        =                       (def. (.))
            null (concat [])
        =                       (def. concat.1)
            null []
        =                       (def. null.1)
            True


    
    ||| CASO INDUCTIVO yss = (ys:yss') |||
        HI) all null yss' = (null . concat) yss'
        TI) all null (ys:yss') = (null . concat) (ys:yss')


    LADO IZQ
            all null (ys:yss')
        =                       (def. all.2)
            null ys && all null yss'
        =                       (HI)
            null ys && (null . concat) yss'
    
    
    LADO DER
            (null . concat) (ys:yss')
        =                       (def. (.))
            null (concat (ys:yss'))
        =                       (def. concat.2)
            null (ys ++ concat yss')
        =                       (LEMA3. ∀xs.∀ys. null xs && null ys = null (xs ++ ys))
            null ys && null (concat yss')
        =                       (def. (.))
            null ys && (null . concat) yss'


    =================================================================================
    LEMA3: ∀xs.∀ys. null xs && null ys = null (xs ++ ys)
    =================================================================================
        Sea ws y zs listas arbitrarias. Por ppio. de induccion sobre ws.

        ||| CASO BASE ws = [] ||| 

            null [] && null ys = null ([] ++ zs)

        LADO IZQ 
                null [] && null zs
            =                   (def. null.1)
                True && null zs
            =                   (Ley identidad de &&)
                null zs

        LADO DER 
                null ([] ++ zs)
            =                   (def. ++.1)
                null zs


        ||| CASO INDUCTIVO ws = (w:ws') |||
            HI) null ws' && null zs = null (ws' ++ zs)
            TI) null (w:ws') && null zs = null ((w:ws') ++ zs)
        
        LADO IZQ
                null (w:ws') && null zs
            =                   (def. null.2)
                False && null zs
            =                   (def. &&)
                False
        
        LADO DER
                null ((w:ws') ++ zs)
            =                   (def. ++.2)
                null (w: ws' ++ zs)
            =                   (def. null.2)
                False
    


h)
        length = length . reverse
    Por ppio. de extensionalidad, es equivalente a demostrar que:
        ∀xs. length xs = (length . reverse) xs
    Sea ys una lista arbitraria. Por ppio. de inducción sobre la estructura de ys.

    ||| CASO BASE ys = [] |||
        length [] = (length . reverse) []

    LADO IZQ 
            length []

    LADO DER
            (length . reverse) []
        =                       (def. (.))
            length (reverse [])
        =                       (def. reverse.1)
            length []

    
    ||| CASO INDUCTIVO ys = (y: ys') |||
        HI) length ys' = (length . reverse) ys'
        TI) length (y:ys') = (length . reverse) (y:ys')

    LADO IZQ 
            length (y:ys')
        =                       (def. length.2)
            1 + length ys'
        =                       (HI)
            1 + (length . reverse) ys'
        =                       (def. (.))
            1 + length (reverse ys')
    
    
    LADO DER
            (length . reverse) (y:ys')
        =                       (def. (.))
            length (reverse (y:ys'))
        =                       (def. reverse.2)
            length (reverse ys' ++ [y])
        =                       (prop. del punto a)
            length (reverse ys') + length [y]
        =                       (def. length.2)
            length (reverse ys') + 1 + length []
        =                       (def. length.1)
            length (reverse ys') + 1 + 0
        =                       (0 neutro y conmutatividad suma)
            1 + length (reverse ys')




i)
        ∀xs.∀ys. reverse (xs ++ ys) = reverse ys ++ reverse xs
    Sean ws y zs listas arbitrarias. Por ppio. de ind. sobre la estructura de ws.
    
    ||| CASO BASE ws = [] |||
        reverse ([] ++ zs) = reverse zs ++ reverse []
    

    LADO IZQ
            reverse ([] ++ zs)
        =                       (def. ++.1)
            reverse zs


    LADO DER 
            reverse zs ++ reverse []
        =                       (def. reverse.1)
           reverse zs ++ []
        =                       (neutro ++)
            reverse zs

    
    ||| CASO INDUCTIVO ws = (w:ws') |||
        HI) reverse (ws' ++ zs) = reverse zs ++ reverse ws'
        TI) reverse ((w:ws') ++ zs) = reverse zs ++ reverse (w:ws')

    LADO IZQ
            reverse ((w:ws') ++ zs)
        =                       (def. ++.2)
            reverse (w : ws' ++ zs)
        =                       (def. reverse.2)
            reverse (ws' ++ zs) ++ [w]
  


    LADO DER
            reverse zs ++ reverse (w:ws')
        =                       (def. reverse.2)
            reverse zs ++ reverse ws' ++ [w]
        =                       (HI)
            reverse (ws' ++ zs) ++ [w]


j)
        ∀xs.∀ys. all p (xs++ys) = all p (reverse xs) && all p (reverse ys)
    Sean ws y zs listas arbitrarias. Por ppio. de inducción sobre la estructura de ws.

    ||| CASO ws = [] |||
            all p ([] ++ zs) = all p (reverse []) && all p (reverse zs)

    LADO IZQ 
            all p ([] ++ zs)
        =                       (def. ++.1)
            all p zs
        =                       (LEMA4: ∀p.∀xs. all p xs = all p (reverse xs))
            all p (reverse zs)
    
    LADO DER 
            all p (reverse []) && all p (reverse zs)
        =                       (def. reverse.1)
            all p [] && all p (reverse zs)
        =                       (def. all.1)
            True && all p (reverse zs)
        =                       (Ley identidad de &&)
            all p (reverse zs)


    ||| CASO INDUCTIVO ws = (w:ws') |||
        HI) all p (ws' ++ zs) = all p (reverse ws') && all p (reverse zs)
        TI) all p ((w:ws') ++ zs) = all p (reverse (w:ws')) && all p (reverse zs)

    
    LADO IZQ
            all p ((w:ws') ++ zs)
        =                       (def. ++.2)
            all p (w : ws' ++ zs)   
        =                       (def. all.2)
            p w && all p (ws' ++ zs)
        =                       (HI)
            p w && all p (reverse ws') && all p (reverse zs)

    
    LADO DER
            all p (reverse (w:ws')) && all p (reverse zs)
        =                       (def. reverse.2)
            all p (reverse ws' ++ [w]) && all p (reverse zs)
        =                       (LEMA5: ∀p.∀xs.∀ys. all p (xs ++ ys) = all p xs && all p ys))
            all p (revere ws') && all p [w] && all p (reverse zs)
        =                       (def. all.2)
            all p (reverse ws') && p w && all p [] && all p (reverse zs)
        =                       (def. all.1)
            all p (reverse ws') && p w && True && all p (reverse zs)
        =                       (Ley Identidad de &&)
            all p (reverse ws') && p w && all p (reverse zs)
        =                       (Conmutatividad de &&)
            p w && all p (reverse ws') && all p (reverse zs)

         
    =================================================================================
    LEMA4:  ∀p.∀xs. all p xs = all p (reverse xs)
    =================================================================================
        Sea p' un funcion de a->Bool arbitraria. Sea ys una lista de tipo [a] arbitraria.
        Por ppio. de ind. sobre la estructura de ys.

    ||| CASO BASE ys = [] |||
            all p' [] = all p' (reverse [])
        
    LADO IZQ 
            all p' []

    LADO DER
            all p' (reverse [])
        =                       (def. reverse.1)
            all p' []

    

    ||| CASO INDUCTIVO ys = (y:ys') |||
        HI) all p' ys' = all p' (reverse ys')
        TI) all p' (y:ys') = all p' (reverse (y:ys'))

    LADO IZQ 
            all p' (y:ys')
        =                       (def. all.2)
            p' y && all p' ys'
        =                       (HI)
            p' y && all p' (reverse ys')
    

    LADO DER
            all p' (reverse (y:ys'))
        =                       (def. reverse.2)
            all p' (reverse ys' ++ [y])
        =                       (LEMA5: ∀p.∀xs.∀ys. all p (xs ++ ys) = all p xs && all p ys)
            all p' (reverse ys') && all p' [y]
        =                       (def. all.2)
            all p' (reverse ys') && p' y && all p' []
        =                       (def. all.1)
            all p' (reverse ys') && p' y && True
        =                       (Ley Identidad de &&)
            all p' (reverse ys') && p' y
        =                       (conmutatividad &&)
            p' y && all p' (reverse ys')




    =================================================================================
    LEMA5: ∀p.∀xs.∀ys. all p (xs ++ ys) = all p xs && all p ys)
    =================================================================================
        Sea p' una función de a->Bool arbitraria. Sean ws y zs listas arbitrarias :: [a].
        Por ppio. de inducción sobre la estructura de ws

    ||| CASO BASE ws = [] |||
            all p' ([] ++ zs) = all p' [] && all p' zs
    
    LADO IZQ
            all p' ([] ++ zs)
        =                       (def. ++.1)
            all p' zs
        
    LADO DER
            all p' [] && all p' zs
        =                       (def. all.1)
            True && all p' zs
        =                       (Ley Identidad de &&)
            all p' zs
    

    ||| CASO INDUCTIVO ws = (w:ws') |||
        HI) all p' (ws' ++ zs) = all p' ws' && all p' zs
        TI) all p' ((w:ws') ++ zs) = all p' (w:ws') && all p' zs

    LADO IZQ
            all p' ((w:ws') ++ zs)
        =                       (def. ++.2)
            all p' (w : ws' ++ zs)
        =                       (def. all.2)
            p' w && all p' (ws' ++ zs)

    LADO DER
            all p' (w:ws') && all p' zs
        =                       (def. all.2)
            p' w && all p' ws' && all p' zs
        =                       (HI)
            p' w && all p' (ws' ++ zs)





k)
            ∀xs.∀ys. unzip (zip xs ys) = (xs, ys)
    Esta propiedad es falsa ya que con xs = [10, 20], ys = [30] la reducción es:

            unzip (zip [10,20] [30])
        =                       (def. zip.2)
            unzip (case [30] of
                    (z:zs) -> (10,z) : zip [20] zs
                    _      -> [])
        =                       (1° ecuación de case, con z <- 30, zs <- [])
            unzip ((10,30) : zip [20] [])
        =                       (def. zip.2)
            unzip ((10,30) : case [] of
                              (z:zs) -> (20,z) : zip [] zs
                              _      -> [])
        =                       (2° ecuación de case)
            unzip ((10,30) : [])
        =                       (def. unzip.2)
            let (10,30)  = (10,30)
                (xs, ys) = unzip []
                in (10:xs, 30:ys)
        =                       (def.unzip.1)
            let (10,30)    = xy
                ([], []) = unzip []
                in (10:[], 30:[])
        =                       (in del let)
                ([10], [30])
    
    La reducción devuelve una tupla cuyo primer elemento es distinto de xs.

-}


-- ================================================================================= --
--                                    SECCIÓN 2                                      --
-- ================================================================================= --

data N = Z | S N 

-- EJERCICIO 1
--a)
evalN :: N -> Int 
evalN Z     = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N 
addN Z     k = k
addN (S n) k = S (addN n k) 

prodN :: N -> N -> N 
prodN Z     k = Z
prodN (S n) k = addN k (prodN n k)

int2N :: Int -> N 
int2N 0 = Z
int2N n = S (int2N (n-1))

--b)
{-
    i) 
        ∀n1.∀n2. evalN (addN n1 n2) = evalN n1 + evalN n2    
        // IMPORTANTE: lo q dice esta propiedad es que si N denota los enteros
                       addN DENOTA la SUMA 
    Sean n y k elementos de N arbitrarios. Por ppio. de ind. sobre la estructura de n.

    ||| CASO BASE n = Z ||| 
        evalN (addN Z k) = evalN Z + evalN k

    LADO IZQ
            evalN (addN Z k)
        =                       (def. addN.1)
            evalN k

    LADO DER
            evalN Z + evalN k
        =                       (def. evalN.1)
            0 + evalN k
        =                       (0 neutro de la suma)
            evalN k

    
    ||| CASO INDUCTIVO n = (S n') ||| 
        HI) evalN (addN n' k) = evalN n' + evalN k
        TI) evalN (addN (S n') k) = evalN (S n') + evalN k 


    LADO IZQ
            evalN (addN (S n') k)
        =                       (def. addN.2)
            evalN (S (addN n' k))
        =                       (def. evalN.2)
            1 + evalN (addN n' k)


    LADO DER
            evalN (S n') + evalN k
        =                       (def. evalN.2)
            1 + evalN n' + evalN k
        =                       (HI)
            1 + evalN (addN n' k)    


    

    ii)
        ∀n1.∀n2. evalN (prodN n1 n2) = evalN n1 * evalN n2
        Sean n y k elementos arbitrarios de N. Por ppio. de ind. sobre la estructura de n.


    ||| CASO BASE n = Z |||
        evalN (prodN Z k) = evalN Z * evalN k

    LADO IZQ 
            evalN (prodN Z k)
        =                       (def. prodN.1)
            evalN Z
        =                       (def. evalN)
            0

    LADO DER
            evalN Z * evalN k
        =                       (def. evalN Z)
            0 * evalN k
        =                       (prop. mult. de 0)
            0

    
    ||| CASO INDUCTIVO n = (S n') |||
        HI) evalN (prodN n' k) = evalN n' * evalN k
        TI) evalN (prodN (S n') k) = evalN (S n') * evalN k


    LADO IZQ 
            evalN (prodN (S n') k)
        =                       (def. prodN.2)
            evalN (addN k (prodN n' k))
        =                       (Prop. inciso (i))
            evalN k + evalN (prodN n' k)
        =                       (HI)
            evalN k + evalN n' * evalN k



    LADO DER 
            evalN (S n') * evalN k
        =                       (def. evalN)
            (1 + evalN n') * evalN k
        =                       (distributiva mult.)
            evalN k + evalN n' * evalN k
     




    iii)
        int2N . evalN = id
        Por ppio. de extensionalidad es equivalente a demostrar que:
        ∀n. (int2N . evalN) n =  id n
        Sea x un elemento arbitrario de N. Por ppio. de ind. sobre la estructura de x.

    ||| CASO BASE x = Z |||
            (int2N . evalN) Z = id Z
        

    LADO IZQ 
            (int2N . evalN) Z
        =                       (def. (.))
            int2N (evalN Z)
        =                       (def. evalN.1)
            int2N 0
        =                       (def. int2N)
            Z

    LADO DER
            id Z
        =                       (def. id)
            Z


    iv)
        evalN . int2N = id
        Por ppio. de extensionalidad es equivalente a demostrar que:
        ∀n. (evalN . int2N) n = id n
        Sea x un entero mayor o igual a 0 cualquiera. Por ppio. de ind matemática.

    ||| CASO BASE x = 0 |||
            (evalN . int2N) 0 = id 0
        
    LADO IZQ 
            (evalN . int2N) 0
        =                       (def. (.))
            evalN (int2N 0)
        =                       (def. int2N.1)
            evalN Z
        =                       (def. evalN.1)
            0

    LADO DER
            id 0
        =                       (def. id)
            0


    ||| CASO INDUCTIVO x = (x'+ 1) |||
        HI) (eval . int2N) x' = id x' 
        TI) (eval . int2N) (x' + 1) = id (x + 1)
    

    LADO IZQ 
            (eval . int2N) (x' + 1)
        =                       (def. (.))
            evalN (int2N (x' + 1))
        =                       (LEMA 2.1: ∀n1.∀n2. int2N (n1 + n2) = addN (int2N n1) (int2N n2))
            evalN (addN (int2N x') (int2N 1))
        =                       (Prop. del inciso (i) )
            evalN (int2N x') + evalN (int2N 1)
        =                       (Prop. del inciso (iii) )
            x' + 1
        

    LADO DER   
            id (x' + 1)
        =                       (def. id)
            x' + 1


    =================================================================================
    LEMA 2.1: ∀n1.∀n2. int2N (n1 + n2) = addN (int2N n1) (int2N n2)
    =================================================================================
    Sean n y k enteros >= 0. Por ppio. de ind. matemática de n.
    
    ||| CASO BASE n = 0 |||
        int2N (0 + k) = addN (int2N 0) (int2N k)

    LADO IZQ
            int2N (0 + k)
        =                       (neutro de la suma)
            int2N k
    

    LADO DER
            addN (int2N 0) (int2N k)
        =                       (def. int2N.1)
            addN Z (int2N k)
        =                       (def. addN.1)
            int2N k


    
    ||| CASO INDUCTIVO n = (n' + 1) |||
        HI) int2N (n' + k) = addN (int2N n') (int2N k)
        TI) int2N ((n' + 1) + k) = addN (int2N (n' + 1)) (int2N k)

    LADO IZQ 
            int2N ((n' + 1) + k)
        =                       (conmutatividad y asociatividad de la suma)
            int2N ((n' + k) + 1)
        =                       (def. int2N.2)
            S (int2N ((n' + k) + 1 - 1))
        =                       (aritmética)
            S (int2N (n' + k))

    
    LADO DER 
            addN (int2N (n' + 1)) (int2N k)
        =                       (def. int2N.2)
            addN (S (int2N (n' + 1 - 1))) (int2N k)
        =                       (aritmética)
            addN (S (int2N n')) (int2N k)
        =                       (def. addN.2)
            S (addN (int2N n') (int2N k))
        =                       (HI)
            S (int2N (n' + k))
-}





-- EJERCICIO 2
type NU = [()]

--a)
evalNU :: NU -> Int 
evalNU = length

succNU :: NU -> NU 
succNU = (():)

addNU :: NU -> NU -> NU
addNU = (++)

nu2n :: NU -> N 
nu2n []     = Z
nu2n (u:us) = S (nu2n us)

n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : n2nu n

{-
b)

    i) 
            evalNU . succNU = (+1) . evalNU
    Por ppio. de extensionalidad, es equivalente a demostrar que: 
            ∀x. (evalNU . succNU) x = ((+1) . evalNU) x
    Sea n un elemento arbitrario de NU. Por ppio. de ind. sobre la estructura de n.

    ||| CASO BASE n = [] |||
        
            (evalNU . succNU) [] = ((+1) . evalNU) []

    LADO IZQ 
            (evalNU . succNU) []
        =                       (def. (.))
            evalNU (succNU [])
        =                       (def. succNU.1)
            evalNU [()]
        =                       (def. evalNU)
            length [()]
        =                       (def. length.2)
            1 + length []
        =                       (def. length.1)
            1 + 0
    
    LADO DER
            ((+1) . evalNU) []
        =                       (def. (.))
            (+1) (evalNU [])
        =                       (def. evalNU)
            (+1) (length [])
        =                       (def. length.1)
            (+1) 0
        =                       (def. (+), donde y <- 1)
            0 + 1
        =                       (conmutatividad de la suma)
            1 + 0

    
    ||| CASO INDUCTIVO n = (u:us) |||
        HI) (evalNU . succNU) us = ((+1) . evalNU) us
        TI) (evalNU . succNU) (u:us) = ((+1) . evalNU) (u:us)

    LADO IZQ 
            (evalNU . succNU) (u:us)
        =                       (def. (.))
            evalNU (succNU (u:us))
        =                       (def. succNU)
            evalNU (():(u:us))
        =                       (def. evalNU)
            length (():(u:us))
        =                       (def. length.2)
            1 + length (u:us)
        

    LADO DER
            ((+1) . evalNU) (u:us)
        =                       (def. (.))
            (+1) (evalNU (u:us))
        =                       (def. evalNU)
            (+1) (length (u:us))
        =                       (def. (+), donde y <- 1)
            length (u:us) + 1
        =                       (conmutatividad de la suma)
            1 + length (u:us)

    

    ii)
            ∀n1. ∀n2. evalNU (addNU n1 n2) = evalNU n1 + evalNU n2
        Sean n y k elementos arbitrarios de NU. Por ppio. de inducción estructural sobre n.

    ||| CASO BASE n = [] |||
        
            evalNU (addNU [] k) = evalNU [] + evalNU k
    
    LADO IZQ 
            evalNU (addNU [] k)
        =                       (def. addNU)
            evalNU ((++) [] k)
        =                       (def. (++).1)
            evalNU k


    LADO DER
            evalNU [] + evalNU k
        =                       (def. evalNU)
            length [] + evalNU k
        =                       (def. length.1)
            0 + evalNU k
        =                       (neutro de la suma)
            evalNU k


    ||| CASO INDUCTIVO  n = (x:xs) |||
        HI) evalNU (addNU ns k) = evalNU ns + evalNU k
        TI) evalNU (addNU (n:ns) k) = evalNU (n:ns) + evalNU k  

    LADO IZQ
            evalNU (addNU (n:ns) k)
        =                       (def. addNU)
            evalNU ((++) (n:ns) k)
        =                       (def. (++).2)
            evalNU (n: (++) ns k)
        =                       (def.evalNU)
            length (n: (++) ns k)
        =                       (def. length.2)
            1 + length ((++) ns k)
        =                       (def. addNU)
            1 + length (addNU ns k)
        =                       (def. evalNU)
            1 + evalNU (addNU ns k)
        
    LADO DER
            evalNU (n:ns) + evalNU k
        =                       (def. evalNU)
            length (n:ns) + evalNU k
        =                       (def. length.2)
            1 + length ns + evalNU k
        =                       (def. evalNU)
            1 + evalNU ns + evalNU k
        =                       (HI)
            1 +  evalNU (addNU ns k)
    


    iii)
            nu2n . n2nu = id
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀n. (nu2n . n2nu) n = id n
        Sea k un elemento arbitrario de N. Por ppio. de inducción sobre la estructura de k.

    ||| CASO BASE k = Z |||
        
        (nu2n . n2nu) Z = id Z



    LADO IZQ 
            (nu2n . n2nu) Z
        =                       (def. (.))
            nu2n (n2nu Z)
        =                       (def. n2nu.1)
            nu2n []
        =                       (def. nu2n.1)
            Z


    LADO DER    
            id Z
        =                       (def. id)
            Z




    ||| CASO INDUCTIVO k = (S k') |||
        HI) (nu2n . n2nu) k' = id k'
        TI) (nu2n . n2nu) (S k') = id (S k')



    LADO IZQ
            (nu2n . n2nu) (S k')
        =                       (def. (.))
            nu2n (n2nu (S k'))
        =                       (def. n2nu.2)
            nu2n ((): n2nu k' )
        =                       (def. nu2n.2)
            S (nu2n (n2nu k'))
        =                       (def. (.))
            S ((nu2n . n2nu) k')
        =                       (HI)
            S (id k')
        =                       (def. id)
            S k'



    LADO DER
            id (S k')
        =                       (def. id)
            S k' 


    iv)
            n2nu . nu2n = id
        Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀n. (n2nu . nu2n) n = id n
        Sea k un elementro arbitrario de NU. Por ppio. de ind. sobre la estructura de k.

    ||| CASO BASE k = [] |||
        
            (n2nu . nu2n) [] = id []


    LADO IZQ 
            (n2nu . nu2n) []
        =                       (def. (.))
            n2nu (nu2n [])
        =                       (def. nu2n.1)
            n2nu Z
        =                       (def. n2nu.1)
            []

    LADO DER
            id []
        =                       (def. id)
            []   
        




    ||| CASO INDUCTIVO k = (x:xs) |||
        HI) (n2nu . nu2n) xs = id xs
        TI) (n2nu . nu2n) (x:xs) = id (x:xs)

    LADO IZQ
            (n2nu . nu2n) (x:xs)
        =                       (def. (.))
            n2nu (nu2n (x:xs))
        =                       (def. nu2n.2)
            n2nu (S (nu2n xs))
        =                       (def. n2nu.2)
            () : (n2nu (nu2n xs))
        =                       (def. (.))
            () : ((n2nu . nu2n) xs)
        =                       (HI)
            () : (id xs)
        =                       (def. id)
            () : xs
        
    LADO DER
            id (x:xs)
        =                       (def. id)
            x:xs
        =                       (los elementos de NU estan representados por una lista de unit, por lo que x = ())
            () : xs




-}

-- EJERCICIO 3
data DigBin = O | I
type NBin = [DigBin] --el digito menos significativo a la izquierda

dbAsInt :: DigBin -> Int 
dbAsInt O = 0
dbAsInt I = 1

--a)






-- ================================================================================= --
--                                    SECCIÓN 3                                      --
-- ================================================================================= --

-- EJERCICIO 1
data ExpA = Cte Int 
          | Suma ExpA ExpA
          | Prod ExpA ExpA

--a)
evalExpA :: ExpA -> Int 
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

simplificarExpA :: ExpA -> ExpA 
simplificarExpA (Cte n)      = Cte n
simplificarExpA (Suma e1 e2) = sumaSimplificada (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) = prodSimplificada (simplificarExpA e1) (simplificarExpA e2)

prodSimplificada :: ExpA -> ExpA -> ExpA
prodSimplificada (Cte 1) n       = n
prodSimplificada (Cte 0) n       = Cte 0
prodSimplificada m       (Cte 1) = m
prodSimplificada m       (Cte 0) = Cte 0
prodSimplificada m       n       = Prod m  n

sumaSimplificada :: ExpA -> ExpA -> ExpA
sumaSimplificada (Cte 0) n       = n 
sumaSimplificada m       (Cte 0) = m
sumaSimplificada m       n       = Suma m n


cantidadDeSumaCero :: ExpA -> Int 
cantidadDeSumaCero (Cte _)      = 0
cantidadDeSumaCero (Suma e1 e2) = fromEnum (esSumaCero e1 e2) + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

esSumaCero :: ExpA -> ExpA -> Bool
esSumaCero (Cte 0) _ = True
esSumaCero _ (Cte 0) = True 
esSumaCero _  _      = False 


{-
b)
    i)
            evalExpA . simplificarExpA = evalExpA
    Por ppio. de extensionalidad, es equivalente a demostrar que:
            ∀x. (evalExpA . simplificarExpA) x = evalExpA x
    Sea e un elemento arbitrario de ExpA. Por ppio. de ind. sobre la estructura de e.

    ||| CASO BASE e = (Cte n), donde n es un entero arbitrario |||
        
        (evalExpA . simplificarExpA) (Cte n) = evalExpA (Cte n)

    LADO IZQ 
            (evalExpA . simplificarExpA) (Cte n)
        =                       (def. (.))
            evalExpA (simplificarExpA (Cte n))
        =                       (def. simplificarExpA.1)
            evalExpA (Cte n)
    
    LADO DER
            evalExpA (Cte n)

    ||| CASO INDUCTIVO 1, e = (Suma e1 e2) |||
        HI1) (evalExpA . simplificarExpA) e1 = evalExpA e1
        HI2) (evalExpA . simplificarExpA) e2 = evalExpA e2
        TI)  (evalExpA . simplificarExpA) (Suma e1 e2) = evalExpA (Suma e1 e2)


    LADO IZQ 
            (evalExpA . simplificarExpA) (Suma e1 e2)
        =                       (def. (.))
            evalExpA (simplificarExpA (Suma e1 e2))
        =                       (def. simplificarExpA.2)
            evalExpA (sumaSimplificada (simplificarExpA e1) (simplificarExpA e2))

            || SUBCASO (simplificarExpA e1) = Cte 0 ||
                evalExpA (sumaSimplificada (Cte 0) (simplificarExpA e2))
            =                   (def. sumaSimplificada.1)
                evalExpA (simplificarExpA e2)
            =                   (def. (.))
                (evalExpA . simplificarExpA) e2
            =                   (HI2)
                evalExpA e2
            =                   (neutro de la suma)
                0 + evalExpA e2
            =                   (def. evalExpA.1)
                evalExpA (Cte 0) + evalExpA e2
            =                   (def. evalExpA.2)
                evalExpA (Suma (Cte 0) e2)

           || SUBCASO (simplificarExpA e2) = Cte 0 ||
                evalExpA (sumaSimplificada (simplificarExpA e1) (Cte 0))
            =                   (def. sumaSimplificada.2)
                evalExpA (simplificarExpA e1)
            =                   (def. (.))
                (evalExpA . simplificarExpA) e1
            =                   (HI1)
                evalExpA e1
            =                   (neutro de la suma)
                evalExpA e1 + 0
            =                   (def. evalExpA.1)
                evalExpA e1 + evalExpA (Cte 0)
            =                   (def. evalExpA.2)
                evalExpA (Suma e1 (Cte 0))
        
           || SUBCASO (simplificarExpA e1) != Cte 0 y (simplificarExpA e2) != Cte 0 ||
                evalExpA (sumaSimplificada (simplificarExpA e1) (simplificarExpA e2))
            =                   (def. sumaSimplificada.3)
                evalExpA (Suma (simplificarExpA e1) (simplificarExpA e2))
            =                   (def. evalExpA.2)
                evalExpA (simplificarExpA e1) + evalExpA (simplificarExpA e2)
            =                   (def. (.))
                (evalExpA . simplificarExpA) e1 + (evalExpA . simplificarExpA) e2
            =                   (HI1)
                evalExpA e1 + (evalExpA . simplificarExpA) e2
            =                   (HI2)
                evalExpA e1 + evalExpA e2
            =                   (def. evalExpA)
                evalExpA (Suma e1 e2)
    
    ||| CASO INDUCTIVO 2 e = (Prod e1 e2) |||
        HI1) (evalExpA . simplificarExpA) e1 = evalExpA e1
        HI2) (evalExpA . simplificarExpA) e2 = evalExpA e2
        TI)  (evalExpA . simplificarExpA) (Prod e1 e2) = evalExpA (Prod e1 e2)

    DEM.:  
            (evalExpA . simplificarExpA) (Prod e1 e2)
        =                       (def. (.))
            evalExpA (simplificarExpA (Prod e1 e2))
        =                       (def. simplificarExpA.3)
            evalExpA (prodSimplificada (simplificarExpA e1) (simplificarExpA e2))


            || SUBCASO (simplificarExpA e1) = (Cte 1) ||
                evalExpA (prodSimplificada (Cte 1) (simplificarExpA e2))
            =                   (def. prodSimplificada.1)
                evalExpA (simplificarExpA e2)
            =                   (def. (.))
                (evalExpA . simplificarExpA) e2
            =                   (HI2)
                evalExpA e2
            =                   (neutro mult.)
                1 * evalExpA e2
            =                   (def. evalExpA.1)
                evalExpA (Cte 1) * evalExpA e2
            =                   (def. evalExpA.3) 
                evalExpA (Prod (Cte 1) e2) 


            || SUBCASO (simplificarExpA e1) = (Cte 0) || 
                evalExpA (prodSimplificada (Cte 0) (simplificarExpA e2))
            =                   (def. prodSimplificada.2)
                evalExpA (Cte 0)
            =                   (def. evalExpA.1)
                0
            =                   (prop. mult. del 0)
                0 * evalExpA e2
            =                   (def. evalExpA.1)
                evalExpA (Cte 0) * evalExpA e2
            =                   (def. evalExpA.3)
                evalExpA (Prod (Cte 0) e2)
            

            || SUBCASO (simplificarExpA e2) = (Cte 1) ||
                evalExpA (prodSimplificada (simplificarExpA e1) (Cte 1))
            =                   (def. prodSimplificada.3)
                evalExpA (simplificarExpA e1)
            =                   (def. (.))
                (evalExpA . simplificarExpA) e1
            =                   (HI1)
                evalExpA e1
            =                   (neutro mult.)
                evalExpA e1 * 1
            =                   (def. evalExpA.1)
                evalExpA e1 * evalExpA (Cte 1)
            =                   (def. evalExpA.3) 
                evalExpA (Prod e1 (Cte 1)) 


            || SUBCASO (simplificarExpA e2) = (Cte 0) || 
                evalExpA (prodSimplificada (simplificarExpA e1) (Cte 0))
            =                   (def. prodSimplificada.4)
                evalExpA (Cte 0)
            =                   (def. evalExpA.1)
                0
            =                   (prop. mult. del 0)
                evalExpA e1 * 0
            =                   (def. evalExpA.1)
                evalExpA e1 * evalExpA (Cte 0)
            =                   (def. evalExpA.3)
                evalExpA (Prod e1 (Cte 0))



            || SUBCASO (simplificarExpA e1) y (simplificarExpA e2) no pertenecen a {(Cte 0), (Cte 1)} ||
                evalExpA (prodSimplificada (simplificarExpA e1) (simplificarExpA e2) )
            =                   (def. prodSimplificada.5)
                evalExpA (Prod (simplificarExpA e1) (simplificarExpA e2))
            =                   (def. evalExpA.3)
                evalExpA (simplificarExpA e1) * evalExpA (simplificarExpA e2)
            =                   (def. (.))
                (evalExpA . simplificarExpA) e1 * (evalExpA . simplificarExpA) e2
            =                   (HI1)
                evalExpA e1 * (evalExpA . simplificarExpA) e2
            =                   (HI2)
                evalExpA e1 * evalExpA e2
            =                   (def. evalExpA.3)
                evalExpA (Prod e1 e2) 



-}