import Prelude hiding (Maybe, Just)
-- EJERCICIO 1

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon

data Helado = Vasito Gusto
            | Cucurucho Gusto Gusto 
            | Pote Gusto Gusto Gusto 

chocoHelate :: (Gusto -> a) -> a
chocoHelate consH = consH Chocolate

{-
a)
    Vasito :: Gusto -> Helado

b)
    Chocolate :: Gusto 

c)
    Cucurucho :: Gusto -> Gusto -> Helado 

d)
    Sambayon :: Gusto 

e)
    Pote :: Gusto -> Gusto -> Gusto -> Helado

f)
    chocoHelate :: (Gusto -> a) -> a

g)
    chocoHelate Vasito :: Helado

h)
    chocoHelate Cucurucho :: Gusto -> Helado

i)
    chocoHelate (Cucurucho Sambayon) :: Helado

j)
    chocoHelate (chocoHelate Cucurucho) :: Helado

k)
    chocoHelate (Vasito DulceDeLeche) :/:

l)
    chocoHelate Pote :: Gusto -> Gusto -> Helado

m) 
    chocoHelate (chocoHelate (Pote Frutilla)) :: Helado
-}


-- EJERCICIO 2

data DigBin = O | I

--a)
dbAsInt :: DigBin -> Int 
dbAsInt O = 0
dbAsInt I = 1

--b)
dbAsBool :: DigBin -> Bool 
dbAsBool O = False
dbAsBool I = True 

--c)
dbOfBool :: Bool -> DigBin
dbOfBool False = O
dbOfBool True  = I

--d)
negDB :: DigBin -> DigBin
negDB O = I
negDB I = O 


-- EJERCICIO 3
data DigDec = D0 | D1 | D2 | D3 | D4
            | D5 | D6 | D7 | D8 | D9

--a)
ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

--b)
ddOfInt :: Int -> DigDec 
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

--c)
nextDD :: DigDec -> DigDec 
nextDD dd = ddOfInt ((ddAsInt dd + 1) `mod` 10)

--d)
prevDD :: DigDec -> DigDec 
prevDD dd = ddOfInt ((ddAsInt dd - 1) `mod` 10)


-- EJERCICIO 4
data Medida = Mm   Float | Cm   Float
            | Inch Float | Foot Float 

--a)
asMm :: Medida -> Medida 
asMm (Cm n)   = Mm (n * 10)
asMm (Inch n) = Mm (n * 25.4)
asMm (Foot n) = Foot (n * 304.8)
asMm mm       = mm 

--b)
asCm :: Medida -> Medida 
asCm (Mm n)   = Cm (n / 10)
asCm (Inch n) = Cm (n * 2.54)
asCm (Foot n) = Cm (n * 30.48)
asCm cm       = cm 

--c)
asInch :: Medida -> Medida
asInch (Mm n)   = Inch (n / 25.4)
asInch (Cm n)   = Inch (n / 2.54)
asInch (Foot n) = Inch (n * 12)
asInch inch     = inch

--d)
asFoot :: Medida -> Medida 
asFoot (Mm n)   = Foot (n / 304.8)
asFoot (Cm n)   = Foot (n / 30.48)
asFoot (Inch n) = Foot (n / 12)
asFoot foot     = foot 


-- EJERCICIO 5
data Shape = Circle Float | Rect Float Float 

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0


{-
a)
    uncurry :: (a -> b -> c) -> (a, b) -> c
    Rect:: Float -> Float -> Shape
    --------------------------------------- a <- Float | b <- Float | c <- Shape
    uncurry Rect :: (Float, Float) -> Shape


b)

    flip :: (a -> b -> c) -> b -> a -> c
    Rect :: Float -> Float -> Shape
    ------------------------------------ a <- Float | b <- Float | c <- Shape
    flip Rect :: Float -> Float -> Shape


    construyeShNormal :: (Float -> Shape) -> Shape
    (flip Rect 5.0) :: Float -> Shape
    ----------------------------------------------
    construyeShNormal (flip Rect 5.0) :: Shape



c)
    compose :: (b -> c) -> (a -> b) -> a -> c
    uncurry Rect :: (Float, Float) -> Shape
    ----------------------------------------- b <- (Float, Float) | c <- Shape
    compose (uncurry Rect) :: (a -> (Float, Float)) -> a -> Shape
    swap :: (d,e) -> (e, d)
    ------------------------------------------------------------- a <- (d, e) | e <- Float | d <- Float
    compose (uncurry Rect) swap :: (Float, Float) -> Shape  




d)
    uncurry :: (a -> b -> c) -> (a,b) -> c
    Cucurucho :: Gusto -> Gusto -> Helado
    -------------------------------------- a <- Gusto | b <- Gusto | c <- Helado
    uncurry Cucurucho :: (Gusto, Gusto) -> Helado



e)
    uncurry Rect :: (Float, Float) -> Shape
    swap :: (a,b) -> (b,a)
    ---------------------------------------
    uncurry Rect swap :/:



f)
    compose :: (b -> c) -> (a -> b) -> a -> c
    uncurry :: (x -> y -> z) -> (x,y) -> z
    -------------------------------------------- b <- (x -> y -> z) | c <- ((x,y) -> z)
    compose uncurry :: (a -> x -> y -> z) -> a -> (x, y) -> z
    Pote :: Gusto -> Gusto -> Gusto -> Helado 
    --------------------------------------------------------- a <- Gusto | x <- Gusto | y <- Gusto | z <- Helado
    compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado


g)
    compose :: (b -> c) -> (a -> b) -> a -> c
    Just :: d -> Maybe d
    ----------------------------------------- d <- b | c <- Maybe d 
    compose Just :: (a -> b) -> a -> Maybe b



h)
    compose uncurry :: (a -> b -> c -> d) -> a -> (b, c) -> d
    Pote Chocolate :: Gusto -> Gusto -> Helado
    ---------------------------------------------------------
    compose uncurry (Pote Chocolate) :/:

-}

---------------------------
compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

data Maybe a = Nothing | Just a 

-- EJERCICIO 6

miRec :: Shape
miRec = uncurry Rect (2.0, 3.0)


miRecF :: Shape 
miRecF = flip Rect 3.0 2.0

miRecS :: Shape 
miRecS = compose (uncurry Rect) swap (3.0, 2.0)

miHelado :: Helado 
miHelado = uncurry Cucurucho (Chocolate, Frutilla)

miHeladoEnPote :: Helado 
miHeladoEnPote = compose uncurry Pote Chocolate (Chocolate, Frutilla)

miNumero :: Maybe Int 
miNumero = compose Just (\x -> x*2) 7



-- EJERCICIO 7
data Set a = S (a -> Bool)

--a)
belongs :: Set a -> a -> Bool 
belongs (S f) x = f x

--b)
empty :: Set a 
empty = S (const False)

--c)  MAL
singleton :: a -> Set a 
singleton x = let elem = x in
                        S (\y -> case y of 
                        elem -> True 
                        _ -> False)

--d)
union :: Set a -> Set a -> Set a
union (S f) (S g) = S (\x -> f x || g x)

--e)
intersection :: Set a -> Set a -> Set a 
intersection (S f) (S g) = S (\x -> f x && g x)


-- EJERCICIO 8 
data MayFail a = Raise Exception | Ok a 
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Ok x)    f handler = f x
tryCatch (Raise e) f handler = handler e