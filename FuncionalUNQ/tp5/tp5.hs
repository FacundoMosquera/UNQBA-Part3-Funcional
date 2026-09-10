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

asMm mm = mm 