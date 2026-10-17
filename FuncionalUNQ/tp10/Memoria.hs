module Memoria
    (Memoria, enBlanco, cuantoVale, recordar, variables)
    where

data Memoria = M [(Variable, Int)]
type Variable = String

---------------------TAD---------------------
enBlanco :: Memoria 
cuantoVale ::  Variable -> Memoria -> Maybe Int 
recordar :: Variable -> Int -> Memoria -> Memoria 
variables :: Memoria -> [Variable]
---------------------------------------------


---------------------IMPL---------------------
enBlanco = M []


cuantoVale v (M ks) = buscar v ks


recordar v n (M ks) = M (assoc v n ks)

variables (M xs) = map fst xs

assoc :: Variable -> Int -> [(Variable, Int)] -> [(Variable, Int)]
assoc v n []          = [(v,n)]
assoc v n ((k,m):kms) = if v==k then (v,n):kms else (k,m) : assoc v n kms


buscar :: Variable -> [(Variable, Int)] -> Maybe Int
buscar v []          = Nothing
buscar v ((k,n):kns) = if v==k then Just n else buscar v kns



{-
        Sería más apropiado específicar el funcionamiento intencionado para las implementaciones con
    propiedades que deberían se deberían cumplir:
        
            ∀x. cuantoVale x enBlanco = Nothing
            ∀m.∀x.∀n. cuantoVale x (recordar x n m) = Just n
            ∀m.∀x.∀y.∀n. si x != y, entonces
                    cuantoVale x (recordar y n m) = cuantoVale x m
            ∀m.∀x. x `elem` variables m = True, es equivalente a  cuantoVale x m != Nothing
-}