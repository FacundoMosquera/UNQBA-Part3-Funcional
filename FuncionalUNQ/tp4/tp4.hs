{-
    EJERCICIO 1

a) udiv (x,y) = div x y 
    
    Es una función parcial ya que si el segundo argumento es 0 (un valor definido)
    devolverá ⊥ ya que div es una función parcial


b) udivE (x,0) = error "No puedo dividir por 0"
   udivE (x,y) = div x y

    Sabiendo que error <string> = ⊥, udivE es parcial por el mismo motivo
    que udiv


c) udivH = uncurry div

    Es una función parcial ya que si el segundo argumento es 0 (un valor definido)
    devolverá ⊥


d) succ x = x + 1

    Es una función total ya que si recibe un número definido, devolverá ese número
    más 1


e) succH = suma 1

    Es una función total. La justificación es la misma que en el inciso d.


f) porLaMitad = flip div 2

    Es una función total ya que dado un número definido, devolverá ese
    número dividido por 2, una división definida.


g) conDieresis 'u' = 'ü'

    Es una función parcial, ya que dado un char diferente a 'u' devolverá ⊥
    al no haber una ecuación definida para ese caso.


h) conDieresisB 'u' = 'ü'
   conDieresisB c = conDieresisB c

   Es una función parcial, ya que si recibe un argumento diferente a 'u', se volverá a llamar así
   misma con el mismo argumento, metiendose en un loop infinito.


i) conTildePM 'a' = 'á'
   conTildePM 'e' = 'é'
   conTildePM 'i' = 'í'
   conTildePM 'o' = 'ó'
   conTildePM 'u' = 'ú'

   Es una función parcial ya que si recibe, por ejemplo, el char 'z', la función no tiene un caso
   definido y por lo tanto devolverá ⊥


j) conTildeE c = if esVocal c
                    then conTildePM c
                    else error "El valor recibido no es vocal"

    Es una función parcial porque si no es vocal, dará ⊥


k) conTilde c = if esVocal c && esMinuscula c
                    then conTildePM c
                    else c

    Depende del tipo de la función. Si es de Char -> Char es una función total, caso contrario
    es parcial.



    EJERCICIO 2

    udiv = udivE = udivH

    succ = succH

    conDieresis = conDieresisB

    conTildePM = conTildeE


    
    EJERCICIO 3

    twice = \f -> \x -> f (f x)

a)      twice doble        --- 2 redex en total  --1 redex
    ->                    (def. twice)
        (\f -> \x -> f (f x)) doble    --1 redex
    ->                    (def. beta, f <- doble)
        (\x -> doble (doble x))
    


b)      twice doble 2      --- +3 redex en total   --#1 redex
    ->                     (def. twice)   
        (\f -> \x -> f (f x)) doble 2    --1 redex
    ->                     (def. beta, f <- doble)
        (\x -> doble (doble x)) 2          --1 redex
    ->                     (def. beta, x <- 2)
        doble (doble 2)                 -- 2 redex
    ->                     (def. doble con x <- doble 2)
        doble 2 + doble 2               -- 2 redex
    ->                     (def. doble con x <- 2)
        2 + 2 + 2 + 2                   -- 3 redex
    ->                     (aritmética)
        8


c)      twice    -- 1 redex
    ->             (def. twice)
        \f -> \x -> f (f x)





    EJERCICIO 4

    twice f = g
            where g x = f (f x)


a)      twice doble -- 1 redex
    ->              (def. twice con f <- doble)
        g           (donde g x = doble (doble x))



b)      twice doble 2
    ->                (def. twice con f <- doble)
        g 2           (donde g x = doble (doble x))
    ->                (def. g con x <- 2)
        doble (doble 2)
    ->                     (def. doble con x <- doble 2)
        doble 2 + doble 2               -- 2 redex
    ->                     (def. doble con x <- 2)
        2 + 2 + 2 + 2                   -- 3 redex
    ->                     (aritmética)
        8

c)     twice    --0 redex





    EJERCICIO 5 

    twice f x = f (f x)

a)      twice doble -- 0 redex en total

b)      twice doble 2     --1 redex
    ->                  (def. twice con f <- doble, x <- 2)
        doble (doble 2)                 -- 2 redex
    ->                     (def. doble con x <- doble 2)
        doble 2 + doble 2               -- 2 redex
    ->                     (def. doble con x <- 2)
        2 + 2 + 2 + 2                   -- 3 redex
    ->                     (aritmética)
        8


c)      twice -- 0 redex



    
    EJERCICIO 6

    No es posible dar expresiones que denoten valores diferentes a bottom


-}