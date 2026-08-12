case' :: (a -> c, b -> c) -> Either a b -> c
case' (f, g) (Left x)  = f x 
case' (f, g) (Right y) = g y


plus :: (a -> c, b -> d) -> Either a b -> Either c d 
plus (f, g) = case' (Left . f, Right . g)


{-
    DEMOSTRAR
    case (f, g) . plus (h, k) = case (f . h, g . k)
    

        case (f, g) . plus (h, k)
    =                          { def. plus }
        case (f, g) . case (Left . h, Right . k)
    =                          { 3° prop. dada por el libro, con h<- case (f, g) }
        case ( case (f, g) . Left . h, case (f, g) . Right . k )
    =                          { 1° prop. dada por el libro }
        case ( f . h, case (f, g) . Right . k)
    =                          { 2° prop. dada por el libro }
        case (f . h, g . k)
-}




{- 
    EXTRA 

    -Prop. 1 

        case (f, g) . Left = f
    =               { por principio de extensionalidad, 
                        dado un x arbitrario perteneciente al conjunto a}
        (case (f, g) . Left) x = f x


        (case (f, g) . Left) x
    =               { def. (.) }
        case (f, g) (Left x)
    =               { def. case, primera ecuación }
        f x                



    -Prop. 2 

        case (f, g) . Right = g
    =               { por principio de extensionalidad, 
                        dado un x arbitrario perteneciente al conjunto a}
        (case (f, g) . Right) x = g x

        (case (f, g) . Right) x 
    =                      { def. (.) }
        case (f, g) (Right x)
    =                       { def. case, segunda ecuación}
        g x


-}