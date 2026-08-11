pair :: (a -> b, a -> c) -> a -> (b,c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a,c) -> (b,d)
cross (f, g) = pair (f . fst, g . snd)

{-
    DEMOSTRAR
    cross (f,g) . cross (h,k) = cross (f . h, g . k)

        cross (f,g) . cross (h,k)
    =                       { def. cross, con f<- h y g<- k }
        cross (f,g) . pair (h . fst, k . snd)
    =                       { equivalencia demostrada en el libro }
        pair (f . (h . fst), g . (k . snd))
    =                       { asociatividad de (.) }
        pair ((f . h) . fst, (g . k) . snd))
    =                       { def. cross, con f<- (f . h) y g<- (g . k) }
        cross (f . h, g . k)
-}