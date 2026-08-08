strange :: ((a -> b) -> a) -> (a -> b) -> b
strange f g = g (f g)

--stranger f = f f  #Da error por tipo infinito