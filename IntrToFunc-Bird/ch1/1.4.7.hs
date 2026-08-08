{-
*curry (uncurry f) x y = f x y

    curry (uncurry f) x y
=>                def curry
   (uncurry f) (x,y)
=>                def uncurry
    f x y


* uncurry (curry f) (x,y) = f (x,y)

    uncurry (curry f) (x,y)
=>                 def uncurry 
    (curry f) x y
=>                 def curry 
    f (x,y)
-}