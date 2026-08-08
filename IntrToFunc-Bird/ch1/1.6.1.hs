const :: a -> b -> a
const x y = x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)

apply :: (a -> b) -> a -> b
apply f x = f x

flip :: (b -> a -> c) -> a -> b -> c
flip f x y = f y x