import Prelude hiding ((+), (*))
data Nat = Zero | Succ Nat


(+) :: Nat -> Nat -> Nat 
Zero + m      = m
(Succ n) + m  = Succ (n + m)

(*) :: Nat -> Nat -> Nat 
Zero * m      = Zero
(Succ n) * m  = (n * m) + m
