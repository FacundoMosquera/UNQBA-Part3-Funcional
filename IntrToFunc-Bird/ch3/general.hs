import Prelude hiding ((^))
data Nat = Zero | Succ Nat

instance Num Nat where 
    m + Zero   = m 
    m + Succ n = Succ (m + n)

    m * Zero   = Zero 
    m * Succ n = (m * n) + m

(^) :: Nat -> Nat -> Nat 
m ^ Zero   = Succ Zero 
m ^ Succ n = (m ^ n) * m