data Nat = Zero | Succ Nat

convert :: Nat -> Integer 
convert Zero     = 0
convert (Succ n) = 1 + convert n

