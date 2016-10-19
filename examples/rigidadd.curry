-- First example of POPL'97 paper: evaluate addition by residuation:

data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add x y = add' (ensureNotFree x) y

add' O     n = n
add' (S m) n = S(add m n )


isNat :: Nat -> Bool

isNat O     = True
isNat (S n) = isNat n 



goal = let x free in add x O =:= S O & isNat x

