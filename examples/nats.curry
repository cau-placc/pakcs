-- natural numbers defined by s-terms (Z=zero, S=successor):
data Nat = Z | S Nat

-- addition on natural numbers:
add         :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

-- subtraction defined by reversing the addition:
sub x y | add y z == x  = z where z free

-- less-or-equal predicated on natural numbers:
leq Z     _     = True
leq (S _) Z     = False
leq (S x) (S y) = leq x y


goal1 = sub (S(S(S(S Z)))) (S(S Z))

goal2 | leq x (S(S Z)) = x   where x free
