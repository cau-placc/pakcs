-- Testing narrowing features:

import Assertion

-- natural numbers defined by s-terms (O=zero, S=successor):
data Nat = O | S Nat
 deriving (Eq,Show)

-- addition on natural numbers:
add         :: Nat -> Nat -> Nat
add O     n = n
add (S m) n = S(add m n)

-- subtraction defined by reversing the addition:
sub x y | add y z =:= x  = z where z free

-- less-or-equal predicated on natural numbers:
leq O     _     = True
leq (S _) O     = False
leq (S x) (S y) = leq x y


-- and now the test cases:

testAdd = assertValues "add" (add (S O) (S O)) [S (S O)]

testDiff = assertSolutions "diff" (\x->add x (S O) =:= S (S O)) [S O]

testSub = assertValues "sub" (sub (S (S O)) (S O)) [S O]

testLeq = assertSolutions "leq" (\x -> leq x (S(S O)) =:= True)
                                [O, S O, S (S O)]


-- Narrowing features for lists:
append []     ys = ys
append (x:xs) ys = x : append xs ys

last xs | append _ [x] =:= xs
        = x  where x free

testLast = assertValues "last" (last [1,2,3,4]) [4]

