-- Testing narrowing features:

import Test.Prop

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

testAdd  = (add (S O) (S O))                           -=- S (S O)

testDiff = solutionOf (\x -> add x (S O) =:= S (S O))  -=- S O

testSub  = (sub (S (S O)) (S O))                       -=- S O

testLeq  = solutionOf (\x -> leq x (S(S O))) <~> (O ? S O ? S (S O))

-- Property: the addition operator is commutative
addIsCommutative x y = add x y -=- add y x

-- Property: the addition operator is associative
addIsAssociative x y z = add (add x y) z -=- add x (add y z)

-- Properties: subtracting a value which was added yields the same value
subAddLeft x y = sub (add x y) x -=- y

subAddRight x y = sub (add x y) y -=- x

-- Property: adding a number yields always a greater-or-equal number
leqAddIsGreaterOrEqual x y = always $ leq x (add x y)


-- Narrowing features for lists:
append []     ys = ys
append (x:xs) ys = x : append xs ys

last xs | append _ [x] =:= xs
        = x  where x free

testLast1234 = last [1,2,3,4]  -=-  4

lastIsElem xs = not (null xs) ==> always $ last xs `elem` xs
