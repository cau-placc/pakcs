-- Test deriving clauses for user-defined datatypes

import Test.Prop

data Color = Red | Blue
 deriving (Eq, Read, Show, Ord)

data Nat = Z | S Nat
 deriving (Eq, Read, Show)

data Tree a = Leaf a | Node [Tree a]
 deriving (Eq, Show, Read, Ord)

showOfRead :: (Eq a, Read a, Show a) => a -> Prop
showOfRead x = read (show x) -=- x

showOfReadNat :: Nat -> Prop
showOfReadNat = showOfRead

showOfReadColor :: Color -> Prop
showOfReadColor = showOfRead

showOfReadTreeColor :: Tree Color -> Prop
showOfReadTreeColor = showOfRead

testOrd1 = always (Red < Blue)

testOrd2 = always (Leaf Red < Leaf Blue)

testOrd3 :: Color -> [Tree Color] -> Prop
testOrd3 x y = always (Leaf x < Node y)
