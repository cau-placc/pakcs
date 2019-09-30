-- Testing non-linear patterns in left-hand sides of rules.

import Test.Prop

-- Simple test for non-linear patterns:
fstEqu :: [a] -> [a] -> [a]
fstEqu (x:xs) (x:_) = xs

testNonLinearSimple1 = failing (fstEqu [1,2] [2,3])
testNonLinearSimple2 = (fstEqu [1,2] [1,3]) <~> [2]


-- Test whether non-linearity test comes before the guard:
f :: [a] -> [a] -> a
f x x | loop x = head x

loop x = loop x

testNonLinearFirst = failing (f [1] [2])


-- member constraint with non-linear patterns
member x (x:_) = success
member x (_:xs) = member x xs

testMember = solutionOf (\x -> member x [1,2,3]) <~> (1 ? 2 ? 3)
