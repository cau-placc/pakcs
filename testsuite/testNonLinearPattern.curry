-- Testing non-linear patterns in left-hand sides of rules.

import Assertion

-- Simple test for non-linear patterns:
fstEqu :: [a] -> [a] -> [a]
fstEqu (x:xs) (x:_) = xs

test1 = assertValues "nonlinear simple1" (fstEqu [1,2] [2,3]) []
test2 = assertValues "nonlinear simple2" (fstEqu [1,2] [1,3]) [[2]]


-- Test whether non-linearity test comes before the guard:
f :: [a] -> [a] -> a
f x x | loop x = head x

loop x = loop x

test3 = assertValues "nonlinear first" (f [1] [2]) []


-- member constraint with non-linear patterns
member x (x:_) = success
member x (_:xs) = member x xs

test4 = assertSolutions "member" (\x -> member x [1,2,3]) [1,2,3]
