-- Testing functional features:

import Test.Prop

-- Concatenating two lists:
append :: [a] -> [a] -> [a]
append []     x  = x
append (x:xs) ys = x : append xs ys

testAppend1234 = append [1,2] [3,4] -=- [1,2,3,4]

appendIsAssociative xs ys zs =
  append (append xs ys) zs -=- append xs (append ys zs)


-- Reverse (naively) the order of elements in a list:
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

testRev1234 = rev [1,2,3,4] -=- [4,3,2,1]

revLeavesSingletonUntouched x = rev [x] -=- [x]

revOfNonemptyIsNotNull xs = not (null xs) ==> rev xs `is` (not . null)

revAntiDistributesOverAppend xs ys =
  rev (append xs ys) -=- append (rev ys) (rev xs)

revEqualReverse xs = rev xs -=- reverse xs
