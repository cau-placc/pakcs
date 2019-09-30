-- Testing Prelude operations:

import Test.Prop

-- Test simple arithmetic:
testPlus  = (2+3) -=- 5

testMinus = (1 - 5) -=- (-4)


-- Testing compare:

testCompare1 = (compare True False) -=- GT

testCompare2 = (compare 3 4) -=- LT

testCompare3 = (compare Nothing (Just (not unknown))) -=- LT

testCompare4 = (compare (True,failed) (False,True)) -=- GT

testCompare5 = failing (compare (True,failed) (True,True))

testCompare6 = (compare 'A' 'a') -=- LT

testCompare7 = (compare "AAB" "AA") -=- GT


-- Testing integer arithmetic:

testDiv1     =  (13 `div` 5)     -=- 2

testDiv2     =  (15 `div` (-4))  -=- (-4)

testMod1     =  (13 `mod` 5)     -=- 3

testMod2     =  ((-15) `mod` 4)  -=- 1

testQuot1    =  (13 `quot` 5)    -=- 2

testQuot2    =  (15 `quot` (-4)) -=- (-3)

testRem1     =  (13 `rem` 5)     -=- 3

testRem2     =  ((-15) `rem` 4)  -=- (-3)

testDivMod1  =  (divMod 7 2)     -=- (3,1)

testDivMod2  =  (divMod 7 (-2))  -=- (-4,-1)

testQuotRem1 =  (quotRem 7 2)    -=- (3,1)

testQuotRem2 =  (quotRem 7 (-2)) -=- (-3,1)

testModDivProperty  x y = y/=0  ==>  x `mod` y  -=-  x - y * (x `div` y)

testRemQuotProperty x y = y/=0  ==>  x `rem` y  -=-  x - y * (x `quot` y)
