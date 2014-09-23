-- Testing Prelude operations:

import Assertion

-- Test simple arithmetic:
testPlus = assertEqual "plus" (2+3) 5

testMinus = assertEqual "minus" (1 - 5) (-4)

-- Testing compare:

testCompare1 = assertEqual "compare1" (compare True False) GT

testCompare2 = assertEqual "compare2" (compare 3 4) LT

testCompare3 = assertEqual "compare3" (compare Nothing (Just (not unknown))) LT

testCompare4 = assertEqual "compare4" (compare (True,failed) (False,True)) GT

testCompare5 = assertValues "compare5" (compare (True,failed) (True,True)) []

testCompare6 = assertEqual "compare6" (compare 'A' 'a') LT

testCompare7 = assertEqual "compare7" (compare "AAB" "AA") GT

-- Testing integer arithmetic:

testDiv1 = assertEqual "div1" (13 `div` 5) 2

testDiv2 = assertEqual "div2" (15 `div` (-4)) (-4)

testMod1 = assertEqual "mod1" (13 `mod` 5) 3

testMod2 = assertEqual "mod2" ((-15) `mod` 4) 1

testQuot1 = assertEqual "quot1" (13 `quot` 5) 2

testQuot2 = assertEqual "quot2" (15 `quot` (-4)) (-3)

testRem1 = assertEqual "rem1" (13 `rem` 5) 3

testRem2 = assertEqual "rem2" ((-15) `rem` 4) (-3)

testDivMod1 = assertEqual "divMod1" (divMod 7 2) (3,1)

testDivMod2 = assertEqual "divMod2" (divMod 7 (-2)) (-4,-1)

testQuotRem1 = assertEqual "quotRem1" (quotRem 7 2) (3,1)

testQuotRem2 = assertEqual "quotRem2" (quotRem 7 (-2)) (-3,1)
