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
