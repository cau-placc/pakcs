-- Testing fcase expressions:

import Assertion

fNot x = fcase x of
          False -> True
          True  -> False

testNot1 = assertValues "fNot1" (fNot False) [True]

testNot2 = assertValues "fNot2" (fNot _) [True,False]


aBool1 = fcase () of
           _ -> True
           _ -> False

testABool1 = assertValues "aBool1" aBool1 [True,False]


aBool2 = fcase _ of
           True  -> True
           False -> False

testABool2 = assertValues "aBool2" aBool2 [True,False]


firstOrSecond zs =
  fcase zs of
    (x:_)    -> x
    (_:y:_)  -> y

testFS = assertValues "firstOrSecond" (firstOrSecond [1,2,3]) [1,2]


posFirstOrSecond zs =
  case zs of
    (x:_)   | x>0 -> x
    (_:y:_) | y>0 -> y

--testPFS = assertValues "posFirstOrSecond" (posFirstOrSecond [0,2,3]) [2]


