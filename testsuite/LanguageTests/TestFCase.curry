-- Testing fcase expressions:

import Test.Prop

fNot x = fcase x of
          False -> True
          True  -> False

testNot1 = (fNot False) <~> True

testNot2 = (fNot _)     <~> (True ? False)


aBool1 = fcase () of
           _ -> True
           _ -> False

testABool1 = aBool1 <~> (True ? False)


aBool2 = fcase _ of
           True  -> True
           False -> False

testABool2 = aBool2 <~> (True ? False)


firstOrSecond zs =
  fcase zs of
    (x:_)    -> x
    (_:y:_)  -> y

testFirstOrSecond = (firstOrSecond [1,2,3]) <~> (1 ? 2)


posFirstOrSecond zs =
  fcase zs of
    (x:_)   | x>0 -> x
    (_:y:_) | y>0 -> y

testPosFirstOrSecond = (posFirstOrSecond [0,2,3]) <~> 2


