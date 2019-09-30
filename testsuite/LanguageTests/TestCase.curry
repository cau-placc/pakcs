-- Testing case expressions:

import Test.Prop

swap :: [Int] -> [Int]
swap l = case l of
  [x,y] -> [y,x]
  _     -> l

testSwap1 = (swap []     ) -=- []
testSwap2 = (swap [1]    ) -=- [1]
testSwap3 = (swap [1,2]  ) -=- [2,1]
testSwap4 = (swap [1,2,3]) -=- [1,2,3]


f l = case l of
        []   -> 0
        _:xs -> 100 + case xs of
                          [y] -> y
                      + 50

testf1 = (f []   ) -=- 0
testf2 = failing (f [1])
testf3 = (f [1,2]) -=- 152

g x = case x of
        Just "abc" -> True
        Just "xyz" -> True
        _          -> False

testg1 = (g (Just "xyz")) -=- True
testg2 = (g (Just "ab") ) -=- False

h x = case x of
        [1,2] -> True
        _     -> False

testh1 = (h [1,2])       -=- True
testh2 = (h [1,3])       -=- False
testh3 = (h [2,div 1 0]) -=- False


firstOrSecond zs =
  case zs of
    (x:_)    -> x
    (_:y:_)  -> y

testFirstOrSecond = (firstOrSecond [1,2,3]) -=- 1


posFirstOrSecond zs =
  case zs of
    (x:_)   | x>0 -> x
    (_:y:_) | y>0 -> y

testPosFirstOrSecond = (posFirstOrSecond [0,2,3]) -=- 2

