------------------------------------------------------------------------------
--- Some tests for recursive let/where declarations.
------------------------------------------------------------------------------

import Test.Prop


ones5 = let ones = 1 : ones
         in take 5 ones

testOnes = ones5 -=- [1,1,1,1,1]

twolist n = take n twos
  where twos = 2 : twos

testTwos = (twolist 5) -=- [2,2,2,2,2]

onetwo n = take n ones2
 where
   ones2 = 1 : twos1
   twos1 = 2 : ones2

testOneTwo = (onetwo 6) -=- [1,2,1,2,1,2]
