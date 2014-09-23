------------------------------------------------------------------------------
--- Some tests for recursive let/where declarations which are
--- supported in PAKCS.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testLetRec"
--- 
--- @author Michael Hanus
--- @version January 2005
------------------------------------------------------------------------------

import Assertion


ones5 = let ones = 1 : ones
         in take 5 ones

testOnes = assertEqual "recursive let" ones5 [1,1,1,1,1]

twolist n = take n twos
  where twos = 2 : twos

testTwos = assertEqual "recursive where" (twolist 5) [2,2,2,2,2]

onetwo n = take n ones2
 where
   ones2 = 1 : twos1
   twos1 = 2 : ones2

testOneTwo = assertEqual "mutually recursive where" (onetwo 6) [1,2,1,2,1,2]
