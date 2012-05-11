-- Some tests for module Combinatorial
--
-- To run all tests automatically by the currytest tool, use the command:
-- "currytest testCombinatorial"

import Combinatorial
import Integer
import List(nub)
import Sort(mergeSort)
import Assertion -- to assert test cases
import AllSolutions

------------------------------------------------------------------

testPermute1 = assertValues "permute empty" (permute (tail [1])) [[]]

testPermute2 = assertValues "permute single" (permute [1]) [[1]]

testPermute3 = assertValues "permute triple" (permute [1,2,3])
                 [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]


------------------------------------------------------------------

testSubset = assertValues "subset"
  (subset [0,1,2,3])
  [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3],
   [0],[0,3],[0,2],[0,2,3],[0,1],[0,1,3],[0,1,2],[0,1,2,3]]

------------------------------------------------------------------

testSizedSubset = assertEqualIO "sizedSubset"
                     (getAllValues (sizedSubset size base) >>= return . length)
                     (return (binomial (length base) size))
    where base = [0,1,2,3,4,5,6,7]
          size = 4

------------------------------------------------------------------

testSplitSet = assertEqualIO "splitSet" (return True)
  (getAllValues (splitSet input) >>= \output ->
   return $ length output == pow 2 (length input) -- check length
            && nub output == output               -- check distinct
            && elem ([2,3],[1,4]) output &&       -- check memgers
               elem ([1,2,3],[4]) output &&
               elem ([],[1,2,3,4]) output)
    where input = [1,2,3,4] 

------------------------------------------------------------------

testPartition = assertEqualIO "partition" (return True)
  (getAllValues (partition input) >>= \output ->
   return $ all (all (not . null)) output           -- checkNotEmpty
            && nub output == output                 -- checkDistinct
            && all (\x -> length x == length input) -- checkMemberLength
                   (map concat output)
            && all (\x -> mergeSort (<) x == input) -- checkMemberContent
                   (map concat output))
    where input = [1,2,3,4] 

------------------------------------------------------------------
