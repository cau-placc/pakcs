-- Some tests for module Combinatorial
--
-- To run all tests automatically by the currycheck tool, use the command:
-- "currycheck testCombinatorial"

import Combinatorial
import Integer
import List(nub)
import Sort(mergeSortBy)
import AllSolutions
import Test.EasyCheck

------------------------------------------------------------------

testPermuteEmpty = (permute (tail [1])) <~> []

testPermuteSingle = (permute [1]) <~> [1]

testPermuteTriple =
  (permute [1,2,3])
  <~>  ([1,2,3] ? [2,1,3] ? [2,3,1] ? [1,3,2] ? [3,1,2] ? [3,2,1])


------------------------------------------------------------------

testSubset = 
  (subset [0,1,2,3]) <~>
  ([] ? [3] ? [2] ? [2,3] ? [1] ? [1,3] ? [1,2] ? [1,2,3] ? 
   [0] ? [0,3] ? [0,2] ? [0,2,3] ? [0,1] ? [0,1,3] ? [0,1,2] ? [0,1,2,3])

------------------------------------------------------------------

testSizedSubset =
  (getAllValues (sizedSubset size base) >>= return . length)
  `returns` (binomial (length base) size)
 where
  base = [0,1,2,3,4,5,6,7]
  size = 4

------------------------------------------------------------------

testSplitSet =
  (getAllValues (splitSet input) >>= \output ->
   return $ length output == pow 2 (length input) -- check length
            && nub output == output               -- check distinct
            && elem ([2,3],[1,4]) output &&       -- check memgers
               elem ([1,2,3],[4]) output &&
               elem ([],[1,2,3,4]) output)
  `returns` True
 where
  input = [1,2,3,4] 

------------------------------------------------------------------

testPartition =
  (getAllValues (partition input) >>= \output ->
   return $ all (all (not . null)) output           -- checkNotEmpty
            && nub output == output                 -- checkDistinct
            && all (\x -> length x == length input) -- checkMemberLength
                   (map concat output)
            && all (\x -> mergeSortBy (<) x == input) -- checkMemberContent
                   (map concat output))
  `returns` True
 where
  input = [1,2,3,4] 

------------------------------------------------------------------
