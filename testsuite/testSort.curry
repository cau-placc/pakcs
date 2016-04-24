-- Some tests for module Sort
--
-- To run all tests automatically by the currytest tool, use the command:
-- "currytest testSort"

import Sort
import Assertion

testMergeSort1 = assertEqual "mergeSort1" [1,3,3,4,5,6,7,7,8,23,32,35]
                    (mergeSortBy (<=) [3,1,5,3,7,4,32,8,35,7,23,6])

testMergeSort2 = let n = 10000 in
  assertEqual "mergeSort2" [1..n] (mergeSortBy (<=) [1..n])

testMergeSort3 = let n = 10000 in
  assertEqual "mergeSort3" [1..n] (mergeSortBy (<=) [n,n-1..1])


testQuickSort = assertEqual "quickSort"
  (quickSortBy leqString ["zx","zz","ad","cd","hf","he","hff"])
  ["ad","cd","he","hf","hff","zx","zz"]
