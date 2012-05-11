------------------------------------------------------------------------------
--- A collection of common non-deterministic and/or combinatorial operations.
--- Many operations are intended to operate on sets.
--- The representation of these sets is not hidden; rather
--- sets are represented as lists.
--- Ideally these lists contains no duplicate elements and
--- the order of their elements cannot be observed.
--- In practice, these conditions are not enforced.
---
--- @author Sergio Antoy
--- @version August 2004
--  Fri Aug 20 11:04:22 MEST 2004
------------------------------------------------------------------------------

module Combinatorial(permute, subset, splitSet,
                     sizedSubset, partition) where

------------------------------------------------------------------
--                       Public Operations
------------------------------------------------------------------

--- Compute any permutation of a list.
--- For example, [1,2,3,4] may give [1,3,4,2].
---
--- @param l - The list.
--- @return A permutation of the argument.

permute        :: [a] -> [a]
permute []     = []
permute (x:xs) = ndinsert (permute xs)
    where ndinsert [] = [x]
          ndinsert (y:ys) = (x:y:ys) ? (y:ndinsert ys)

--- Compute any sublist of a list.
--- The sublist contains some of the elements of the list in the same order.
--- For example, [1,2,3,4] may give [1,3], and
--- [1,2,3] gives [1,2,3], [1,2], [1,3], [1], [2,3], [2], [3], or [].
---
--- @param l - The list.
--- @return A sublist of the argument.

subset        :: [a] -> [a]
subset []     = []
subset (x:xs) = x:subset xs
subset (_:xs) =   subset xs

--- Compute all the sublists of a list.
--- For example, [1,2,3] gives [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]].
---
--- @param l - The list.
--- @return All the sublists of the argument.

allSubsets      :: [a] -> [[a]]
allSubsets list = findall ((subset list) =:=)

--- Split a list into any two sublists.
--- For example, [1,2,3,4] may give ([1,3,4],[2]).
---
--- @param l - The list.
--- @return A pair consisting of two complementary sublists of the argument.

splitSet    :: [a] -> ([a],[a])
splitSet [] = ([],[])
splitSet (x:xs) = let (u,v) = splitSet xs in (x:u,v) ? (u,x:v)

--- Compute any sublist of fixed length of a list.
--- Similar to <code>subset</code>, but the length of the result is fixed.
---
--- @param c - The length of the output sublist.
--- @param l - The input list.
--- @return A sublist of <code>l</code> of length <code>c</code>.

sizedSubset     :: Int -> [a] -> [a]
sizedSubset c l = if c == 0 then [] else aux l
    where aux (x:xs) = x:sizedSubset (c-1) xs ? sizedSubset c xs

--- Compute any partition of a list.
--- The output is a list of non-empty lists such that their concatenation
--- is a permutation of the input list.
--- No guarantee is made on the order of the arguments in the output.
--- For example, [1,2,3,4] may give [[4],[2,3],[1]], and
--- [1,2,3] gives [[1,2,3]], [[2,3],[1]], [[1,3],[2]], [[3],[1,2]],
--- or [[3],[2],[1]].
---
--- @param l - The input list.
--- @return A partition of <code>l</code> represented as a list of lists.

partition    :: [a] -> [[a]]
partition [] = []
partition (x:xs) = insert x (partition xs)
    where insert e [] = [[e]]
          insert e (y:ys) = ((e:y):ys) ? (y:insert e ys)


-- end module Combinatorial
