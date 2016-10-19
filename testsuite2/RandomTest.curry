-- A few auxiliary functions to formulate tests with random numbers.
module RandomTest(test,eq) where

import Random
import List(nub)
import Test.EasyCheck

--- Tests a given predicate on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
test :: ([Int] -> Bool) -> PropIO
test f = (rndList lenRnds >>= \xs -> return (if f xs then Nothing else Just xs))
         `returns` Nothing

--- Tests whether two operations return equal results
--- on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
eq :: ([Int] -> a) -> ([Int] -> a) -> PropIO
eq f g = test (\x -> (f x)==(g x))

--- generate a list of at most n random numbers (without duplicated elements)
rndList :: Int -> IO [Int]
rndList n = getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000)

--- maximal length of test lists
lenRnds :: Int
lenRnds = 1000
