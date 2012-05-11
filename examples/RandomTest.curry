-- A few auxiliary functions to formulate tests with random numbers.
module RandomTest where

import Random
import Assertion

--- generate a list of n random numbers
rndList :: Int -> IO [Int]
rndList n =  getRandomSeed >>= return . take n . nextInt

--- test a given predicate on lists
test :: String -> ([Int]->Bool) -> Assertion Bool
test s f = assertIO s (rndList lenRnds >>= return . f) True

--- test equality on random list
eq :: String -> ([Int]->a) -> ([Int]->a) -> Assertion Bool 
eq s f g = test s (\x -> (f x)==(g x))

--- length of test lists
lenRnds = 1000
