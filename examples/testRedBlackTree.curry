------------------------------------------------------------------------------
--- Some tests for library RedBlackTree.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testRedBlackTree"
--- 
--- @author Bernd Brassel
--- @version April 2005
------------------------------------------------------------------------------

import Random
import Assertion
import RedBlackTree

intList2Tree = foldr update (empty (\ _ _ -> False) (==) (<))

rndTree n = getRandomSeed >>= return . take n . nextInt >>= \is -> return (intList2Tree is,is)

sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x < y && sorted (y:xs)

rndDels n x = getRandomSeed >>= return . take n . (flip nextIntRange x)

deleteTest t _ [] = t
deleteTest t is (x:xs) = deleteTest (delete (is !! x) t) is xs

testIO m n =   
          rndTree m >>= \ (t,is) -> 
          rndDels n (length is) >>= \ ds ->
          let newt = deleteTest t is ds
           in return (sorted (tree2list newt))

test  = assertIO 
           ("Create tree with 1000 random entries, then randomly delete 100.\n" ++
            "Test, if result is sorted.")
           (testIO 1000 100)
           True
    
