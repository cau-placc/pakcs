------------------------------------------------------------------------------
-- Some tests for the library UnsafeSearchTree.
------------------------------------------------------------------------------

import Test.EasyCheck
import UnsafeSearchTree
import qualified SearchTree as ST

--------------------------------------------------------------------------------
-- Testing isVar:

testIsVar1 =
  always (not (isVar (someValue (let x free in x =:= (_,_) &> x))))

testIsVar2 =
  always (isVar (fst (someValue (let x free in x =:= (_,_) &> x))))

--------------------------------------------------------------------------------
-- Testing varId:

testGetVarId1 =
  always
   (let (a,b) = someValue (let x free in x =:= (_,_) &> x)
     in (varId a /= varId b))

testGetVarId2 =
  always
   (let (a,b) = someValue (let x,y,z free in (x =:= (y,z) & y=:=z) &> x)
     in (varId a == varId b))

--------------------------------------------------------------------------------
-- The following tests also demonstrate why the encapsulated search
-- with unbound variables in result values is non-declarative.

testNumOfNongroundValues =
   (length (allValuesDFS (someSearchTree (let x free in id (x::Bool)))))
   -=- 1

testNumOfGroundValues =
   (length (allValuesDFS (someSearchTree (let x free in not (not (x::Bool))))))
   -=- 2

-- However, there is no difference w.r.t. the SearchTree library:

testNumberOfSearchTreeValues1 =
   (length (ST.allValuesDFS (ST.someSearchTree (let x free in id (x::Bool)))))
   -=- 2

testNumberOfSearchTreeValues2 =
   (length (ST.allValuesDFS (ST.someSearchTree
                                 (let x free in not (not (x::Bool))))))
   -=- 2

--------------------------------------------------------------------------------
