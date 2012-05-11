----------------------------------------------------------------------------
--- Library with an implementation of sets as red-black trees.
--- <P>
--- All the operations on sets are generic, i.e., one has to provide
--- an explicit order predicate ("<CODE>cmp</CODE>" below) on elements.
--- 
--- @author Johannes Koj, Michael Hanus, Bernd Brassel
--- @version May 2003
----------------------------------------------------------------------------

module SetRBT where

import qualified RedBlackTree as RBT
--import RedBlackTree (RedBlackTree) -- uncomment for old (buggy) Java front end
import Maybe (isJust)

----------------------------------------------------------------------------
-- the main interface:

type SetRBT a = RBT.RedBlackTree a

--- Returns an empty set, i.e., an empty red-black tree 
--- augmented with an order predicate.

emptySetRBT = RBT.empty (==) (==)

--- Returns true if an element is contained in a (red-black tree) set.
--- @param e - an element to be checked for containment
--- @param s - a set (represented as a red-black tree)
--- @return True if e is contained in s
elemRBT :: a -> SetRBT a -> Bool
elemRBT e = isJust . (RBT.lookup e)

--- Inserts an element into a set if it is not already there.
insertRBT :: a -> SetRBT a -> SetRBT a
insertRBT = RBT.update 


--- Inserts an element into a multiset.
--- Thus, the same element can have several occurrences in the multiset.

insertMultiRBT :: a -> SetRBT a -> SetRBT a
insertMultiRBT e = RBT.setInsertEquivalence (==) . 
                   RBT.update e . 
                   RBT.setInsertEquivalence (\ _ _ -> False)

--- delete an element from a set.
--- Deletes only a single element from a multi set
deleteRBT :: a -> SetRBT a -> SetRBT a
deleteRBT = RBT.delete

--- Transforms a (red-black tree) set into an ordered list of its elements.
setRBT2list :: SetRBT a -> [a]
setRBT2list = RBT.tree2list

--- Computes the union of two (red-black tree) sets.
--- This is done by inserting all elements of the first set into the
--- second set. 
unionRBT :: SetRBT a -> SetRBT a -> SetRBT a
unionRBT s1 s2 = foldr insertRBT s2 (setRBT2list s1)

--- Computes the intersection of two (red-black tree) sets.
--- This is done by inserting all elements of the first set
--- contained in the second set into a new set, which order
--- is taken from the first set.
intersectRBT :: SetRBT a -> SetRBT a -> SetRBT a
intersectRBT s1 s2 = foldr insertRBT (RBT.newTreeLike s1)
                          (filter (\e->elemRBT e s2) (setRBT2list s1))


--- Generic sort based on insertion into red-black trees.
--- The first argument is the order for the elements.

sortRBT  :: (a->a->Bool) -> [a] -> [a]
sortRBT = RBT.sort

