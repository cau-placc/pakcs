----------------------------------------------------------------------------
--- OLD (OBSOLETE) Version of a
--- library with an implementation of sets as red-black trees.
--- <P>
--- All the operations on sets are generic, i.e., one has to provide
--- an explicit order predicate ("<CODE>cmp</CODE>" below) on elements.
--- 
--- @author Johannes Koj, Michael Hanus
--- @version May 2001
----------------------------------------------------------------------------

module SetRBT0(SetRBT,emptySetRBT,insertRBT,insertMultiRBT,elemRBT,setRBT2list,
              unionRBT,intersectRBT,sortRBT) where


----------------------------------------------------------------------------
-- the main interface:

--- Returns an empty set, i.e., an empty red-black tree.
emptySetRBT = RBSEmpty

--- Returns true if an element is contained in a (red-black tree) set.
--- @param cmp - order predicate on elements
--- @param e - an element to be checked for containment
--- @param s - a set (represented as a red-black tree)
--- @return True if e is contained in s
elemRBT :: (a->a->Bool) -> a -> SetRBT a -> Bool
elemRBT _ _ RBSEmpty = False
elemRBT cmp e (RBS _ e1 l r) | e1 == e   = True
                             | cmp e e1  = elemRBT cmp e l
                             | otherwise = elemRBT cmp e r

--- Inserts an element into a set if it is not already there.
--- The first argument is the order on elements.
insertRBT :: (a->a->Bool) -> a -> SetRBT a -> SetRBT a
insertRBT cmp e s = let (RBS _ e2 l r) = ins s
                     in RBS RBSBlack e2 l r
  where
    ins RBSEmpty = RBS RBSRed e RBSEmpty RBSEmpty
    ins (RBS c e2 l r) | e == e2   = RBS c e2 l r
                       | cmp e e2  = rbsBalanceL (RBS c e2 (ins l) r)
                       | otherwise = rbsBalanceR (RBS c e2 l (ins r))

--- Inserts an element into a multiset.
--- Thus, the same element can have several occurrences in the multiset.
insertMultiRBT :: (a->a->Bool) -> a -> SetRBT a -> SetRBT a
insertMultiRBT cmp e s = let (RBS _ e2 l r) = ins s
                          in RBS RBSBlack e2 l r
  where
    ins RBSEmpty = RBS RBSRed e RBSEmpty RBSEmpty
    ins (RBS c e2 l r) | cmp e e2  = rbsBalanceL (RBS c e2 (ins l) r)
                       | otherwise = rbsBalanceR (RBS c e2 l (ins r))

--- Transforms a (red-black tree) set into an ordered list of its elements.
setRBT2list :: SetRBT a -> [a]
setRBT2list t = s2l t []
  where
    s2l RBSEmpty es = es
    s2l (RBS _ e l r) es = s2l l (e : s2l r es)

--- Computes the union of two (red-black tree) sets.
--- This is done by inserting all elements of the first set into the
--- second set.
unionRBT :: (a->a->Bool) -> SetRBT a -> SetRBT a -> SetRBT a
unionRBT cmp s1 s2 = foldr (insertRBT cmp) s2 (setRBT2list s1)

--- Computes the intersection of two (red-black tree) sets.
--- This is done by inserting all elements of the first set
--- contained into the second set into a new set.
intersectRBT :: (a->a->Bool) -> SetRBT a -> SetRBT a -> SetRBT a
intersectRBT cmp s1 s2 = foldr (insertRBT cmp) emptySetRBT
                               (filter (\e->elemRBT cmp e s2) (setRBT2list s1))


--- Generic sort based on insertion into red-black trees.
sortRBT  :: (a->a->Bool) -> [a] -> [a]
sortRBT cmp xs = setRBT2list (foldr (insertMultiRBT cmp) emptySetRBT xs)


----------------------------------------------------------------------------
-- implementation of red-black trees:

--- The colors of a node in a red-black tree.
data RBSColor = RBSRed | RBSBlack

--- The structure of red-black trees.
data SetRBT a = RBS RBSColor a (SetRBT a) (SetRBT a) | RBSEmpty

rbsBalance :: SetRBT a -> SetRBT a
rbsBalance RBSEmpty = RBSEmpty
rbsBalance (RBS c i l r) | c/=RBSBlack = RBS c i l r
                         | c2==RBSRed  = RBS c2 e2 l2 r2
                         | otherwise  = rbsBalanceR (RBS c i l r)
  where (RBS c2 e2 l2 r2) = rbsBalanceL (RBS c i l r)

rbsBalanceL :: SetRBT a -> SetRBT a
rbsBalanceL (RBS c i RBSEmpty r) = RBS c i RBSEmpty r
rbsBalanceL (RBS c1 e1 (RBS c2 e2 RBSEmpty RBSEmpty) r1) =
      RBS c1 e1 (RBS c2 e2 RBSEmpty RBSEmpty) r1
rbsBalanceL (RBS c1 e1 (RBS c2 e2 (RBS c3 e3 l3 r3) RBSEmpty) r1)
   | c2==RBSRed && c3==RBSRed =
                 RBS RBSRed e2 (RBS RBSBlack e3 l3 r3)
                               (RBS RBSBlack e1 RBSEmpty r1)
   | otherwise = RBS c1 e1 (RBS c2 e2 (RBS c3 e3 l3 r3) RBSEmpty) r1
rbsBalanceL (RBS c1 e1 (RBS c2 e2 RBSEmpty (RBS c3 e3 l3 r3)) r1)
   | c2==RBSRed && c3==RBSRed =
                 RBS RBSRed e3 (RBS RBSBlack e2 RBSEmpty l3)
                               (RBS RBSBlack e1 r3 r1)
   | otherwise = RBS c1 e1 (RBS c2 e2 RBSEmpty (RBS c3 e3 l3 r3)) r1
rbsBalanceL (RBS c1 e1 (RBS c2 e2 (RBS c3 e3 l3 r3) 
                                  (RBS c4 e4 l4 r4)) r1)
   | c2==RBSRed && c3==RBSRed =
                 RBS RBSRed e2 (RBS RBSBlack e3 l3 r3) 
                               (RBS RBSBlack e1 (RBS c4 e4 l4 r4) r1)
   | c2==RBSRed && c4==RBSRed =
                 RBS RBSRed e4 (RBS RBSBlack e2 (RBS c3 e3 l3 r3) l4)
                               (RBS RBSBlack e1 r4 r1)
   | otherwise = RBS c1 e1 (RBS c2 e2 (RBS c3 e3 l3 r3) (RBS c4 e4 l4 r4)) r1

rbsBalanceR :: SetRBT a -> SetRBT a
rbsBalanceR (RBS c i l RBSEmpty) = RBS c i l RBSEmpty
rbsBalanceR (RBS c1 e1 l1 (RBS c2 e2 RBSEmpty RBSEmpty)) =
                 RBS c1 e1 l1 (RBS c2 e2 RBSEmpty RBSEmpty)
rbsBalanceR (RBS c1 e1 l1 (RBS c2 e2 (RBS c3 e3 l3 r3) RBSEmpty))
   | c2==RBSRed && c3==RBSRed =
                 RBS RBSRed e3 (RBS RBSBlack e1 l1 l3)
                               (RBS RBSBlack e2 r3 RBSEmpty)
   | otherwise = RBS c1 e1 l1 (RBS c2 e2 (RBS c3 e3 l3 r3) RBSEmpty)
rbsBalanceR (RBS c1 e1 l1 (RBS c2 e2 RBSEmpty (RBS c3 e3 l3 r3)))
   | c2==RBSRed && c3==RBSRed =
                 RBS RBSRed e2 (RBS RBSBlack e1 l1 RBSEmpty)
                               (RBS RBSBlack e3 l3 r3)
   | otherwise = RBS c1 e1 l1 (RBS c2 e2 RBSEmpty (RBS c3 e3 l3 r3))
rbsBalanceR (RBS c1 e1 l1 (RBS c2 e2 (RBS c3 e3 l3 r3)
                                     (RBS c4 e4 l4 r4)))
   | c2==RBSRed && c3==RBSRed =
                 RBS RBSRed e3 (RBS RBSBlack e1 l1 l3)
                               (RBS RBSBlack e2 r3 (RBS c4 e4 l4 r4))
   | c2==RBSRed && c4==RBSRed =
                 RBS RBSRed e2 (RBS RBSBlack e1 l1 (RBS c3 e3 l3 r3))
                               (RBS RBSBlack e4 l4 r4) 
   | otherwise = RBS c1 e1 l1 (RBS c2 e2 (RBS c3 e3 l3 r3) (RBS c4 e4 l4 r4))


-- end module RBSet