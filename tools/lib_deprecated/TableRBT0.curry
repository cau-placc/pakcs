---------------------------------------------------------------------------
--- OLD (OBSOLETE) Version of a
--- library with an implementation of tables as red-black trees:
--- <P>
--- A table is a finite mapping from keys to values.
--- All the operations on tables are generic, i.e., one has to provide
--- an explicit order predicate ("<CODE>cmp</CODE>" below) on elements.
--- Each inner node in the red-black tree contains a key-value association.
--- 
--- @author Johannes Koj, Michael Hanus
--- @version May 2001
----------------------------------------------------------------------------

module TableRBT0(TableRBT,emptyTableRBT,lookupRBT,updateRBT,tableRBT2list) where

----------------------------------------------------------------------------
-- the main interface:

--- Returns an empty table, i.e., an empty red-black tree.
emptyTableRBT = RBTEmpty

--- Looks up an entry in a table.
--- @param cmp - order predicate on keys
--- @param k - a key under which a value is stored
--- @param t - a table (represented as a red-black tree)
--- @return (Just v) if v is the value stored with key k,
---         otherwise Nothing is returned.
lookupRBT :: (key->key->Bool) -> key -> TableRBT key a -> Maybe a
lookupRBT _ _ RBTEmpty = Nothing
lookupRBT cmp k (RBT _ i1 e l r) | i1 == k  = Just e
                                 | cmp k i1 = lookupRBT cmp k l
                                 | otherwise = lookupRBT cmp k r

--- Inserts or updates an element in a table.
--- The first argument is the order on elements.
updateRBT :: (key->key->Bool) -> key -> a -> TableRBT key a -> TableRBT key a
updateRBT cmp k v t = let (RBT _ i2 e2 l r) = ins t
                       in RBT RBTBlack i2 e2 l r
  where
    ins RBTEmpty = RBT RBTRed k v RBTEmpty RBTEmpty
    ins (RBT c i2 e2 l r) | k == i2   = RBT c i2 v l r
                          | cmp k i2  = rbtBalanceL (RBT c i2 e2 (ins l) r)
                          | otherwise = rbtBalanceR (RBT c i2 e2 l (ins r))

--- Transforms the nodes of red-black tree into a list.
tableRBT2list :: TableRBT key a -> [(key,a)]
tableRBT2list t = t2l t []
  where
    t2l RBTEmpty kvs = kvs
    t2l (RBT _ key val l r) kvs = t2l l ((key,val) : t2l r kvs)

----------------------------------------------------------------------------
-- implementation of red-black trees:

--- The colors of a node in a red-black tree.
data RBTColor = RBTRed | RBTBlack

--- The structure of red-black trees.
data TableRBT key a = RBT RBTColor key a (TableRBT key a) (TableRBT key a)
                    | RBTEmpty

rbtBalance :: TableRBT key a  ->  TableRBT key a
rbtBalance RBTEmpty = RBTEmpty
rbtBalance (RBT c i e l r) | c/=RBTBlack = RBT c i e l r
                           | c2==RBTRed  = RBT c2 i2 e2 l2 r2
                           | otherwise  = rbtBalanceR (RBT c i e l r)
  where (RBT c2 i2 e2 l2 r2) = rbtBalanceL (RBT c i e l r)

rbtBalanceL :: TableRBT key a  ->  TableRBT key a
rbtBalanceL (RBT c i e RBTEmpty r) = RBT c i e RBTEmpty r
rbtBalanceL (RBT c1 i1 e1 (RBT c2 i2 e2 RBTEmpty RBTEmpty) r1) =
      RBT c1 i1 e1 (RBT c2 i2 e2 RBTEmpty RBTEmpty) r1
rbtBalanceL (RBT c1 i1 e1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) RBTEmpty) r1)
   | c2==RBTRed && c3==RBTRed =
      RBT RBTRed i2 e2 (RBT RBTBlack i3 e3 l3 r3)
                       (RBT RBTBlack i1 e1 RBTEmpty r1)
   | otherwise = RBT c1 i1 e1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) RBTEmpty) r1
rbtBalanceL (RBT c1 i1 e1 (RBT c2 i2 e2 RBTEmpty (RBT c3 i3 e3 l3 r3)) r1)
   | c2==RBTRed && c3==RBTRed =
      RBT RBTRed i3 e3 (RBT RBTBlack i2 e2 RBTEmpty l3)
                       (RBT RBTBlack i1 e1 r3 r1)
   | otherwise = RBT c1 i1 e1 (RBT c2 i2 e2 RBTEmpty (RBT c3 i3 e3 l3 r3)) r1
rbtBalanceL (RBT c1 i1 e1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) 
                                       (RBT c4 i4 e4 l4 r4)) r1)
   | c2==RBTRed && c3==RBTRed =
      RBT RBTRed i2 e2 (RBT RBTBlack i3 e3 l3 r3) 
                       (RBT RBTBlack i1 e1 (RBT c4 i4 e4 l4 r4) r1)
   | c2==RBTRed && c4==RBTRed =
      RBT RBTRed i4 e4 (RBT RBTBlack i2 e2 (RBT c3 i3 e3 l3 r3) l4)
                       (RBT RBTBlack i1 e1 r4 r1)
   | otherwise =
      RBT c1 i1 e1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) (RBT c4 i4 e4 l4 r4)) r1

rbtBalanceR :: TableRBT key a  ->  TableRBT key a
rbtBalanceR (RBT c i e l RBTEmpty) = RBT c i e l RBTEmpty
rbtBalanceR (RBT c1 i1 e1 l1 (RBT c2 i2 e2 RBTEmpty RBTEmpty)) =
      RBT c1 i1 e1 l1 (RBT c2 i2 e2 RBTEmpty RBTEmpty)
rbtBalanceR (RBT c1 i1 e1 l1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) RBTEmpty))
   | c2==RBTRed && c3==RBTRed =
      RBT RBTRed i3 e3 (RBT RBTBlack i1 e1 l1 l3)
                       (RBT RBTBlack i2 e2 r3 RBTEmpty)
   | otherwise =
      RBT c1 i1 e1 l1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) RBTEmpty)
rbtBalanceR (RBT c1 i1 e1 l1 (RBT c2 i2 e2 RBTEmpty (RBT c3 i3 e3 l3 r3)))
   | c2==RBTRed && c3==RBTRed =
      RBT RBTRed i2 e2 (RBT RBTBlack i1 e1 l1 RBTEmpty)
                       (RBT RBTBlack i3 e3 l3 r3)
   | otherwise =
      RBT c1 i1 e1 l1 (RBT c2 i2 e2 RBTEmpty (RBT c3 i3 e3 l3 r3))
rbtBalanceR (RBT c1 i1 e1 l1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3)
                                           (RBT c4 i4 e4 l4 r4)))
   | c2==RBTRed && c3==RBTRed =
      RBT RBTRed i3 e3 (RBT RBTBlack i1 e1 l1 l3)
                       (RBT RBTBlack i2 e2 r3 (RBT c4 i4 e4 l4 r4))
   | c2==RBTRed && c4==RBTRed =
      RBT RBTRed i2 e2 (RBT RBTBlack i1 e1 l1 (RBT c3 i3 e3 l3 r3))
                       (RBT RBTBlack i4 e4 l4 r4) 
   | otherwise =
      RBT c1 i1 e1 l1 (RBT c2 i2 e2 (RBT c3 i3 e3 l3 r3) (RBT c4 i4 e4 l4 r4))


-- end of TableRBT
