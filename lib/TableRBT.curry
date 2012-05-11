---------------------------------------------------------------------------
--- Library with an implementation of tables as red-black trees:
--- <P>
--- A table is a finite mapping from keys to values.
--- All the operations on tables are generic, i.e., one has to provide
--- an explicit order predicate ("<CODE>cmp</CODE>" below) on elements.
--- Each inner node in the red-black tree contains a key-value association.
--- 
--- @author Johannes Koj, Michael Hanus, Bernd Brassel
--- @version March 2005
----------------------------------------------------------------------------

module TableRBT where

import qualified RedBlackTree as RBT
--import RedBlackTree (RedBlackTree) -- uncomment for old (buggy) Java front end

----------------------------------------------------------------------------
-- the main interface:

type TableRBT key a = RBT.RedBlackTree (key,a) 

--- Returns an empty table, i.e., an empty red-black tree.
emptyTableRBT :: (a -> a -> Bool) -> TableRBT a _ 
emptyTableRBT lt = RBT.empty (\ x y -> fst x==fst y) 
                             (\ x y -> fst x==fst y)
                             (\ x y -> lt (fst x) (fst y))

--- tests whether a given table is empty
isEmptyTable :: TableRBT _ _ -> Bool
isEmptyTable = RBT.isEmpty

--- Looks up an entry in a table.
--- @param k - a key under which a value is stored
--- @param t - a table (represented as a red-black tree)
--- @return (Just v) if v is the value stored with key k,
---         otherwise Nothing is returned.
lookupRBT :: key -> TableRBT key a -> Maybe a
lookupRBT k = maybe Nothing (Just . snd) . RBT.lookup (k,failed)

--- Inserts or updates an element in a table.
updateRBT :: key -> a -> TableRBT key a -> TableRBT key a
updateRBT k e = RBT.update (k,e)

--- Transforms the nodes of red-black tree into a list.
tableRBT2list :: TableRBT key a -> [(key,a)]
tableRBT2list = RBT.tree2list

deleteRBT :: key -> TableRBT key a -> TableRBT key a
deleteRBT key = RBT.delete (key,failed)

-- end of TableRBT
