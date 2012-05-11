---------------------------------------------------------------------------
--- Library with an implementation of red-black trees:
--- <P>
--- Serves as the base for both TableRBT and SetRBT
--- All the operations on trees are generic, i.e., one has to provide
--- two explicit order predicates ("<CODE>lessThan</CODE>" and "<CODE>eq</CODE>"below) 
--- on elements.
--- 
--- @author Johannes Koj, Michael Hanus, Bernd Brassel
--- @version March 2005
----------------------------------------------------------------------------

module RedBlackTree 
  (RedBlackTree, empty, isEmpty, lookup, update, 
   tree2list, sort, newTreeLike, setInsertEquivalence, delete
  )   where 

----------------------------------------------------------------------------
-- the main interface:

--- A red-black tree consists of a tree structure and three order predicates.
--- These predicates generalize the red black tree. They define
--- 1) equality when inserting into the tree<br>
---    eg for a set eqInsert is (==), 
---       for a multiset it is (\ _ _ -> False)
---       for a lookUp-table it is ((==) . fst) 
--- 2) equality for looking up values 
---    eg for a set eqLookUp is (==), 
---       for a multiset it is (==)
---       for a lookUp-table it is ((==) . fst) 
--- 3) the (less than) relation for the binary search tree

data RedBlackTree a = RedBlackTree (a->a->Bool) (a->a->Bool) (a->a->Bool) (Tree a)

--- The three relations are inserted into the structure by function empty.
--- Returns an empty tree, i.e., an empty red-black tree 
--- augmented with the order predicates.

empty :: (a->a->Bool) -> (a->a->Bool) -> (a->a->Bool) -> RedBlackTree a
empty eqInsert eqLookUp lessThan = RedBlackTree eqInsert eqLookUp lessThan Empty 

--- Test on emptyness
isEmpty  (RedBlackTree _ _ _ Empty)          = True
isEmpty  (RedBlackTree _ _ _ (Tree _ _ _ _)) = False

--- Creates a new empty red black tree from with the same ordering as a give one.
newTreeLike (RedBlackTree eqIns eqLk lt _) = RedBlackTree eqIns eqLk lt Empty

--- Returns an element if it is contained in a red-black tree.
--- @param p - a pattern for an element to look up in the tree
--- @param t - a red-black tree
--- @return the contained True if p matches in t

lookup :: a -> RedBlackTree a -> Maybe a
lookup p (RedBlackTree _ eqLk lt t) = lookupTree eqLk lt p t

lookupTree :: (a->a->Bool) -> (a->a->Bool) -> a -> Tree a -> Maybe a
lookupTree _ _ _ Empty = Nothing
lookupTree eq lt p (Tree _ e l r) 
     | eq p e    = Just e
     | lt p e    = lookupTree eq lt p l
     | otherwise = lookupTree eq lt p r

--- Updates/inserts an element into a RedBlackTree.
update :: a -> RedBlackTree a -> RedBlackTree a
update e (RedBlackTree eqIns eqLk lt t) = 
          RedBlackTree eqIns eqLk lt (updateTree eqIns lt e t)

updateTree :: (a->a->Bool) -> (a->a->Bool) -> a -> Tree a -> Tree a
updateTree eq lt e t = let (Tree _     e2 l r) = upd t
                        in  Tree Black e2 l r
  where
    upd Empty = Tree Red e Empty Empty
    upd (Tree c e2 l r) | eq e e2   = Tree c e l r
                        | lt e e2   = balanceL (Tree c e2 (upd l) r)
                        | otherwise = balanceR (Tree c e2 l (upd r))

--- Deletes entry from red black tree.
delete :: a -> RedBlackTree a -> RedBlackTree a
delete e (RedBlackTree eqIns eqLk lt t) =
          RedBlackTree eqIns eqLk lt (blackenRoot (deleteTree eqLk lt e t))
  where
    blackenRoot Empty = Empty
    blackenRoot (Tree _ x l r) = Tree Black x l r 

deleteTree _ _ _ Empty = Empty  -- no error for non existence
deleteTree eq lt e (Tree c e2 l r) 
      | eq e e2 = if l==Empty then addColor c r else 
                  if r==Empty then addColor c l
                               else let el = rightMost l 
                                     in delBalanceL (Tree c el (deleteTree eq lt el l) r)
      | lt e e2 = delBalanceL (Tree c e2 (deleteTree eq lt e l) r)
      | otherwise = delBalanceR (Tree c e2 l (deleteTree eq lt e r))
  where
    addColor Red tree = tree
    addColor Black Empty = Empty
    addColor Black (Tree Red x lx rx)   = Tree Black x lx rx
    addColor Black (Tree Black x lx rx) = Tree DoublyBlack x lx rx

    rightMost (Tree _ x _ rx) = if rx==Empty then x else rightMost rx
                                

--- Transforms a red-black tree into an ordered list of its elements.
tree2list :: RedBlackTree a -> [a]
tree2list (RedBlackTree _ _ _ t) = tree2listTree t 
 
tree2listTree tree = t2l tree [] 
   where
     t2l Empty es = es
     t2l (Tree _ e l r) es = t2l l (e : t2l r es)

--- Generic sort based on insertion into red-black trees.
--- The first argument is the order for the elements.

sort  :: (a->a->Bool) -> [a] -> [a]
sort cmp xs = tree2list (foldr update (empty (\_ _->False) (==) cmp) xs)

--- For compatibility with old version only
setInsertEquivalence :: (a->a->Bool) -> RedBlackTree a -> RedBlackTree a
setInsertEquivalence eqIns (RedBlackTree _ eqLk lt t) = RedBlackTree eqIns eqLk lt t

----------------------------------------------------------------------------
-- implementation of red-black trees:

rbt (RedBlackTree _ _ _ t) = t 

--- The colors of a node in a red-black tree.
data Color = Red | Black | DoublyBlack

--- The structure of red-black trees.
data Tree a = Tree Color a (Tree a) (Tree a)
            | Empty

isBlack :: Tree _ -> Bool
isBlack Empty = True
isBlack (Tree c _ _ _) = c==Black

isRed :: Tree _ -> Bool
isRed Empty = False
isRed (Tree c _ _ _) = c==Red

isDoublyBlack :: Tree _ -> Bool
isDoublyBlack Empty = True
isDoublyBlack (Tree c _ _ _) = c==DoublyBlack

element :: Tree a -> a
element (Tree _ e _ _) = e

left :: Tree a -> Tree a
left (Tree _ _ l _) = l

right :: Tree a -> Tree a
right (Tree _ _ _ r) = r

singleBlack Empty = Empty
singleBlack (Tree DoublyBlack x l r) = Tree Black x l r

--- for the implementation of balanceL and balanceR refer to picture 3.5, page 27, 
--- Okasaki "Purely Functional Data Structures"
balanceL :: Tree a  ->  Tree a
balanceL tree 
  | isRed leftTree && isRed (left leftTree)
  = let Tree _ z (Tree _ y (Tree _ x a b) c) d = tree
     in Tree Red y (Tree Black x a b) (Tree Black z c d)
  
  | isRed leftTree && isRed (right leftTree)
  = let Tree _ z (Tree _ x a (Tree _ y b c)) d = tree
     in Tree Red y (Tree Black x a b) (Tree Black z c d)

  | otherwise = tree

  where
    leftTree = left tree 
        
balanceR :: Tree a  ->  Tree a
balanceR tree
  | isRed rightTree && isRed (right rightTree)
  = let Tree _ x a (Tree _ y b (Tree _ z c d)) = tree
     in Tree Red y (Tree Black x a b) (Tree Black z c d)

  | isRed rightTree && isRed (left rightTree)
  = let Tree _ x a (Tree _ z (Tree _ y b c) d) = tree
     in Tree Red y (Tree Black x a b) (Tree Black z c d)

  | otherwise = tree

  where 
    rightTree = right tree


--- balancing after deletion

delBalanceL :: Tree a  ->  Tree a
delBalanceL tree = if isDoublyBlack (left tree) then reviseLeft tree else tree

reviseLeft tree
  | r==Empty = tree
  | blackr && isRed (left r)
  = let Tree col x a (Tree _ z (Tree _ y b c) d) = tree
     in Tree col y (Tree Black x (singleBlack a) b) (Tree Black z c d)
  | blackr && isRed (right r) 
  = let Tree col x a (Tree _ y b (Tree _ z c d)) = tree
     in Tree col y (Tree Black x (singleBlack a) b) (Tree Black z c d)
  | blackr 
  = let Tree col x a (Tree _ y b c) = tree
     in Tree (if col==Red then Black else DoublyBlack) x (singleBlack a) (Tree Red y b c)
  | otherwise 
  = let Tree _ x a (Tree _ y b c) = tree
     in Tree Black y (reviseLeft (Tree Red x a b)) c
  where
    r = right tree
    blackr = isBlack r

delBalanceR :: Tree a  ->  Tree a
delBalanceR tree = if isDoublyBlack (right tree) then reviseRight tree else tree

reviseRight tree
  | l==Empty = tree
  | blackl && isRed (left l)
  = let Tree col x (Tree _ y (Tree _ z d c) b) a = tree
     in Tree col y (Tree Black z d c) (Tree Black x b (singleBlack a))
  | blackl && isRed (right l) 
  = let Tree col x (Tree _ z d (Tree _ y c b)) a = tree
     in Tree col y (Tree Black z d c) (Tree Black x b (singleBlack a))
  | blackl 
  = let Tree col x (Tree _ y c b) a = tree
     in Tree (if col==Red then Black else DoublyBlack) x (Tree Red y c b) (singleBlack a)
  | otherwise 
  = let Tree _ x (Tree _ y c b) a = tree
     in Tree Black y c (reviseRight (Tree Red x b a))
  where
    l = left tree
    blackl = isBlack l

