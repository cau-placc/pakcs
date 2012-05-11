-----------------------------------------------------------------------------
--- A finite map is an efficient purely functional data structure 
--- to store a mapping from keys to values.
--- In order to store the mapping efficiently, an irreflexive(!) order predicate 
--- has to be given, i.e., the order predicate <code>le</code> should not satisfy
--- <code>(le x x)</code> for some key <code>x</code>.
---
--- Example: To store a mapping from Int -&gt; String, the finite map needs
--- a Boolean predicate like (&lt;).
--- This version was ported from a corresponding Haskell library
---
--- @author Frank Huch, Bernd Brassel
--- @version May 2005
-----------------------------------------------------------------------------

module FiniteMap (
        FM,                -- abstract type

        emptyFM, 
        unitFM, 
        listToFM,

        addToFM,
        addToFM_C,
        addListToFM,
        addListToFM_C,
        delFromFM,
        delListFromFM,
        splitFM,

        plusFM,
        plusFM_C,
        minusFM,
        intersectFM,
        intersectFM_C,

        foldFM, 
        mapFM, 
        filterFM, 

        sizeFM, 
        eqFM,
        isEmptyFM, 
        elemFM, 
        lookupFM, 
        lookupWithDefaultFM,
        keyOrder,

        fmToList, 
        keysFM, 
        eltsFM,
        fmSortBy,

        minFM,maxFM,updFM, fmToListPreOrder
    ) where

import Maybe

--- order predicates are boolean 
type LeKey key = key -> key -> Bool

-----------------------------------------------
--        BUILDING finite maps
-----------------------------------------------

--- The empty finite map.
--- @param le an irreflexive order predicate on the keys.
--- @result an empty finite map

emptyFM :: (LeKey key) -> FM key _
emptyFM le = FM le EmptyFM

--- Construct a finite map with only a single element.
--- @param le an irreflexive order predicate on the keys.
--- @param key key of 
--- @param elt the single element to form
--- @result a finite map with only a single element
unitFM :: (LeKey key) -> key -> elt -> FM key elt
unitFM le key elt = FM le (unitFM' key elt)

unitFM' key elt = Branch key elt 1 EmptyFM EmptyFM


--- Builts a finite map from given list of tuples (key,element).
--- For multiple occurences of key, the last corresponding 
--- element of the list is taken.
--- @param le an irreflexive order predicate on the keys.
listToFM :: (LeKey key) -> [(key,elt)] -> FM key elt
listToFM le = addListToFM (emptyFM le)

-----------------------------------------------
--        ADDING AND DELETING
-----------------------------------------------

--- Throws away any previous binding and stores the new one given.

addToFM :: FM key elt -> key -> elt  -> FM key elt
addToFM (FM le fm) key elt = FM le (addToFM' le fm key elt)

addToFM' le fm key elt = addToFM_C' le (\ _ new -> new) fm key elt

addToFM_C' _ _ EmptyFM key elt = unitFM' key elt
addToFM_C' le combiner (Branch key elt size fm_l fm_r) new_key new_elt
  = if le new_key key
    then mkBalBranch key elt (addToFM_C' le combiner fm_l new_key new_elt) fm_r
    else
      if new_key==key
      then Branch new_key (combiner elt new_elt) size fm_l fm_r
      else mkBalBranch key elt fm_l (addToFM_C' le combiner fm_r new_key new_elt)


--- Throws away any previous bindings and stores the new ones given.
--- The items are added starting with the first one in the list
addListToFM :: FM key elt -> [(key,elt)] -> FM key elt
addListToFM (FM le fm) key_elt_pairs =
  FM le (addListToFM' le fm key_elt_pairs)

addListToFM' le fm key_elt_pairs =
  addListToFM_C' le (\ _ new -> new) fm key_elt_pairs

addListToFM_C' le combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs        -- foldl adds from the left
  where
    add fmap (key,elt) = addToFM_C' le combiner fmap key elt


--- Instead of throwing away the old binding, 
--- addToFM_C combines the new element with the old one.
--- @param combiner a function combining to elements
--- @param fm a finite map 
--- @param key the key of the elements to be combined
--- @param elt the new element
--- @result a modified finite map
addToFM_C :: (elt -> elt -> elt) -> FM key elt -> key -> elt
                                 -> FM key elt
addToFM_C combiner (FM le fm) key elt =  
  FM le (addToFM_C' le combiner fm key elt)

--- Combine with a list of tuples (key,element), cf. addToFM_C
addListToFM_C :: (elt -> elt -> elt) -> FM key elt -> [(key,elt)]
                                      -> FM key elt
addListToFM_C combiner (FM le fm) key_elt_pairs =
  FM le (addListToFM_C' le combiner fm key_elt_pairs)

--- Deletes key from finite map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delFromFM :: FM key elt -> key   -> FM key elt
delFromFM (FM le fm) del_key = FM le (delFromFM' le fm del_key)

delFromFM' _ EmptyFM _ = EmptyFM
delFromFM' le (Branch key elt _ fm_l fm_r) del_key
  = if le del_key key
    then mkBalBranch key elt (delFromFM' le fm_l del_key) fm_r
    else
      if del_key==key
        then glueBal le fm_l fm_r
        else mkBalBranch key elt fm_l (delFromFM' le fm_r del_key)

--- Deletes a list of keys from finite map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delListFromFM        :: FM key elt -> [key] -> FM key elt
delListFromFM (FM le fm) keys = FM le (foldl (delFromFM' le) fm keys)

--- Applies a function to element bound to given key.
updFM :: FM a b -> a -> (b -> b) -> FM a b
updFM (FM lt fm) i f = FM lt (upd fm)
  where
    upd EmptyFM                          =  EmptyFM
    upd (Branch k x h l r) 
            | i==k       =  Branch k (f x) h l r   
            | lt i k     =  Branch k x h (upd l) r  
            | otherwise  =  Branch k x h l (upd r) 

--- Combines delFrom and lookup.
splitFM :: FM a b -> a -> Maybe (FM a b,(a,b))
splitFM g v = maybe Nothing (\x->Just (delFromFM g v,(v,x))) (lookupFM g v)

-------------------------------------------------
-- COMBINING finite maps
-------------------------------------------------

--- Efficiently add key/element mappings of two maps into a single one.
--- Bindings in right argument shadow those in the left
plusFM :: FM key elt -> FM key elt -> FM key elt
plusFM (FM le1 fm1) (FM _ fm2) = FM le1 (plusFM' le1 fm1 fm2)

plusFM' _  EmptyFM fm2 = fm2
plusFM' _  (Branch split_key1 elt1 s1 left1 right1) EmptyFM =
  (Branch split_key1 elt1 s1 left1 right1)
plusFM' le (Branch split_key1 elt1 s1 left1 right1)
           (Branch split_key elt2 _ left right)
  = mkVBalBranch le split_key elt2 (plusFM' le lts left) (plusFM' le gts right)
  where
    fm1 = Branch split_key1 elt1 s1 left1 right1
    lts     = splitLT le fm1 split_key
    gts     = splitGT le fm1 split_key

--- Efficiently combine key/element mappings of two maps into a single one, 
--- cf. addToFM_C
plusFM_C :: (elt -> elt -> elt)
                           -> FM key elt -> FM key elt -> FM key elt
plusFM_C combiner (FM le1 fm1) (FM _ fm2) =
  FM le1 (plusFM_C' le1 combiner fm1 fm2)

plusFM_C' _  _        EmptyFM fm2 = fm2
plusFM_C' _  _        (Branch split_key1 elt1 s1 left1 right1) EmptyFM =
          Branch split_key1 elt1 s1 left1 right1
plusFM_C' le combiner (Branch split_key1 elt1 s1 left1 right1)
                      (Branch split_key elt2 _ left right)
  = mkVBalBranch le split_key new_elt
                 (plusFM_C' le combiner lts left)
                 (plusFM_C' le combiner gts right)
  where
    fm1 = Branch split_key1 elt1 s1 left1 right1
    lts     = splitLT le fm1 split_key
    gts     = splitGT le fm1 split_key
    new_elt = case lookupFM' le fm1 split_key of
                Nothing   -> elt2
                Just elt1' -> combiner elt1' elt2

--- (minusFM a1 a2) deletes from a1 any bindings which are bound in a2
minusFM :: FM key elt -> FM key elt -> FM key elt
minusFM (FM le1 fm1) (FM _ fm2) = FM le1 (minusFM' le1 fm1 fm2)

minusFM' _  EmptyFM _ = EmptyFM
minusFM' _  (Branch split_key1 elt1 s1 left1 right1) EmptyFM =
  Branch split_key1 elt1 s1 left1 right1
minusFM' le (Branch split_key1 elt1 s1 left1 right1)
            (Branch split_key _ _ left right)
  = glueVBal le (minusFM' le lts left) (minusFM' le gts right)
       -- The two can be way different, so we need glueVBal
  where
    fm1 = Branch split_key1 elt1 s1 left1 right1
    lts = splitLT le fm1 split_key  -- NB gt and lt, so the equal ones
    gts = splitGT le fm1 split_key  -- are not in either.

--- Filters only those keys that are bound in both of the given maps.
--- The elements will be taken from the second map.
intersectFM :: FM key elt -> FM key elt -> FM key elt
intersectFM (FM le1 fm1) (FM _ fm2) = FM le1 (intersectFM' le1 fm1 fm2)

intersectFM' le fm1 fm2 = intersectFM_C' le (\ _ right -> right) fm1 fm2

--- Filters only those keys that are bound in both of the given maps
--- and combines the elements as in addToFM_C.
intersectFM_C :: (elt -> elt -> elt2) -> FM key elt -> FM key elt -> FM key elt2

intersectFM_C combiner (FM le1 fm1) (FM _ fm2) =
  FM le1 (intersectFM_C' le1 combiner fm1 fm2)

intersectFM_C' _  _        _        EmptyFM = EmptyFM
intersectFM_C' _  _        EmptyFM (Branch _ _ _ _ _) = EmptyFM
intersectFM_C' le combiner (Branch split_key1 elt1 s1 left1 right1)
                           (Branch split_key elt2 _ left right)

  | isJust maybe_elt1   -- split_elt *is* in intersection
  = mkVBalBranch le split_key (combiner elt1' elt2) 
                 (intersectFM_C' le combiner lts left)
                 (intersectFM_C' le combiner gts right)

  | otherwise           -- split_elt is *not* in intersection
  = glueVBal le (intersectFM_C' le combiner lts left)
                (intersectFM_C' le combiner gts right)

  where
    fm1 = Branch split_key1 elt1 s1 left1 right1
    lts = splitLT le fm1 split_key      -- NB gt and lt, so the equal ones
    gts = splitGT le fm1 split_key      -- are not in either.

    maybe_elt1 = lookupFM' le fm1 split_key
    Just elt1'  = maybe_elt1

-------------------------------------------------------------
--  MAPPING, FOLDING, FILTERING on finite maps
-------------------------------------------------------------

--- Folds finite map by given function.
foldFM :: (key -> elt -> a -> a) -> a -> FM key elt -> a
foldFM k z (FM le fm) = foldFM' le k z fm

foldFM' _  _ z EmptyFM = z
foldFM' le k z (Branch key elt _ fm_l fm_r)
  = foldFM' le k (k key elt (foldFM' le k z fm_r)) fm_l

--- Applies a given function on every element in the map.
mapFM :: (key -> elt1 -> elt2) -> FM key elt1 -> FM key elt2
mapFM f (FM le fm) = FM le (mapFM' le f fm)

mapFM' _  _ EmptyFM = EmptyFM
mapFM' le f (Branch key elt size fm_l fm_r)
  = Branch key (f key elt) size (mapFM' le f fm_l) (mapFM' le f fm_r)

--- Yields a new finite map with only those key/element pairs matching the
--- given predicate.
filterFM  :: (key -> elt -> Bool) -> FM key elt -> FM key elt
filterFM p (FM le fm) = FM le (filterFM' le p fm)

filterFM' _  _ EmptyFM = EmptyFM
filterFM' le p (Branch key elt _ fm_l fm_r)
  | p key elt          -- Keep the item
  = mkVBalBranch le key elt (filterFM' le p fm_l) (filterFM' le p fm_r)

  | otherwise          -- Drop the item
  = glueVBal le (filterFM' le p fm_l) (filterFM' le p fm_r)

-----------------------------------------------------
-- INTERROGATING finite maps
-----------------------------------------------------

--- How many elements does given map contain?
sizeFM :: FM _ _ -> Int
sizeFM (FM _ EmptyFM)               = 0
sizeFM (FM _ (Branch _ _ size _ _)) = size

sizeFM' EmptyFM              = 0
sizeFM' (Branch _ _ size _ _) = size


--- Do two given maps contain the same key/element pairs?
eqFM :: FM key elt -> FM key elt -> Bool
fm_1 `eqFM` fm_2 =
  (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
  (fmToList fm_1 == fmToList fm_2)

--- Is the given finite map empty?
isEmptyFM        :: FM _ _ -> Bool
isEmptyFM fm = sizeFM fm == 0

--- Does given map contain given key?
elemFM :: key -> FM key _ -> Bool
key `elemFM` fm = isJust (lookupFM fm key)

--- Retrieves element bound to given key
lookupFM :: FM key elt -> key -> Maybe elt
lookupFM (FM le fm) key = lookupFM' le fm key

lookupFM' _  EmptyFM _   = Nothing
lookupFM' le (Branch key elt _ fm_l fm_r) key_to_find
  = if le key_to_find key
    then lookupFM' le fm_l key_to_find
    else if key_to_find==key
         then Just elt
         else lookupFM' le fm_r key_to_find


--- Retrieves element bound to given key.
--- If the element is not contained in map, return 
--- default value.
lookupWithDefaultFM :: FM key elt -> elt -> key -> elt
lookupWithDefaultFM fm deflt key
  = case lookupFM fm key of
      Nothing -> deflt
      Just elt -> elt

--- Retrieves the ordering on which the given finite map is built.
keyOrder :: FM key _ -> (key->key->Bool)
keyOrder (FM lt _) = lt

--- Retrieves the smallest key/element pair in the finite map 
--- according to the basic key ordering.
minFM :: FM a b -> Maybe (a,b)
minFM = min . tree
  where
   min EmptyFM            = Nothing
   min (Branch k x _ l _) | l==EmptyFM = Just (k,x)
                          | otherwise  = min l

--- Retrieves the greatest key/element pair in the finite map 
--- according to the basic key ordering.
maxFM :: FM a b -> Maybe (a,b)
maxFM = max . tree
  where
    max EmptyFM            = Nothing
    max (Branch k x _ _ r) | r==EmptyFM = Just (k,x)
                           | otherwise  = max r



----------------------------------------------------
-- LISTIFYING: transform finite maps to lists
----------------------------------------------------

--- Builds a list of key/element pairs. The list is ordered 
--- by the initially given irreflexive order predicate on keys.
fmToList        :: FM key elt -> [(key,elt)]
fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm

--- Retrieves a list of keys contained in finite map. 
--- The list is ordered 
--- by the initially given irreflexive order predicate on keys.
keysFM                :: FM key _ -> [key]
keysFM fm   = foldFM (\ key _   rest -> key : rest)       [] fm

--- Retrieves a list of elements contained in finite map. 
--- The list is ordered 
--- by the initially given irreflexive order predicate on keys.
eltsFM                :: FM _ elt -> [elt]
eltsFM fm   = foldFM (\ _   elt rest -> elt : rest)       [] fm

--- Retrieves list of key/element pairs in preorder of the internal tree.
--- Useful for lists that will be retransformed into a tree or to match 
--- any elements regardless of basic order.

fmToListPreOrder :: FM key elt -> [(key,elt)]
fmToListPreOrder (FM _ fm) = pre fm []
   where
     pre EmptyFM xs = xs
     pre (Branch k x _ l r) xs = (k,x):pre l (pre r xs)

--- Sorts a given list by inserting and retrieving from finite map.
--- Duplicates are deleted.
fmSortBy :: LeKey key -> [key] -> [key]
fmSortBy p l = keysFM (listToFM p (zip l (repeat ())))

-----------------------------------------------------
-- internal Implementation
-----------------------------------------------------

data FM key elt = FM (LeKey key) (FiniteMap key elt)

tree (FM _ fm) = fm


data FiniteMap key elt
  = EmptyFM
  | Branch key elt             -- Key and elt stored here
    Int{-STRICT-}              -- Size >= 1
    (FiniteMap key elt)        -- Children
    (FiniteMap key elt)

toGT le x y = not (le x y) && x/=y

isEmptyFM' fm = sizeFM' fm == 0

-------------------------------------------------------------------------
--                                                                        -
--  The implementation of balancing                                     -
--                                                                        -
-------------------------------------------------------------------------
-------------------------------------------------------------------------
--                                                                        -
--  Basic construction of a FiniteMap                                   -
--                                                                        -
-------------------------------------------------------------------------
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: Int
         -> key -> elt
         -> FiniteMap key elt -> FiniteMap key elt
         -> FiniteMap key elt

mkBranch _{-which-} key elt fm_l fm_r =
    let result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
      result
      --    if sizeFM result <= 8 then
      --     result
      --    else
      --      pprTrace ("mkBranch:"++(show which)) (ppr result) (
      --      result
      --      )
  where
    {-left_ok  = case fm_l of
                 EmptyFM                         -> True
                 Branch _ _ _ _ _  -> cmpWithBiggest_left_key key

    cmpWithBiggest_left_key key' = le (fst (findMax fm_l)) key'

    right_ok = case fm_r of
                 EmptyFM                         -> True
                 Branch _ _ _ _ _ -> cmpWithSmallest_right_key key

    cmpWithSmallest_right_key key' = le key' (fst (findMin fm_r))

    balance_ok = True -- sigh-}
    left_size  = sizeFM' fm_l
    right_size = sizeFM' fm_r


    unbox :: Int -> Int
    unbox x = x


-------------------------------------------------------------------------
--                                                                        -
-- Balanced construction of a FiniteMap                                 -
--                                                                        -
-------------------------------------------------------------------------
mkBalBranch :: key -> elt
            -> FiniteMap key elt -> FiniteMap key elt
            -> FiniteMap key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l        -- Right tree too big
  = case fm_R of
        Branch _ _ _ fm_rl fm_rr ->
              if sizeFM' fm_rl < 2 * sizeFM' fm_rr
                then single_L fm_L fm_R
                else double_L fm_L fm_R
        -- Other case impossible

  | size_l > sIZE_RATIO * size_r        -- Left tree too big
  = case fm_L of
        Branch _ _ _ fm_ll fm_lr ->
              if sizeFM' fm_lr < 2 * sizeFM' fm_ll
                then single_R fm_L fm_R
                else double_R fm_L fm_R
        -- Other case impossible

  | otherwise                                -- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = sizeFM' fm_L
    size_r   = sizeFM' fm_R

    single_L fm_l (Branch key_r elt_r _ fm_rl fm_rr)
        = mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr

    double_L fm_l (Branch key_r elt_r _ (Branch key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
        = mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
                                 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)

    single_R (Branch key_l elt_l _ fm_ll fm_lr) fm_r
        = mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)

    double_R (Branch key_l elt_l _ fm_ll (Branch key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
        = mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
                                 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)


mkVBalBranch :: (LeKey key)
             -> key -> elt
             -> FiniteMap key elt -> FiniteMap key elt
             -> FiniteMap key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--           (a) all keys in l are < all keys in r
--           (b) all keys in l are < key
--           (c) all keys in r are > key

mkVBalBranch le key elt EmptyFM fm_r = addToFM' le fm_r key elt
mkVBalBranch le key elt (Branch key_l elt_l s_l fm_ll fm_lr) EmptyFM = 
   addToFM' le (Branch key_l elt_l s_l fm_ll fm_lr) key elt
  
mkVBalBranch le key elt (Branch key_l elt_l s_l fm_ll fm_lr)
                        (Branch key_r elt_r s_r fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch le key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch le key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    fm_l = Branch key_l elt_l s_l fm_ll fm_lr
    fm_r = Branch key_r elt_r s_r fm_rl fm_rr
    size_l = sizeFM' fm_l
    size_r = sizeFM' fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Gluing two trees together                                            -
--                                                                        -
-------------------------------------------------------------------------
glueBal :: (LeKey key)
        -> FiniteMap key elt -> FiniteMap key elt
        -> FiniteMap key elt

glueBal le fm1 fm2 = 
  if isEmptyFM' fm1
    then fm2
    else if isEmptyFM' fm2
           then fm1
           else 
        -- The case analysis here (absent in Adams' program) is really to deal
        -- with the case where fm2 is a singleton. Then deleting the minimum means
        -- we pass an empty tree to mkBalBranch, which breaks its invariant.
             let (mid_key1, mid_elt1) = findMax fm1
                 (mid_key2, mid_elt2) = findMin fm2
             in
             if sizeFM' fm2 > sizeFM' fm1
               then mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin le fm2)
               else mkBalBranch mid_key1 mid_elt1 (deleteMax le fm1) fm2

glueVBal :: (LeKey key)
         -> FiniteMap key elt -> FiniteMap key elt
         -> FiniteMap key elt

glueVBal le fm_l fm_r = 
  if isEmptyFM' fm_l
    then fm_r
    else if isEmptyFM' fm_r
           then fm_l
           else 
             let Branch key_l elt_l _ fm_ll fm_lr = fm_l
                 Branch key_r elt_r _ fm_rl fm_rr = fm_r
                 --(mid_key_l,mid_elt_l) = findMax fm_l
                 --(mid_key_r,mid_elt_r) = findMin fm_r
                 size_l = sizeFM' fm_l
                 size_r = sizeFM' fm_r
             in
               if sIZE_RATIO * size_l < size_r
               then
                 mkBalBranch key_r elt_r (glueVBal le fm_l fm_rl) fm_rr
                else if sIZE_RATIO * size_r < size_l
                    then
                      mkBalBranch key_l elt_l fm_ll (glueVBal le fm_lr fm_r)

                      -- We now need the same two cases as in glueBal above.
                    else glueBal le fm_l fm_r
  
-------------------------------------------------------------------------
--                                                                        -
-- Local utilities                                                      -
--                                                                        -
-------------------------------------------------------------------------

splitLT, splitGT :: (LeKey key) -> FiniteMap key elt -> key
                    -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT _  EmptyFM _ = EmptyFM
splitLT le (Branch key elt _ fm_l fm_r) split_key
  = if le split_key key
    then splitLT le fm_l split_key
    else if split_key == key
         then fm_l
         else mkVBalBranch le key elt fm_l (splitLT le fm_r split_key)

splitGT _  EmptyFM _ = EmptyFM
splitGT le (Branch key elt _ fm_l fm_r) split_key
  = if le split_key key
    then mkVBalBranch le key elt (splitGT le fm_l split_key) fm_r
    else if split_key == key
         then fm_r
         else splitGT le fm_r split_key

findMin :: FiniteMap key elt -> (key,elt)
findMin (Branch key elt _ EmptyFM _) = (key,elt)
findMin (Branch _   _   _ (Branch key_l elt_l s_l fm_ll fm_lr)_) =
      findMin (Branch key_l elt_l s_l fm_ll fm_lr)

deleteMin :: (LeKey key) -> FiniteMap key elt -> FiniteMap key elt
deleteMin _  (Branch _   _   _ EmptyFM fm_r) = fm_r
deleteMin le (Branch key elt _ (Branch key_l elt_l s_l fm_ll fm_lr) fm_r) =
  mkBalBranch key elt (deleteMin le (Branch key_l elt_l s_l fm_ll fm_lr))
                         fm_r

findMax :: FiniteMap key elt -> (key,elt)
findMax (Branch key elt _ _ EmptyFM) = (key,elt)
findMax (Branch _   _   _ _  (Branch key_r elt_r s_r fm_rl fm_rr)) =
  findMax (Branch key_r elt_r s_r fm_rl fm_rr)

deleteMax :: (LeKey key) -> FiniteMap key elt -> FiniteMap key elt
deleteMax _  (Branch _   _   _ fm_l EmptyFM) = fm_l
deleteMax le (Branch key elt _ fm_l (Branch key_r elt_r s_r fm_rl fm_rr)) =
  mkBalBranch key elt fm_l
              (deleteMax le (Branch key_r elt_r s_r fm_rl fm_rr))



-------------------------------------------------------------------------
--                                                                      -
--   FiniteSets---a thin veneer                                         -
--                                                                      -
-------------------------------------------------------------------------
type FiniteSet key = FM key ()
emptySet         :: (LeKey key) -> FiniteSet key
mkSet            :: (LeKey key) -> [key] -> FiniteSet key
isEmptySet       :: FiniteSet _ -> Bool
elementOf        :: key -> FiniteSet key -> Bool
minusSet         :: FiniteSet key -> FiniteSet key -> FiniteSet key
setToList        :: FiniteSet key -> [key]
union            :: FiniteSet key -> FiniteSet key -> FiniteSet key

emptySet = emptyFM
mkSet le xs = listToFM le [ (x, ()) | x <- xs]
isEmptySet = isEmptyFM
elementOf = elemFM
minusSet  = minusFM
setToList = keysFM
union = plusFM


