------------------------------------------------------------------------------
--- A collection of useful functions for sorting and comparing
--- characters, strings, and lists.
---
--- @author Michael Hanus
--- @version February 2004
------------------------------------------------------------------------------

module Sort(quickSort,mergeSort,
            cmpChar, cmpList, cmpString,
            leqChar, leqCharIgnoreCase,leqList,
            leqString,leqStringIgnoreCase,leqLexGerman) where

import Char

--- Quicksort.
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _   []     = []
quickSort leq (x:xs) = let (l,r) = split x xs
                        in quickSort leq l ++ (x:quickSort leq r)
 where
  split _ [] = ([],[])
  split e (y:ys) | leq y e   = (y:l,r)
                 | otherwise = (l,y:r)
              where (l,r) = split e ys


--- Bottom-up mergesort.
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort leq zs =  mergeLists (genRuns zs)
 where
  -- generate runs of length 2:
  genRuns []               =  []
  genRuns [x]              =  [[x]]
  genRuns (x1:x2:xs) | leq x1 x2 =  [x1,x2] : genRuns xs
                     | otherwise =  [x2,x1] : genRuns xs

  -- merge the runs:
  mergeLists []         =  []
  mergeLists [x]        =  x
  mergeLists (x1:x2:xs) =  mergeLists (merge leq x1 x2 : mergePairs xs)

  mergePairs []         =  []
  mergePairs [x]        =  [x]
  mergePairs (x1:x2:xs) =  merge leq x1 x2 : mergePairs xs


--- Merges two lists with respect to an ordering predicate.

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _   [] ys     = ys
merge _   (x:xs) [] = x : xs
merge leq (x:xs) (y:ys) | leq x y   = x : merge leq xs (y:ys)
                        | otherwise = y : merge leq (x:xs) ys


--------------------------------------------------------------------
-- Comparing lists, characters and strings

--- Less-or-equal on lists.
leqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
leqList _   []     _      = True
leqList _   (_:_)  []     = False
leqList leq (x:xs) (y:ys) | x == y    = leqList leq xs ys
                          | otherwise = leq x y

--- Comparison of lists.
cmpList :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
cmpList _   []     []     = EQ
cmpList _   []     (_:_)  = LT
cmpList _   (_:_)  []     = GT
cmpList cmp (x:xs) (y:ys) | cmp x y == EQ = cmpList cmp xs ys
                          | otherwise     = cmp x y

--- Less-or-equal on characters (deprecated, use 'Prelude.<=').
leqChar :: Char -> Char -> Bool
leqChar = (<=)

--- Comparison of characters (deprecated, use 'Prelude.compare').
cmpChar :: Char -> Char -> Ordering
cmpChar = compare

--- Less-or-equal on characters ignoring case considerations.
leqCharIgnoreCase :: Char -> Char -> Bool
leqCharIgnoreCase c1 c2 = (toUpper c1) <= (toUpper c2)

--- Less-or-equal on strings (deprecated, use 'Prelude.<=').
leqString :: String -> String -> Bool
leqString = (<=)

--- Comparison of strings (deprecated, use 'Prelude.compare').
cmpString :: String -> String -> Ordering
cmpString = compare

--- Less-or-equal on strings ignoring case considerations.
leqStringIgnoreCase :: String -> String -> Bool
leqStringIgnoreCase = leqList leqCharIgnoreCase

--- Lexicographical ordering on German strings.
--- Thus, upper/lowercase are not distinguished and Umlauts are sorted
--- as vocals.
leqLexGerman :: String -> String -> Bool
leqLexGerman []    _  = True
leqLexGerman (_:_) [] = False
leqLexGerman (x:xs) (y:ys) | x' == y'  = leqLexGerman xs ys
                           | otherwise = x' < y'
  where
    x' = glex (ord x)
    y' = glex (ord y)
    -- map umlauts to vocals and make everything lowercase:
    glex o | o >= ord 'A' && o <= ord 'Z'  =  o + (ord 'a' - ord 'A')
           | o == 228  = ord 'a'
           | o == 246  = ord 'o'
           | o == 252  = ord 'u'
           | o == 196  = ord 'a'
           | o == 214  = ord 'o'
           | o == 220  = ord 'u'
           | o == 223  = ord 's'
           | otherwise = o

-- end module Sort
