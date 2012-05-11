------------------------------------------------------------------------------
--- Library with some useful operations on lists.
---
--- @author Michael Hanus
--- @version April 2011
------------------------------------------------------------------------------

module List(elemIndex,elemIndices,find,findIndex,findIndices,
            nub,nubBy,delete,deleteBy,(\\),union,intersect,
            intersperse,transpose,partition,
            group,groupBy,inits,tails,replace,
            isPrefixOf,isSuffixOf,isInfixOf,sortBy,insertBy,last)  where

import Maybe(listToMaybe)

infix 5 \\

--- Returns the index `i` of the first occurrence of an element in a list
--- as `(Just i)`, otherwise `Nothing` is returned.
elemIndex               :: a -> [a] -> Maybe Int
elemIndex x              = findIndex (x ==)

--- Returns the list of indices of occurrences of an element in a list.
elemIndices             :: a -> [a] -> [Int]
elemIndices x            = findIndices (x ==)
                        
--- Returns the first element `e` of a list satisfying a predicate as `(Just e)`,
--- otherwise `Nothing` is returned.
find                    :: (a -> Bool) -> [a] -> Maybe a
find p                   = listToMaybe . filter p

--- Returns the index `i` of the first occurrences of a list element
--- satisfying a predicate as `(Just i)`, otherwise `Nothing` is returned.
findIndex               :: (a -> Bool) -> [a] -> Maybe Int
findIndex p              = listToMaybe . findIndices p

--- Returns the list of indices of list elements satisfying a predicate.
findIndices             :: (a -> Bool) -> [a] -> [Int]
findIndices p xs         = [ i | (x,i) <- zip xs [0..], p x ]


--- Removes all duplicates in the argument list.
nub                   :: [a] -> [a]
nub xs                 = nubBy (==) xs

--- Removes all duplicates in the argument list according to an
--- equivalence relation.
nubBy                 :: (a -> a -> Bool) -> [a] -> [a]
nubBy _  []            = []
nubBy eq (x:xs)        = x : nubBy eq (filter (\y -> not (eq x y)) xs)

--- Deletes the first occurrence of an element in a list.
delete                :: a -> [a] -> [a]
delete                 = deleteBy (==)

--- Deletes the first occurrence of an element in a list
--- according to an equivalence relation.
deleteBy              :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []       = []
deleteBy eq x (y:ys)   = if eq x y then ys else y : deleteBy eq x ys

--- Computes the difference of two lists.
--- @param xs - a list
--- @param ys - a list
--- @return the list where the first occurrence of each element of
---         `ys` has been removed from `xs`
(\\) :: [a] -> [a] -> [a]
xs \\ ys = foldl (flip delete) xs ys

--- Computes the union of two lists.
union                 :: [a] -> [a] -> [a]
union []     ys        = ys
union (x:xs) ys        = if x `elem` ys then union xs ys
                                        else x : union xs ys

--- Computes the intersection of two lists.
intersect             :: [a] -> [a] -> [a]
intersect []     _     = []
intersect (x:xs) ys    = if x `elem` ys then x : intersect xs ys
                                        else intersect xs ys

--- Puts a separator element between all elements in a list.
---
--- Example: `(intersperse 9 [1,2,3,4]) = [1,9,2,9,3,9,4]`
intersperse               :: a -> [a] -> [a]
intersperse _   []         = []
intersperse _   [x]        = [x]
intersperse sep (x1:x2:xs) = x1 : sep : intersperse sep (x2:xs)

--- Transposes the rows and columns of the argument.
---
--- Example: `(transpose [[1,2,3],[4,5,6]]) = [[1,4],[2,5],[3,6]]`
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([] : xss)     = transpose xss
transpose ((x:xs) : xss) = (x : map head xss) : transpose (xs : map tail xss)

--- Partitions a list into a pair of lists where the first list
--- contains those elements that satisfy the predicate argument
--- and the second list contains the remaining arguments.
---
--- Example: `(partition (<4) [8,1,5,2,4,3]) = ([1,2,3],[8,5,4])`
partition       :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs  = foldr select ([],[]) xs
       where select x (ts,fs) = if p x then (x:ts,fs)
                                       else (ts,x:fs)

--- Splits the list argument into a list of lists of equal adjacent
--- elements.
---
--- Example: `(group [1,2,2,3,3,3,4]) = [[1],[2,2],[3,3,3],[4]]`
group :: [a] -> [[a]]
group = groupBy (==)

--- Splits the list argument into a list of lists of related adjacent
--- elements.
--- @param eq - the relation to classify adjacent elements
--- @param xs - the list of elements
--- @return the list of lists of related adjacent elements
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []     = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
                    where (ys,zs) = span (eq x) xs

--- Returns all initial segments of a list, starting with the shortest.
--- Example: `inits [1,2,3] == [[],[1],[1,2],[1,2,3]]`
--- @param xs - the list of elements
--- @return the list of initial segments of the argument list
inits :: [a] -> [[a]]
inits []     =  [[]]
inits (x:xs) =  [[]] ++ map (x:) (inits xs)

--- Returns all final segments of a list, starting with the longest.
--- Example: `tails [1,2,3] == [[1,2,3],[2,3],[3],[]]`
tails :: [a] -> [[a]]
tails []         =  [[]]
tails xxs@(_:xs) =  xxs : tails xs

--- Replaces an element in a list.
--- @param x - the new element
--- @param p - the position of the new element (head = 0)
--- @param ys - the old list
--- @return the new list where the `p`. element is replaced by `x`
replace :: a -> Int -> [a] -> [a]
replace _ _ [] = []
replace x p (y:ys) | p==0      = x:ys
                   | otherwise = y:(replace x (p-1) ys)

--- Checks whether a list is a prefix of another.
--- @param xs - a list
--- @param ys - a list
--- @return `True` if `xs` is a prefix of `ys`
isPrefixOf :: [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (_:_) [] = False
isPrefixOf (x:xs) (y:ys) = x==y && (isPrefixOf xs ys)

--- Checks whether a list is a suffix of another. 
--- @param xs - a list 
--- @param ys - a list 
--- @return `True` if `xs` is a suffix of `ys`
isSuffixOf :: [a] -> [a] -> Bool 
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

--- Checks whether a list is contained in another. 
--- @param xs - a list 
--- @param ys - a list 
--- @return True if xs is contained in ys 
isInfixOf :: [a] -> [a] -> Bool 
isInfixOf xs ys = any (isPrefixOf xs) (tails ys)

--- Sorts a list w.r.t. an ordering relation by the insertion method.
sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy le = foldr (insertBy le) []

--- Inserts an object into a list according to an ordering relation.
--- @param le - an ordering relation (e.g., less-or-equal)
--- @param x - an element
--- @param xs - a list
--- @return a list where the element has been inserted
insertBy :: (a -> a -> Bool) -> a -> [a] -> [a]
insertBy _ x []     = [x]
insertBy le x (y:ys) = if le x y 
                         then x : y : ys
                         else y : insertBy le x ys

--- Returns the last element of a non-empty list.
last :: [a] -> a
last [x] = x
last (_:x:xs) = last (x:xs)
