--- Implementation of Arrays with Braun Trees. Conceptually, Braun trees
--- are always infinite. Consequently, there is no test on emptiness.
--- @authors {bbr, fhu}@informatik.uni-kiel.de

module Array 
  (Array,

   emptyErrorArray, emptyDefaultArray,
   listToDefaultArray,listToErrorArray,

   (//), update, applyAt,

   (!),

   combine, combineSimilar)

  where 


import Integer

infixl 9  !, //

data Array b = Array (Int -> b) (Entry b)

data Entry b = Entry b (Entry b) (Entry b) | Empty


--- Creates an empty array which generates errors for non-initialized
--- indexes.
emptyErrorArray :: Array b
emptyErrorArray = emptyDefaultArray errorArray 

errorArray :: Int -> _
errorArray idx =  error ("Array index "++show idx++" not initialized")

--- Creates an empty array, call given function for non-initialized
--- indexes.
--- @param default - function to call for each non-initialized index
emptyDefaultArray :: (Int -> b) -> Array b
emptyDefaultArray default = Array default Empty

--- Inserts a list of entries into an array.
--- @param array - array to modify
--- @param modifications - list of new (indexes,entries)
--- If an index in the list was already initialized, the old value
--- will be overwritten. Likewise the last entry with a given index
--- will be contained in the result array.
(//) :: Array b -> [(Int,b)] -> Array b
Array default array // modifications = 
  Array default 
    (foldr (\ (n,v) a -> at (default n) a n (const v)) array modifications)

--- Inserts a new entry into an array.
--- @param array - array to modify
--- @param idx - index of update
--- @param val - value to update at index idx
--- Entries already initialized will be overwritten.
update :: Array b -> Int -> b -> Array b
update (Array default a) i v = 
  Array default (at (default i) a i (const v))

--- Applies a function to an element.
--- @param array - array to modify
--- @param idx - index of update
--- @param fun - function to apply on element at index idx

applyAt :: Array b -> Int -> (b->b) -> Array b
applyAt (Array default a) n f = Array default (at (default n) a n f)


at :: b -> Entry b -> Int -> (b -> b) -> Entry b
at default Empty n f 
  | n==0      = Entry (f default) Empty Empty
  | odd n     = Entry default (at default Empty (n `div` 2) f) Empty
  | otherwise = Entry default Empty (at default Empty (n `div` 2 - 1) f) 
at default (Entry v al ar) n f
  | n==0      = Entry (f v) al ar
  | odd n     = Entry v (at default al (n `div` 2) f) ar
  | otherwise = Entry v al (at default ar (n `div` 2 - 1) f)


--- Yields the value at a given position.
--- @param a - array to look up in
--- @param n - index, where to look 
(!) :: Array b -> Int -> b
Array default array ! i = from (default i) array i

from :: a -> Entry a -> Int -> a
from default Empty _ = default
from default (Entry v al ar) n 
  | n==0      = v
  | odd n     = from default al (n `div` 2)
  | otherwise = from default ar (n `div` 2 - 1)


split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xys) = let (xs,ys) = split xys in
                    (x:xs,y:ys)

--- Creates a default array from a list of entries.
--- @param def - default funtion for non-initialized indexes
--- @param xs - list of entries
listToDefaultArray ::  (Int -> b) -> [b] -> Array b
listToDefaultArray def = Array def . listToArray

--- Creates an error array from a list of entries.
--- @param xs - list of entries
listToErrorArray :: [b] -> Array b
listToErrorArray = listToDefaultArray errorArray 


listToArray :: [b] -> Entry b
listToArray [] = Empty
listToArray (x:xs) = let (ys,zs) = split xs in
                       Entry x (listToArray ys)
                               (listToArray zs)


--- combine two arbitrary arrays

combine :: (a -> b -> c) -> Array a -> Array b -> Array c
combine f (Array def1 a1) (Array def2 a2) = 
  Array (\i -> f (def1 i) (def2 i)) (comb f def1 def2 a1 a2 0 1)

comb :: (a -> b -> c) -> (Int -> a) -> (Int -> b) 
     -> Entry a -> Entry b -> Int -> Int -> Entry c
comb _ _ _ Empty Empty _ _ = Empty
comb f def1 def2 (Entry x xl xr) Empty b o = 
  Entry (f x (def2 (b+o-1))) 
        (comb f def1 def2 xl Empty (2*b) o)
        (comb f def1 def2 xr Empty (2*b) (o+b))
comb f def1 def2 Empty (Entry y yl yr) b o = 
  Entry (f (def1 (b+o-1)) y) 
        (comb f def1 def2 Empty yl (2*b) o)
        (comb f def1 def2 Empty yr (2*b) (o+b))
comb f def1 def2 (Entry x xl xr) (Entry y yl yr) b o = 
  Entry (f x y) 
        (comb f def1 def2 xl yl (2*b) o)
        (comb f def1 def2 xr yr (2*b) (o+b))



--- the combination of two arrays with identical default function
--- and a combinator which is neutral in the default 
--- can be implemented much more efficient

combineSimilar :: (a -> a -> a) -> Array a -> Array a -> Array a
combineSimilar f (Array def a1) (Array _ a2) =  Array def (combSim f a1 a2)

combSim :: (a -> a -> a) -> Entry a -> Entry a -> Entry a
combSim _ Empty a2 = a2
combSim _ (Entry x y z) Empty = Entry x y z
combSim f (Entry x xl xr) (Entry y yl yr) = 
  Entry (f x y) (combSim f xl yl) (combSim f xr yr)





