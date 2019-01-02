-- Testing higher-order features:

import Test.Prop

-- apply a function to all list elements (predefined as `map'):
map_ ::  (a->b) -> [a] -> [b]
map_ _ []      = []
map_ f (x:xs)  = f x : map_ f xs

-- accumulate all list elements (predefined as `foldr'):
foldr_ ::  (a->b->b) -> b -> [a] -> b
foldr_ _ z []     = z
foldr_ f z (h:t)  = f h (foldr_ f z t)

-- increment function:
inc x = x+1

-- and now the test cases:

testMapInc      = (map inc [0,2,1])                       -=- [1,3,2]
testMapPlus1    = (map (+ 1) [0,2,1])                     -=- [1,3,2]
testFoldrPlus   = (foldr (+) 0 [1,0,2])                   -=- 3
testFoldrMult   = (foldr (*) 1 [1,2,3,4,5])               -=- 120
testFoldrLambda =  (foldr (\ x y -> x * y) 1 [1,2,3,4,5]) -=- 120
