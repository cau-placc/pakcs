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

-- goals:
-- increment list elements:
g1 :: [Int]
g1 = map inc [0,2,1]

g2 :: [Int]
g2 = map (+ 1) [0,2,1]

-- sum of all list elements:
g3 :: Int
g3 = foldr (+) 0 [1,0,2]

-- product of all list elements:
g4 :: Int
g4 = foldr (*) 1 [1,2,3,4,5]

g5 :: Int
g5 = foldr (\ x y -> x * y) 1 [1,2,3,4,5]
