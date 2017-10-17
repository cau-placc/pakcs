-- quicksort using a split function (demonstrating where/let declarations):

split :: Int -> [Int] -> ([Int],[Int])
split _ [] = ([],[])
split e (x:xs) | e>=x  = (x:l,r)
               | e<x   = (l,x:r)
               where (l,r) = split e xs

qsort :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = let (l,r) = split x xs
               in qsort l ++ (x:qsort r)

goal :: [Int]
goal = qsort [8,6,7,5,4,2,3,1]

