-- quicksort using a split function (demonstrating where/let declarations):

split _ [] = ([],[])
split e (x:xs) | e>=x  = (x:l,r)
               | e<x   = (l,x:r)
               where (l,r) = split e xs


qsort []     = []
qsort (x:xs) = let (l,r) = split x xs
               in qsort l ++ (x:qsort r)

goal = qsort [8,6,7,5,4,2,3,1]

