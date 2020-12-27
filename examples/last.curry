-- Compute the last element in a list using append:

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys


last :: (Data a, Eq a) => [a] -> a
last xs | append _ [x] == xs
        = x  where x free
 
goal :: Int
goal = last [1,2,3,4]
