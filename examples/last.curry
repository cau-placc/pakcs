-- Compute the last element in a list using append:

append []     ys = ys
append (x:xs) ys = x : append xs ys


last xs | append _ [x] == xs
        = x  where x free


goal = last [1,2,3,4]
