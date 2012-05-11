-- Compute the last element in a list using append:

append []     ys = ys
append (x:xs) ys = x : append xs ys


last xs | append ys [x] =:= xs
        = x  where x,ys free


goal = last [1,2,3,4]
