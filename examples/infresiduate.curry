-- Example for potential disadvantage of residuation [Hanus JLP'95]:
-- with residuation, the program has an infinite search space
-- with narrowing, the search space is finite!

conc xs ys = append xs ys                  --> finite search space
--conc xs ys = append (ensureSpine xs) ys  --> infinite search space

append []     ys = ys
append (x:xs) ys = x : append xs ys

rev [] []     = True
rev l  (x:xs) = let lx free in conc lx [x] =:= l & rev lx xs

goal l = rev [0] l

