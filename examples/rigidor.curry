-- Example for suspending or-branches (similarly to POPL'97, Example 3.5)

f 0 _ = success
f _ 1 = success

g x = g' (ensureNotFree x)
 where g' 0 = 0

h 1 = 1

goal = let x free in f (g x) (h x) & x=:=0
