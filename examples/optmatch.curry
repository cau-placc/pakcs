-- Example: leftmost pattern-matching is not needed reduction:

g 0 []    = 0
g _ (x:_) = x

h x = h x

-- the following goal does not terminate in Haskell,
-- but terminates in Curry (with optmatch option)
-- since the program is inductively sequential.
-- It terminates also in Haskell if the two rules for g are swapped

goal = g (h 0) [1]
