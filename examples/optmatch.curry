-- Example: leftmost pattern-matching is not needed reduction:

g 0 []    = 0
g _ (x:_) = x

h x = h x

-- The following expression does not terminate in Haskell,
-- but terminates in Curry since the program is inductively sequential.
-- It terminates also in Haskell if the two rules for g are swapped.

main = g (h 0) [1]
