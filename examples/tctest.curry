-- This is a test of the type inferencer.
-- To type this program, the dependence graph must be correctly built.

f n = g n + h n

g n = j n + f n

h n = i n + g n

i n = n

j n = k n

k n = j n + l n

l _ = 5

-- "Goals" in an interactive Curry environment:
-- :t f       (should be  Int->Int)
-- :t i	      (should be  a->a)
-- :t k       (should be  a->Int)
