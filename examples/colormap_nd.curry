-- graph coloring with non-deterministic functions
-- exploiting the demand-driven search due to lazy evaluation in Curry

-- This is our actual map:
--
-- --------------------------
-- |       |        |       |
-- |       |   L2   |       |
-- |       |        |       |
-- |  L1   |--------|  L4   |
-- |       |        |       |
-- |       |   L3   |       |
-- |       |        |       |
-- --------------------------
--

data Color = Red | Green | Yellow | Blue

aColor = Red
aColor = Yellow
aColor = Green
aColor = Blue

-- correct coloring:
correct l1 l2 l3 l4
   | l1/=l2 && l1/=l3 && l2/=l3 && l2/=l4 && l3/=l4
   = [l1,l2,l3,l4]

-- solution:
main = correct aColor aColor aColor aColor
