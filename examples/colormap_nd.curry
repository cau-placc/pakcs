-- graph coloring with non-deterministic functions
-- exploiting the demand-driven search due to lazy evaluation in Curry


-- auxiliary function:

-- negation of ==:
diff x y = (x == y) =:= False

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
   | diff l1 l2 & diff l1 l3 & diff l2 l3 & diff l2 l4 & diff l3 l4
   = [l1,l2,l3,l4]

-- solution:
goal = correct aColor aColor aColor aColor
