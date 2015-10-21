-- constraint solving (simple generate and test) in Curry:
-- graph coloring

{-
 This is our actual map:

 --------------------------
 |       |        |       |
 |       |   L2   |       |
 |       |        |       |
 |  L1   |--------|  L4   |
 |       |        |       |
 |       |   L3   |       |
 |       |        |       |
 --------------------------
-}

data Color = Red | Green | Yellow | Blue


isColor :: Color -> Bool
isColor Red    = True
isColor Yellow = True
isColor Green  = True
isColor Blue   = True


coloring :: Color -> Color -> Color -> Color -> Bool
coloring l1 l2 l3 l4 = isColor l1 & isColor l2 & isColor l3 & isColor l4


-- correct coloring:
correct :: Color -> Color -> Color -> Color -> Bool
correct l1 l2 l3 l4 = l1 /= l2 & l1 /= l3 & l2 /= l3 & l2 /= l4 & l3 /= l4


-- generate+test solution:
goal l1 l2 l3 l4 = coloring l1 l2 l3 l4 & correct l1 l2 l3 l4


