-- Some examples for the use of the module AllSolutions

import AllSolutions
import Combinatorial
import Integer

-- The famous non-deterministic function:
coin :: Int
coin = 0
coin = 1

-- Principal use of getAllSolutions:
all1 = getAllSolutions (=:=(coin+coin)) >>= print

-- This example shows that no sharing is performed accress encapsulated search:
all2 = let cc = coin+coin in 
  getAllSolutions (=:=cc) >>= print >>
  getAllSolutions (=:=cc) >>= print

-- Example for getOneValue:
first1 = getOneValue (coin+coin) >>= print

-- Generate search tree of depth 0 (similar to getAllSolutions):
tree0 = getSearchTree [] (=:=(x+y)) >>= print
        where
          x=coin
          y=coin
--> (Solutions [0,1,1,2])

-- Generate search tree of depth 1:
tree1 = getSearchTree [x+5] (=:=(x+y)) >>= print
        where
          x=coin
          y=coin 
--> (SearchBranch [(5,(Solutions [0,1])),(6,(Solutions [1,2]))])

-- Generate search tree of depth 2:
tree2 = getSearchTree [x,y] (=:=(x+y=:=1)) >>= print
        where
          x=coin
          y=coin  
--> (SearchBranch [(0,(SearchBranch [(0,(Solutions [])),
--                                   (1,(Solutions [True]))])),
--                 (1,(SearchBranch [(0,(Solutions [True])),
--                                   (1,(Solutions []))]))])


-- An application of getAllFailures:
--
-- Place n queens on a chessboard so that no queen can capture another queen:
-- (this solution is due to Sergio Antoy)

queens n = getAllFailures (permute [1..n]) capture

capture y = let l1,l2,l3,y1,y2 free in
  l1 ++ [y1] ++ l2 ++ [y2] ++ l3 =:= y & abs (y1-y2) =:= length l2 + 1
