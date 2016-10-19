----------------------------------------------------------------------------
--- Computing magic series.
--- A series [a_0,a_1,....,a_(n-1)] is called magic iff there are
--- a_i occurrences of i in this series, for all i=1,...,n-1
---
--- Adapted from an example of the TOY(FD) distribution.
----------------------------------------------------------------------------

import CLP.FD

-- Compute a magic series of length n:
magic :: Int -> [Int]
magic n =
 let vs = take n (domain 0 (n-1))  -- FD variables
     is = map fd (take n [0..])   -- FD constants: indices of elements
  in solveFD [FirstFail] vs $
       constrain vs vs is /\
       sum vs Equ (fd n) /\
       scalarProduct is vs Equ (fd n)

constrain :: [FDExpr] -> [FDExpr] -> [FDExpr] -> FDConstr
constrain []     _  _      = true
constrain (x:xs) vs (i:is) = count i vs Equ x /\ constrain xs vs is

magicfrom :: Int -> [[Int]]
magicfrom n = magic n : magicfrom (n+1)

main = take 3 (magicfrom 7)
--> [[3,2,1,1,0,0,0],[4,2,1,0,1,0,0,0],[5,2,1,0,0,1,0,0,0]]
