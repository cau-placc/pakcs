-------------------------------------------------------------------------------------
-- Computing magic series.
-- A series [a_0,a_1,....,a_(n-1)] is called magic iff there are s_i occurrences
-- of i in this series, for all i=1,...,n-1
--
-- Adapted from an example of the TOY(FD) distribution.

import CLPFD

magic :: Int -> [Int]
magic n | take n (generateFD n) =:= l &
          constrain l l 0 cs & sum l (=#) n & scalarProduct cs l (=#) n &
          labeling [FirstFail] l
        = l
  where l,cs free

generateFD n | n ># 0 & domain [x] 0 (n-1) = x : generateFD n  where x free

constrain [] _ _ [] = True
constrain (x:xs) l i (j:s2) = i=:=j & count i l (=#) x & constrain xs l (i+1) s2


magicfrom :: Int -> [[Int]]
magicfrom n = magic n : magicfrom (n+1)

main = take 3 (magicfrom 7)
--> [[3,2,1,1,0,0,0],[4,2,1,0,1,0,0,0],[5,2,1,0,0,1,0,0,0]]
