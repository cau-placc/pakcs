import CLP.FD

smm :: [Int]
smm =
 let xs@[s,e,n,d,m,o,r,y] = take 8 (domain 0 9)
  in solve [] xs $
        s ># 0 /\
        m ># 0 /\
        allDifferent xs /\
                       1000 * s + 100 * e + 10 * n + d
                     + 1000 * m + 100 * o + 10 * r + e
        =# 10000 * m + 1000 * o + 100 * n + 10 * e + y

-- ...and with good old constraint representation:
smmC :: [Int] -> Success
smmC xs = smm =:= xs

-- smmC [s,e,n,d,m,o,r,y]  where s,e,n,d,m,o,r,y free
