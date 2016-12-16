import CLP.FD

-- send more money puzzle in Curry with FD constraints:

smm :: [Int]
smm =
 let xs@[s,e,n,d,m,o,r,y] = take 8 (domain 0 9)
  in solveFD [] xs $
        s ># 0 /\
        m ># 0 /\
        allDifferent xs /\
                       1000 * s + 100 * e + 10 * n + d
                     + 1000 * m + 100 * o + 10 * r + e
        =# 10000 * m + 1000 * o + 100 * n + 10 * e + y

-- smm --> [9,5,6,7,1,0,8,2]
