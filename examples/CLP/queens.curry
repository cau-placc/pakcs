import CLPFD

-- solving the n-queens problem in Curry with FD constraints:

queens options n qs =
       gen_vars n =:= qs &
       domain qs 1 n &
       allSafe qs &
       labeling options qs

allSafe [] = success
allSafe (q:qs) = safe q qs 1 & allSafe qs

safe :: Int -> [Int] -> Int -> Success
safe _ [] _ = success
safe q (q1:qs) p = no_attack q q1 p & safe q qs (p+#1)

no_attack q1 q2 p = q1 /=# q2 & q1 /=# q2+#p & q1 /=# q2-#p

gen_vars n = if n==0 then [] else var : gen_vars (n-1)  where var free

-- queens [] 8 qs  where qs free
-- queens [FirstFail] 16 qs  where qs free
