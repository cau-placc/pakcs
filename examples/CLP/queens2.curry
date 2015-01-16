import CLP.FD

-- solving the n-queens problem in Curry with FD constraints:

queens options n =
  let qs = take n (domain 1 n)
   in solve options qs (allSafe qs)

allSafe []     = true
allSafe (q:qs) = safe q qs 1 /\ allSafe qs

safe :: FDExpr -> [FDExpr] -> FDExpr -> FDConstr
safe _ []      _ = true
safe q (q1:qs) p = no_attack q q1 p /\ safe q qs (p+1)

no_attack q1 q2 p = q1 /=# q2 /\ q1 /=# q2+p /\ q1 /=# q2-p

-- queens [] 8
-- queens [FirstFail] 16
