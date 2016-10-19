import CLP.FD

-- solving the n-queens problem in Curry with FD constraints:

queens :: [Option] -> Int -> [Int]
queens options n =
  let qs = take n (domain 1 n)
   in solveFD options qs (allSafe qs)

allSafe :: [FDExpr] -> FDConstr
allSafe []     = true
allSafe (q:qs) = safe q qs (fd 1) /\ allSafe qs

safe :: FDExpr -> [FDExpr] -> FDExpr -> FDConstr
safe _ []      _ = true
safe q (q1:qs) p = no_attack q q1 p /\ safe q qs (p +# fd 1)

no_attack :: FDExpr -> FDExpr -> FDExpr -> FDConstr
no_attack q1 q2 p = q1 /=# q2 /\ q1 /=# q2+#p /\ q1 /=# q2-#p

-- queens [] 8
-- queens [FirstFail] 16
