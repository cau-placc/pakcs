-- Lazy functional logic programming with infinite lists

data Nat = O | S Nat

first O     _      = []
first (S n) (x:xs) = x : first n xs

from n = n : from (S n)

goal1     = first (S (S O)) (from O)
goal2 x y = first x (from y) =:= [O]
