-- HORSEMAN: relation between men, horses and their heads and feet:

data Nat = O | S Nat

-- translate integers to nat-terms:
int2nat :: Int -> Nat
int2nat n = if n<=0 then O else S(int2nat (n-1))


-- addition on naturals:
add O     n = n
add (S m) n = S (add m n)


horseman m h heads feet =  
   heads =:= add m h  &  feet =:= add (add m m) (add (add h h) (add h h))


-- How many men and horses have 3 heads and 8 feet?
goal1 m h = horseman m h (S(S(S O))) (S(S(S(S(S(S(S(S O))))))))

-- How many men and horses have 8 heads and 20 feet?
goal2 m h = horseman m h (int2nat 8) (int2nat 20)

-- Relation between men and horses  feets with 2 heads:
goal3 m h hd = horseman m h (S (S O)) hd

