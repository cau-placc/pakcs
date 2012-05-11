-- choose is the basic non-deterministic choice function
choose x  _ = x
choose _  y = y

-- Non-deterministic insertion in a list
insert x []     = [x]
insert x (y:ys) = choose (x:y:ys) (y:insert x ys)

-- Non-deterministic generation of permutations
permut []     = []
permut (x:xs) = insert x (permut xs)

-- In the following definition, 'ys' (i.e. 'permut xs')  
-- is lazily generated, as much as the  filter 'sorted' demands it.
-- The filter may reject 'ys' without fully constructing it.

sort xs = rId sorted (permut xs)

-- restricted identity: rId p x is x if x satisfies (p x)=True
rId :: (a -> Bool) -> a -> a
rId p x | p x = x

-- this version of sort looks simpler but its behavior is not so
-- clear due to the interpretation of where (sharing or not sharing?)
wheresort xs | sorted ys = ys
                      where ys = permut xs


strictsort xs | ys =:= permut xs & sorted ys =:= True  = ys  where ys free

sorted []  = True
sorted [_] = True
sorted (x:y:ys) | x<=y = sorted (y:ys)

goal1 = sort [4,3,2,1]
goal2 = wheresort [4,3,2,1]
goal3 = strictsort [4,3,2,1]



data Nat = o | s Nat

coin = o
coin = s(o)

add o     n = n
add (s m) n = s(add m n)

double x = add x x


goal4 = double coin
