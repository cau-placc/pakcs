-- An implementation of permutation sort using list comprehensions:

-- compute the list of all splittings of a list:
splits :: [a] -> [([a],[a])]
splits []     = [ ([],[]) ]
splits (y:ys) = ([],y:ys) : [ (y:ps,qs) | (ps,qs) <- splits ys ]

-- compute the list of all permutations of a list:
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [ ps++[x]++qs | rs <- perms xs, (ps,qs) <- splits rs ]

-- is a list sorted?
sorted []  = True
sorted [_] = True
sorted (x:y:ys) = x<=y && sorted (y:ys)

-- permutation sort:
sort :: [Int] -> [Int]
sort xs = head [ ys | ys <- perms xs, sorted ys ]


goal n = sort [n,n-1..1]
-- Result: [1..n]
