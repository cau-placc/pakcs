-- Testing non-deterministic features:

import Test.EasyCheck

coin = 0 ? 1

coinYields0    = coin ~> 0
coinYields1    = coin ~> 1
testCoinValues = coin <~> (1 ? 0)


double x = x+x

addCoinCoin      = (coin+coin) <~> (0 ? 1 ? 2)

addCoinCoinMulti = (coin+coin) <~~> (0 ? 1 ? 1 ? 2)

doubleCoin       = (double coin) <~> (0 ? 2)


-- Non-deterministic permutation sort:
insert x []     = [x]
insert x (y:ys) = (x:y:ys) ? (y:insert x ys)

permut []     = []
permut (x:xs) = insert x (permut xs)

sorted :: [Int] -> Bool
sorted []  = True
sorted [_] = True
sorted (x:y:ys) | x<=y = sorted (y:ys)

psort :: [Int] -> [Int]
psort xs | sorted ys = ys   where ys = permut xs

testSort1234 = (psort [4,1,3,2]) <~> [1,2,3,4]

psortKeepsLength xs = length (psort xs) <~> length xs
