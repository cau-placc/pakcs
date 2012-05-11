-- Some examples for the use of COOSy:

import Observe

-- Observe the evaluation of an integer expression (x+y):

ex1 = let x=2
          y=5 in (observe oInt "Addition" (x+y))


-- Observe the evaluation of an infinite list (from 1):

ex2 = take 10 (observe (oList oInt) "enumFrom 1" [1..])


-- Observe the evaluation (i.e., instantiations) of a logical variable x:

ex3 = let x,y free in (observe (oList oInt) "List variable" x) ++ y =:= [1,2]

-- Observe the evaluation of a function (reverse):

ex4 = (observe (oList oInt ~> oList oInt) "reverse" reverse) [1,2,3]


-- Observe all evaluations of a function (reverse):

orev = observe (oList oInt ~> oList oInt) "all_reverse" reverse

ex5 = orev [1,2] ++ orev [3,4]


-- Observe all evaluations of a higher-order function (foldr):

ex6 = (observe ((oInt ~> oInt ~> oInt) ~> oInt ~> oList oInt ~> oInt)
               "foldr" foldr) (+) 0 [1..4]


ex7 = observe oString "ex7" ((chr 129):"k\fj\bh\rdf\n\t\\\"")

-- Observe the evaluation of a list while calculating its length:

ex8 = length (observe (oList oInt) "length" [1..500])

-- Nondeterminism

ex9 = let x=observe (oInt ~> oInt) "coin" coin 1 in
         x + x

coin x | x==x = 0
coin x | x==x = 1

-- Nondeterminism over functional values

coinF = f
coinF = g

f 1 = 1
g 1 = 2

ex10 = let f=(observe (oInt ~> oInt) "coinF" coinF) in
         f 1 + f 1

