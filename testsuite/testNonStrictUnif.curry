-- Testing the built-in non-strict unifications =:<= and =:<<= that implement
-- function patterns

import Assertion

-- define three different versions of last:
last1 l | xs++[x] =:= l = x  where xs,x free

last2 l | xs++[x] =:<= l = x  where xs,x free

last3 l | xs++[x] =:<<= l = x  where xs,x free

inc x n = if n==0 then x else inc (x+1) (n-1)


-- and test them:
test1 = assertEqual  "last1ok" (last1 (map (inc 0) [1..200])) 200

test2 = assertEqual  "last2ok" (last2 (map (inc 0) [1..200])) 200

test3 = assertEqual  "last3ok" (last3 (map (inc 0) [1..200])) 200


test4 = assertValues "last1nonstrict" (last1 (take 10000 (repeat failed) ++ [1])) []

test5 = assertValues "last2nonstrict" (last2 (take 10000 (repeat failed) ++ [1])) [1]

test6 = assertValues "last3nonstrict" (last3 (take 10000 (repeat failed) ++ [1])) [1]

