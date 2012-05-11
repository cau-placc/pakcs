import Assertion
import RandomTest
import Dequeue
import List

deq f = f . listToDeq

deqs f = deqToList . f . listToDeq

test1 = eq "head" (deq deqHead) head

test2 = eq "last" (deq deqLast) last

test3 = eq "cons" (deqs (cons 73)) (73:)

test4 = eq "tail" (deqs (deqTail)) tail

test5 = eq "snoc" (deqs (snoc 73)) (++[73])

test6 = eq "init" (deqs deqInit) init
 where
  init [x] = []
  init (x:y:ys) = x : init (y:ys)

test7 = eq "reverse" (deqs deqReverse) reverse

test8 = eq "length" (deq deqLength) length

test9 = eq "rotate" (deqs rotate) (\ (x:xs) -> xs ++ [x])


