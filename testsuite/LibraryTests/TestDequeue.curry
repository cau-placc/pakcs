import RandomTest
import Dequeue
import List

deq f = f . listToDeq

deqs f = deqToList . f . listToDeq

testHead = eq (deq deqHead) head

testLast = eq (deq deqLast) last

testCons = eq (deqs (cons 73)) (73:)

testTail = eq (deqs (deqTail)) tail

testSnoc = eq (deqs (snoc 73)) (++[73])

testInit = eq (deqs deqInit) init
 where
  init [x] = []
  init (x:y:ys) = x : init (y:ys)

testReverse = eq (deqs deqReverse) reverse

testLength  = eq (deq deqLength) length

testRotate  = eq (deqs rotate) (\ (x:xs) -> xs ++ [x])


