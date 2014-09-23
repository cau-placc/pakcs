------------------------------------------------------------------------------
--- Some tests for library Dynamic
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testDynamic"
--- 
--- @author Michael Hanus
--- @version August 2004
------------------------------------------------------------------------------

import Assertion -- for testing

import Dynamic
import AllSolutions
import System(system)

p :: Int -> Dynamic
p = dynamic

q :: Int -> Dynamic
q = persistent "file:DB_test"

-- get all p knowledge:
getP = getKnowledge >>= \known -> getAllSolutions (\x -> known (p x))

-- get all q knowledge:
getQ = getKnowledge >>= \known -> getAllSolutions (\x -> known (q x))

-- retract first fact of a unary predicate (if present):
retractFirst pred = do
  known <- getKnowledge
  mbx <- getOneSolution (\x -> known (pred x))
  maybe (return False) (\x -> retract (pred x)) mbx

-- remove complete knowledge about a unary predicate:
retractAll pred = do
  b <- retractFirst  pred
  if b then retractAll pred else done

prm = retractAll p

qrm = retractAll q

pa = assert (p 1)

pr = retract (p 1)

qa = assert (q 1)

qr = retract (q 1)

-- test of simple assert/retract:
test1 = assertIO "p assert/retract" (prm >> pa >> pa >> pr >> getP) [1]

test2 = assertIO "q assert/retract" (qrm >> qa >> qa >> qr >> getQ) [1]


-- test of independence of evaluation time:
assertGet pred =
  getKnowledge >>= \known1 ->
  assert (pred 3) >>
  assert (pred 5) >>
  getKnowledge >>= \known2 ->
  getAllSolutions (\x -> known1 (pred x)) >>= \sols1 ->
  getAllSolutions (\x -> known2 (pred x)) >>= \sols2 ->
  return (sols1,sols2)

test3 = assertIO "p eval time" (prm >> assertGet p) ([],[3,5])

test4 = assertIO "q eval time" (qrm >> assertGet q) ([],[3,5])


-- manipulating more data:
assertTo pred n = mapIO_ (\i->assert (pred i)) [1..n]

retractN pred n = mapIO_ (\_->retractFirst pred) [1..n]

test5 = assertIO "manipulate many p"
         (prm >> assertTo p 100 >> retractN p 50 >> assertTo p 100 >>
                 retractN p 50 >> getP) [1..100]

test6 = assertIO "manipulate many q"
         (qrm >> assertTo q 100 >> retractN q 50 >> assertTo q 100 >>
                 retractN q 50 >> getQ) [1..100]

-- test of aborting transactions:
abortWith act = transaction $ do
  assert (q 42)
  act
  abortTransaction
  assert (q 43)

test7 = assertIO "q abort" (qrm >> qa >> abortWith done >> getQ) [1]

test8 = assertIO "q abort with exception"
                 (qrm >> qa >> abortWith (assert (q (div 1 0))) >> getQ) [1]

-- test queries:
primePairs pred = do
  assert (pred 2 <> pred 3 <> pred 5 <> pred 7)
  getDynamicSolutions (\(x,y) -> pred x <> pred y |> x+2==y)

test9  = assertIO "p prime pairs" (prm >> primePairs p) [(3,5),(5,7)]

test10 = assertIO "q prime pairs" (qrm >> primePairs q) [(3,5),(5,7)]


-- finalize:
testFinal = assertIO "clean up" (system "rm -rf DB_test") 0

