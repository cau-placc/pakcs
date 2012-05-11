------------------------------------------------------------------------------
--- Some tests for library Database
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testDatabase"
--- 
--- @author Michael Hanus
--- @version June 2007
------------------------------------------------------------------------------

import Assertion -- for testing

import Database
import System(system)

p :: Int -> Dynamic
p = dynamic

q :: Int -> Dynamic
q = persistent "file:DB_test"

-- get all p facts:
getP = runQ $ queryAll p

-- get all q facts:
getQ = runQ $ queryAll q

-- delete first fact of a unary predicate (if present):
deleteFirst pred = getDB (queryOne pred) |>>= maybe doneT (deleteDB . pred)

-- remove complete knowledge about a unary predicate:
deleteAll pred =
  getDB (queryOne pred) |>>=
  maybe doneT (\x -> deleteDB (pred x) |>> deleteAll pred)

prm = deleteAll p

qrm = deleteAll q

pa = addDB (p 1)

pr = deleteDB (p 1)

qa = addDB (q 1)

qr = deleteDB (q 1)

-- test of simple add/delete:
test1 = assertIO "p add/delete"
                 (runT (prm |>> pa |>> pa |>> pr) >> getP)
                 [1]

test2 = assertIO "q add/delete"
                 (runT (qrm |>> qa |>> qa |>> qr) >> getQ)
                 [1]


-- manipulating more data:
addTo pred n = mapT_ (addDB . pred) [1..n]

deleteN pred n = mapT_ (\_->deleteFirst pred) [1..n]

test5 = assertIO "manipulate many p"
         (runT (prm |>> addTo p 100 |>> deleteN p 50 |>> addTo p 100 |>>
                        deleteN p 50) >> getP) [1..100]

test6 = assertIO "manipulate many q"
         (runT (qrm |>> addTo q 100 |>> deleteN q 50 |>> addTo q 100 |>>
                deleteN q 50) >> getQ) [1..100]

-- test of aborting transactions:
abortWith trans = runT $
  addDB (q 42) |>>
  trans        |>>
  addDB (q 43)

test7 = assertIO "q abort"
                 (runT (qrm |>> qa) >> abortWith failed >> getQ)
                 [1]

test8 = assertIO "q abort with exception"
                 (runT (qrm |>> qa) >>
                  abortWith (addDB (q (div 1 0))) >> getQ) [1]

-- test queries:
primePairs pred = do
  runT (deleteAll pred |>> addDB (pred 2 <> pred 3 <> pred 5 <> pred 7))
  runQ $ queryAll (\(x,y) -> pred x <> pred y |> x+2==y)

test9  = assertIO "p prime pairs" (primePairs p) [(3,5),(5,7)]

test10 = assertIO "q prime pairs" (primePairs q) [(3,5),(5,7)]


-- finalize:
testFinal = assertIO "clean up" (system "rm -rf DB_test") 0
