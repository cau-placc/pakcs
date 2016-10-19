------------------------------------------------------------------------------
--- Some tests for library Database
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "currycheck testDatabase"
--- 
--- @author Michael Hanus
--- @version June 2007
------------------------------------------------------------------------------

import Database
import Dynamic(Dynamic)
import System(system)
import Test.EasyCheck

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
test_p_add_delete = (runT (prm |>> pa |>> pa |>> pr) >> getP)
                    `returns` [1]

test_q_add_delete = (runT (qrm |>> qa |>> qa |>> qr) >> getQ)
                    `returns` [1]


-- manipulating more data:
addTo pred n = mapT_ (addDB . pred) [1..n]

deleteN pred n = mapT_ (\_->deleteFirst pred) [1..n]

test_manipulate_many_p =
         (runT (prm |>> addTo p 100 |>> deleteN p 50 |>> addTo p 100 |>>
                        deleteN p 50) >> getP) `returns` [1..100]

test_manipulate_many_q =
         (runT (qrm |>> addTo q 100 |>> deleteN q 50 |>> addTo q 100 |>>
                deleteN q 50) >> getQ) `returns` [1..100]

-- test of aborting transactions:
abortWith trans = runT $
  addDB (q 42) |>>
  trans        |>>
  addDB (q 43)

test_q_abort = (runT (qrm |>> qa) >> abortWith failed >> getQ)
               `returns` [1]

test_q_abort_with_exception =
                 (runT (qrm |>> qa) >>
                  abortWith (addDB (q (div 1 0))) >> getQ) `returns` [1]

-- test queries:
primePairs pred = do
  runT (deleteAll pred |>> addDB (pred 2 <> pred 3 <> pred 5 <> pred 7))
  runQ $ queryAll (\(x,y) -> pred x <> pred y |> x+2==y)

test_p_primepairs = (primePairs p) `returns` [(3,5),(5,7)]

test_q_primepairs = (primePairs q) `returns` [(3,5),(5,7)]


-- finalize:
testCleanUp = (system "rm -rf DB_test") `returns` 0
