------------------------------------------------------------------------------
--- Some tests for library KeyDatabaseSQLite
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testKeyDatabaseSQLite"
--- 
--- @author Michael Hanus and Sebastian Fischer
--- @version December 2011
------------------------------------------------------------------------------

import IO
import KeyDatabaseSQLite
import List ( sortBy )
import System
import Test.EasyCheck

sort = sortBy (<=)

testPred :: Int -> (String,Int) -> Dynamic
testPred = persistentSQLite "test.db" "test" ["rowid","oid"]

testNotExists = (runQ $ existsDBKey testPred 0) `returns` False

testAllKeysEmpty = (runQ $ allDBKeys testPred) `returns` []

testAllInfosEmpty = (runQ $ allDBInfos testPred) `returns` []

testAllKeyInfosEmpty = (runQ $ allDBKeyInfos testPred) `returns` []

testInfoEmpty = (runQ $ getDBInfo testPred 0) `returns` Nothing

testInfosEmpty = (runQ $ getDBInfos testPred [0,1,2]) `returns` Nothing

testDeleteKeyEmpty = (runT $ deleteDBEntry testPred 0) `returns` (Left ())

testDeleteKeysEmpty =
  (runT $ deleteDBEntries testPred [0,1,2]) `returns` (Left ())

testUpdateEmpty =
 (runTWithError $ updateDBEntry testPred 0 ("",1))
 `returns` (Right KeyNotExistsError)

testCreatedExists =
    (runT $ (newDBEntry testPred ("new",42) |>>= getDB . existsDBKey testPred))
    `returns` (Left True)

testCreatedGoneAfterClean = runTWithError trans `returns` (Left Nothing)
 where
  trans = do
    key <- newDBEntry testPred ("new",42)
    cleanDB testPred
    getDB (getDBInfo testPred key)

testGetAllCreatedKeys = runT trans `returns` (Left True)
 where
  trans = do
    keys1 <- mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)]
    keys2 <- getDB (allDBKeys testPred)
    returnT (sameBag keys1 keys2)

testGetAllCreatedInfos =
    (runT (cleanDB testPred |>>
           mapT (newDBEntry testPred) infos1 |>>
           getDB (allDBInfos testPred) |>>= \infos2 ->
           returnT (sort infos2)))
    `returns` (Left infos1)
 where infos1 = [("a",10),("b",20),("c",30)]

testGetAllCreatedKeyInfos =
    (runT (cleanDB testPred |>>
           mapT (newDBEntry testPred) infos |>>= \keys ->
           let keyinfos1 = zip keys infos
            in getDB (allDBKeyInfos testPred) |>>= \keyinfos2 ->
               returnT (sameBag keyinfos1 keyinfos2)))
    `returns` (Left True)
 where infos = [("a",10),("b",20),("c",30)]

testGetCreatedInfo =
    (runT (cleanDB testPred |>>
           newDBEntry testPred ("new",42) |>>= getDB . getDBInfo testPred))
    `returns` (Left (Just ("new",42)))

testGetCreatedInfos =
    (runT (cleanDB testPred |>>
           mapT (newDBEntry testPred) infos |>>= \keys ->
           getDB (getDBInfos testPred keys)))
    `returns` (Left (Just infos))
 where infos = [("a",10),("b",20),("c",30)]

testDeleteOneCreated =
   (runT (cleanDB testPred |>>
          mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys ->
          deleteDBEntry testPred (keys!!1) |>>
          getDB (allDBInfos testPred) |>>= \infos ->
          returnT (sort infos)))
   `returns` (Left [("a",10),("c",30)])

testDeleteAllCreated =
   (runT (cleanDB testPred |>>
          mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys ->
          deleteDBEntries testPred keys |>>
          getDB (allDBKeys testPred)))
   `returns` (Left [])

testUpdateCreated =
   (runT (cleanDB testPred |>>
          newDBEntry testPred ("old",41) |>>= \key ->
          updateDBEntry testPred key ("new",42) |>>
          getDB (getDBInfo testPred key)))
   `returns` (Left (Just ("new",42)))

testQueryDeleted =
   (runT (cleanDB testPred |>>
          newDBEntry testPred ("new",42) |>>= \key ->
          deleteDBEntry testPred key |>>
          getDB (getDBInfo testPred key)))
   `returns` (Left Nothing)

testQueryListWithOneDeleted =
   (runT (cleanDB testPred |>>
          mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys ->
          deleteDBEntry testPred (keys!!1) |>>
          getDB (getDBInfos testPred keys)))
   `returns` (Left Nothing)

testRollbackOnError =
   (runT (cleanDB testPred) >>
    runTWithError (cleanDB testPred |>>
                   newDBEntry testPred ("new",42) |>> transError))
   `returns` (Right ExecutionError)

testEmptyAfterRollback = (runQ $ allDBKeys testPred) `returns` []

transError :: Transaction ()
transError = error "transaction error"

-- finalize:
testFinalCleanUp = (system "rm -f test.db") `returns` 0

-- Auxiliaries:

runTWithError trans =
  runT trans >>= return . either Left (\ (TError tkind _) -> Right tkind)

sameBag :: Ord a => [a] -> [a] -> Bool
sameBag xs ys = sort xs == sort ys

