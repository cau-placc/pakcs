------------------------------------------------------------------------------
--- Library for accessing and storing data in databases.
--- The contents of a database is represented in this library
--- as dynamic predicates that are defined by facts than can change over
--- time and can be persistently stored.
--- All functions in this library distinguishes between <em>queries</em> that
--- access the database and <em>transactions</em> that manipulates data
--- in the database. Transactions have a monadic structure.
--- Both queries and transactions can be executed as I/O actions.
--- However, arbitrary I/O actions cannot be embedded in transactions.
---
--- A dynamic predicate <code>p</code> with arguments of type
--- <code>t1,...,tn</code> must be declared by:
--- 
--- <code>p :: t1 -> ... -> tn -> Dynamic</code><br/>
--- <code>p = dynamic</code>
---
---
--- A dynamic predicate where all facts should be persistently stored
--- in the directory <code>DIR</code> must be declared by:
---
--- <code>p :: t1 -> ... -> tn -> Dynamic</code><br/>
--- <code>p = persistent "file:DIR"</code>
---
--- @author Michael Hanus
--- @version August 2011
------------------------------------------------------------------------------

module Database(Dynamic,dynamic,persistent,(<>),(|&>),(|>),
                Query,queryOne,queryAll,queryOneWithDefault,queryJustOne,
                dynamicExists,transformQ,runQ,
                Transaction,TError(..),TErrorKind(..),showTError,
                addDB,deleteDB,getDB,
                (|>>),(|>>=),returnT,doneT,errorT,failT,
                sequenceT,sequenceT_,mapT,mapT_,runT,runJustT,runTNA)
 where

import Dynamic
import Global -- to store transaction errors

infixl 1 |>>, |>>=

------------------------------------------------------------------------------
-- Database queries:

--- Abstract datatype to represent database queries.
data Query a = QueryDB (IO a)

--- A database query that returns all answers to an abstraction on a
--- dynamic expression.
queryAll :: (a -> Dynamic) -> Query [a]
queryAll dynq = QueryDB (getDynamicSolutions dynq)

--- A database query that returns a single answer to an abstraction on a
--- dynamic expression. It returns Nothing if no answer exists.
queryOne :: (a -> Dynamic) -> Query (Maybe a)
queryOne dynq = QueryDB (getDynamicSolution dynq)

--- A database query that returns a single answer to an abstraction on a
--- dynamic expression. It returns the first argument if no answer exists.
queryOneWithDefault :: a -> (a -> Dynamic) -> Query a
queryOneWithDefault d dynq = transformQ (maybe d id) (queryOne dynq)

--- A database query that returns a single answer to an abstraction on a
--- dynamic expression. It fails if no answer exists.
queryJustOne :: (a -> Dynamic) -> Query a
queryJustOne = queryOneWithDefault failed

--- A database query that returns True if there exists the argument facts
--- (without free variables!) and False, otherwise.
dynamicExists :: Dynamic -> Query Bool
dynamicExists dyn = QueryDB (isKnown dyn)

--- Transforms a database query from one result type to another
--- according to a given mapping.
transformQ :: (a -> b) -> Query a -> Query b
transformQ f (QueryDB a) = QueryDB (a >>= return . f)

--- Executes a database query on the current state of dynamic predicates.
--- If other processes made changes to persistent predicates,
--- these changes are read and made visible to the currently running program.
runQ :: Query a -> IO a
runQ (QueryDB q) = q


------------------------------------------------------------------------------
-- Transactions:

--- The type of errors that might occur during a transaction.
data TError = TError TErrorKind String

--- The various kinds of transaction errors.
data TErrorKind = KeyNotExistsError
                | NoRelationshipError
                | DuplicateKeyError
                | KeyRequiredError
                | UniqueError
                | MinError
                | MaxError
                | UserDefinedError
                | ExecutionError

--- Transforms a transaction error into a string.
showTError :: TError -> String
showTError (TError k s) = "Transaction error " ++ show k ++ ": " ++ s

--- Abstract datatype for representing transactions.
data Transaction a = TransDB (IO (TransResult a))

-- Internal type for representing the result of a transaction.
data TransResult a = OK a
                   | Error TError

--- Adds new facts (without free variables!) about dynamic predicates.
--- Conditional dynamics are added only if the condition holds.
addDB :: Dynamic -> Transaction ()
addDB dyn = TransDB (Dynamic.assert dyn >> return (OK ()))

--- Deletes facts (without free variables!) about dynamic predicates.
--- Conditional dynamics are deleted only if the condition holds.
deleteDB :: Dynamic -> Transaction ()
deleteDB dyn = TransDB (Dynamic.retract dyn >> return (OK ()))

--- Returns the result of a database query in a transaction.
getDB :: Query a -> Transaction a
getDB (QueryDB q) = TransDB (q >>= \qresult -> return (OK qresult))

--- The empty transaction that directly returns its argument.
returnT :: a -> Transaction a
returnT x = TransDB (return (OK x))

--- The empty transaction that returns nothing.
doneT :: Transaction ()
doneT = returnT ()

--- Abort a transaction with a specific transaction error.
errorT :: TError -> Transaction _
errorT e = TransDB (return (Error e))

--- Abort a transaction with a general error message.
failT :: String -> Transaction _
failT s = errorT (TError UserDefinedError s)

--- Sequential composition of transactions.
--- @param a - a transaction
--- @param fa - a function from a value into a transaction
--- @return a transaction that first performs <code>a</code>
---         (yielding result <code>r</code>)
---         and then performs <code>(fa r)</code>
(|>>=) :: Transaction a -> (a -> Transaction b) -> Transaction b
(TransDB t1) |>>= ft = TransDB $ do
  r1 <- t1
  case r1 of
    Error e    -> return (Error e)
    OK t1value -> let TransDB t2 = ft t1value in t2

--- Sequential composition of transactions.
--- @param a1 - a transaction
--- @param a2 - a transaction
--- @return a transaction that first performs a1 and then a2
(|>>) :: Transaction _ -> Transaction a -> Transaction a
t1 |>> t2 = t1 |>>= \_ -> t2

--- Executes a sequence of transactions and collects all results in a list.
sequenceT :: [Transaction a] -> Transaction [a]
sequenceT [] = returnT []
sequenceT (t:ts) = t |>>= \x -> sequenceT ts |>>= \xs -> returnT (x:xs)

--- Executes a sequence of transactions and ignores the results.
sequenceT_ :: [Transaction _] -> Transaction ()
sequenceT_ = foldr (|>>) doneT

--- Maps a transaction function on a list of elements.
--- The results of all transactions are collected in a list.
mapT :: (a -> Transaction b) -> [a] -> Transaction [b]
mapT f = sequenceT . map f

--- Maps a transaction function on a list of elements.
--- The results of all transactions are ignored.
mapT_ :: (a -> Transaction _) -> [a] -> Transaction ()
mapT_ f = sequenceT_ . map f

--- Executes a possibly composed transaction on the current state
--- of dynamic predicates as a single transaction.
---
--- Before the transaction is executed, the access to all persistent
--- predicates is locked (i.e., no other process can perform a
--- transaction in parallel).
--- After the successful transaction, the access is unlocked so that
--- the updates performed in this transaction become persistent and
--- visible to other processes.
--- Otherwise (i.e., in case of a failure or abort of the transaction),
--- the changes of the transaction to persistent predicates are
--- ignored and Nothing is returned.
---
--- In general, a transaction should terminate and all failures inside
--- a transaction should be handled (execept for an explicit <code>failT</code>
--- that leads to an abort of the transaction).
--- If a transaction is externally interrupted (e.g., by killing the process),
--- some locks might never be removed. However, they
--- can be explicitly removed by deleting the corresponding lock files
--- reported at startup time.
runT :: Transaction a -> IO (Either a TError)
runT t = do
  writeGlobal currentTransError execError
  etr <- Dynamic.transactionWithErrorCatch
           (let TransDB trans = t in trans >>= \tresult ->
            case tresult of
              OK    _ -> return tresult
              Error e -> writeGlobal currentTransError e >> failed)
  either (return . transformResult)
         (\re -> readGlobal currentTransError >>= \e ->
                 return (Right (if e==execError
                                then TError ExecutionError (showError re)
                                else e)))
         etr
 where
  transformResult (OK x)    = Left x
  transformResult (Error e) = Right e

--- Executes a possibly composed transaction on the current state
--- of dynamic predicates as a single transaction.
--- Similarly to <code>runT</code> but a run-time error is raised
--- if the execution of the transaction fails.
runJustT :: Transaction a -> IO a
runJustT t =
  runT t  >>=
  return . either id
              (\e -> error ("Transaction failed: " ++ showTError e))

--- Executes a possibly composed transaction as a Non-Atomic(!)
--- sequence of its individual database updates.
--- Thus, the argument is not executed as a single transaction
--- in contrast to <code>runT</code>, i.e., no predicates are
--- locked and individual updates are not undone in case of a
--- transaction error.
--- This operation could be applied to execute a composed transaction
--- without the overhead caused by (the current implementation of)
--- transactions if one is sure that locking is not necessary
--- (e.g., if the transaction contains only database reads and
--- transaction error raising).
runTNA :: Transaction a -> IO (Either a TError)
runTNA t = do
  writeGlobal currentTransError execError
  etr <- safeExecIO (let TransDB trans = t in trans >>= \tresult ->
                       case tresult of
                         OK    _ -> return tresult
                         Error e -> writeGlobal currentTransError e >> failed)
  either (return . transformResult)
         (\re -> readGlobal currentTransError >>= \e ->
                 return (Right (if e==execError
                                then TError ExecutionError (showError re)
                                else e)))
         etr
 where
  transformResult (OK x)    = Left x
  transformResult (Error e) = Right e

  safeExecIO :: IO a -> IO (Either a IOError)
  safeExecIO action = catch (action >>= return . Left) (return . Right)

-- Global entity to store the transaction error during the execution of
-- the transaction.
currentTransError :: Global TError
currentTransError = global execError Temporary

execError = TError ExecutionError "run-time error during transaction execution"
