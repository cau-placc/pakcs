------------------------------------------------------------------------------
--- Library for dynamic predicates.
--- <a href="http://www.informatik.uni-kiel.de/~mh/papers/JFLP04_dyn.html">
--- This paper</a> contains a description of the basic ideas
--- behind this library.
---
--- Currently, it is still experimental so that its interface might
--- be slightly changed in the future.
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
--- Remark:
--- This library has been revised to the library <code>Database</code>.
--- Thus, it might not be further supported in the future.
---
--- @author Michael Hanus
--- @version August 2011
------------------------------------------------------------------------------

module Dynamic(Dynamic,(<>),(|>),(|&>),dynamic,persistent,
               assert,retract,getKnowledge,
               getDynamicSolutions,getDynamicSolution,isKnown,
               transaction,transactionWithErrorCatch,abortTransaction) where

import AllSolutions

infixr 2 <>
infixl 1 |>, |&>

----------------------------------------------------------------------

--- The general type of dynamic predicates.
data Dynamic = Dynamic DynSpec       -- a single predicate
             | Prod Dynamic Dynamic  -- cartesian product of two dynamics
             | Cond Dynamic Bool     -- conditional dynamic


-- The behavior specification of a dynamic predicate (only internally used).
data DynSpec = Temporary  -- a dynamic predicate that exists only during
                          -- a single execution of a program
             | Persistent -- a persistent dynamic predicate

--- <code>dynamic</code> is only used for the declaration of
--- a dynamic predicate and should not be used elsewhere.
dynamic :: _
dynamic = failed

--- <code>persistent</code> is only used for the declaration of
--- a persistent dynamic predicate and should not be used elsewhere.
persistent :: String -> _
persistent _ = failed

----------------------------------------------------------------------

--- Combine two dynamics.
(<>) :: Dynamic -> Dynamic -> Dynamic
d1 <> d2 = Prod d1 d2

--- Restrict a dynamic with a condition.
(|>) :: Dynamic -> Bool -> Dynamic
d |> b = Cond d b

--- Restrict a dynamic with a constraint.
(|&>) :: Dynamic -> Success -> Dynamic
d |&> c = d |> (c &> True)

--- Asserts new facts (without free variables!) about dynamic predicates.
--- Conditional dynamics are asserted only if the condition holds.
assert :: Dynamic -> IO ()
assert (Dynamic spec) = assertFact (Dynamic spec)
assert (Prod d1 d2)   = assert d1 >> assert d2
assert (Cond d b)     = if b then assert d else done

--- Deletes facts (without free variables!) about dynamic predicates.
--- Conditional dynamics are retracted only if the condition holds.
--- Returns True if all facts to be retracted exist,
--- otherwise False is returned.
retract :: Dynamic -> IO Bool
retract (Dynamic spec) = retractFact (Dynamic spec)
retract (Prod d1 d2) = do
  b1 <- retract d1
  b2 <- retract d2
  return (b1&&b2)
retract (Cond d b) = if b then retract d else return True

--- Returns the knowledge at a particular point of time about dynamic
--- predicates. If other processes made changes to persistent predicates,
--- these changes are read and made visible to the currently running program.
getKnowledge :: IO (Dynamic -> Success)
getKnowledge = do
  known <- getDynamicKnowledge
  return (knownAll known)
 where
  knownAll k (Dynamic spec) = k (Dynamic spec)
  knownAll k (Prod d1 d2)   = knownAll k d1 & knownAll k d2
  knownAll k (Cond d b)     = knownAll k d & b=:=True

--- Returns all answers to an abstraction on a dynamic expression.
--- If other processes made changes to persistent predicates,
--- these changes are read and made visible to the currently running program.
getDynamicSolutions :: (a -> Dynamic) -> IO [a]
getDynamicSolutions query = do
  known <- getKnowledge
  getAllSolutions (\x -> known (query x))

--- Returns an answer to an abstraction on a dynamic expression.
--- Returns Nothing if no answer exists.
--- If other processes made changes to persistent predicates,
--- these changes are read and made visible to the currently running program.
getDynamicSolution :: (a -> Dynamic) -> IO (Maybe a)
getDynamicSolution query = do
  known <- getKnowledge
  getOneSolution (\x -> known (query x))

--- Returns True if there exists the argument facts (without free variables!)
--- and False, otherwise.
isKnown :: Dynamic -> IO Bool
isKnown (Dynamic spec) = do
  known <- getDynamicKnowledge
  first <- getOneSolution (\_ -> known (Dynamic spec))
  return (first /= Nothing)
isKnown (Prod d1 d2) = do
  b1 <- isKnown d1
  b2 <- isKnown d2
  return (b1&&b2)
isKnown (Cond d c) = do
  b <- isKnown d
  return (b&&c)


--- Perform an action (usually containing updates of various
--- dynamic predicates) as a single transaction.
--- This is the preferred way to execute any changes to persistent
--- dynamic predicates if there might be more than one process
--- that may modify the definition of such predicates in parallel.
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
--- a transaction should be handled (execept for abortTransaction).
--- If a transaction is externally interrupted (e.g., by killing the process),
--- some locks might never be removed. However, they
--- can be explicitly removed by deleting the corresponding lock files
--- reported at startup time.
---
--- Nested transactions are not supported and lead to a failure.
transaction :: IO a -> IO (Maybe a)
transaction action = do
  tnr <- startTransaction
  catchFail (performTrans tnr)
            (catchFail abortTransaction -- we perform an explicit abort in
                                        -- case of run time errors in action
                       (return Nothing))
 where
  performTrans tnr = do
    result <- action
    commitTransaction tnr
    return (Just result)

--- Perform an action (usually containing updates of various
--- dynamic predicates) as a single transaction.
--- This is similar to <code>transaction</code> but an execution
--- error is caught and returned instead of printing it.
transactionWithErrorCatch :: IO a -> IO (Either a IOError)
transactionWithErrorCatch action = do
  tnr <- startTransaction
  catch (performTrans tnr)
        (\e -> catch abortTransaction -- we perform an explicit abort in
                                      -- case of run time errors in action
                     (\_ -> return (Right e)))
 where
  performTrans tnr = do
    result <- action
    commitTransaction tnr
    return (Left result)

--- Aborts the current transaction. If a transaction is aborted,
--- the remaining actions of the transaction are not executed
--- and all changes to <b>persistent</b> dynamic predicates
--- made in this transaction are ignored.
---
--- abortTransaction should only be used in a transaction.
--- Although the execution of abortTransaction always fails
--- (basically, it writes an abort record in log files, unlock them
--- and then fails), the failure is handled inside <code>transaction</code>.
abortTransaction :: IO _
abortTransaction external


------------------------------------------------------------------------
-- Internals...

-- Asserts a new fact (without free variables!) about a dynamic predicate.
assertFact :: Dynamic -> IO ()
assertFact pred = prim_assertFact $## pred

prim_assertFact :: Dynamic -> IO ()
prim_assertFact external

-- Deletes a fact (without free variables!) about a dynamic predicate.
-- Returns True if there was such a fact and False, otherwise.
retractFact :: Dynamic -> IO Bool
retractFact pred = prim_retractFact $## pred

prim_retractFact :: Dynamic -> IO Bool
prim_retractFact external

-- Returns the knowledge at a particular point of time about all dynamic
-- predicates. If other processes made changes to persistent predicates,
-- these changes are read and made visible to the currently running program.
getDynamicKnowledge :: IO (Dynamic -> Success)
getDynamicKnowledge external

-- To implement transactions: lock persistent files, load newest version,
-- and mark begin of transaction in log files
startTransaction :: IO Int
startTransaction external

-- To implement transactions: mark end of transaction in log files and
-- unlock persistent files
commitTransaction :: Int -> IO ()
commitTransaction t = prim_commitTransaction $# t

prim_commitTransaction :: Int -> IO ()
prim_commitTransaction external

-- ...to implement getKnowledge:
isKnownAtTime :: Int -> Dynamic -> Success
isKnownAtTime t dyn = (prim_isKnownAtTime $# t) $!! dyn

prim_isKnownAtTime :: Int -> Dynamic -> Success
prim_isKnownAtTime external
