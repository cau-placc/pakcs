------------------------------------------------------------------------------
--- This module defines the datatype and operations for the
--- Curry module tester "currytest".
---
--- @author Michael Hanus
--- @version June 2012
------------------------------------------------------------------------------

module Assertion(-- for writing test cases:
                 Assertion,assertTrue,assertEqual,
                 assertValues,assertSolutions,assertIO,assertEqualIO,
                 -- the remaining entities are only used by the test tool:
                 checkAssertion,
                 seqStrActions,writeAssertResult,
                 ProtocolMsg(..),
                 showTestMod,showTestCase,showTestEnd,showTestCompileError)
 where

import AllSolutions
import List((\\))
import Socket -- for sending results to test GUI
import IO(hPutStrLn,hClose)

infixl 1 `seqStrActions`

--- Datatype for defining test cases.
--- @cons AssertTrue   s b     - assert (with name s) that b must be true
--- @cons AssertEqual  s e1 e2 - assert (with name s) that e1 and e2 must
---                              be equal (w.r.t. ==)
--- @cons AssertValues s e vs  - assert (with name s) that vs is the multiset
---                              of all values of e (i.e., all values of e are
---                              compared with the elements in vs w.r.t. ==)
--- @cons AssertSolutions s c vs - assert (with name s) that constraint
---   abstraction c has the multiset of solutions vs
---   (i.e., the solutions of c are compared with the elements in vs w.r.t. ==)
--- @cons AssertIO     s a r   - assert (with name s) that I/O action a
---                              yields the result value r
--- @cons AssertEqualIO s a1 a2 - assert (with name s) that I/O actions a1 and
---                               a2 yield equal (w.r.t. ==) results
data Assertion a = AssertTrue      String Bool
                 | AssertEqual     String a a
                 | AssertValues    String a [a]
                 | AssertSolutions String (a->Success) [a]
                 | AssertIO        String (IO a) a
                 | AssertEqualIO   String (IO a) (IO a)

--- `(assertTrue s b)` asserts (with name `s`) that `b` must be true.
assertTrue :: String -> Bool -> Assertion ()
assertTrue s b = AssertTrue s b

--- `(assertEqual s e1 e2)` asserts (with name `s`) that `e1` and `e2`
--- must be equal (w.r.t. `==`).
assertEqual :: String -> a -> a -> Assertion a
assertEqual s x y = AssertEqual s x y

--- `(assertValues s e vs)` asserts (with name `s`) that `vs` is the multiset
--- of all values of `e`. All values of `e` are
--- compared with the elements in `vs` w.r.t. `==`.
assertValues :: String -> a -> [a] -> Assertion a
assertValues s x y = AssertValues s x y

--- `(assertSolutions s c vs)` asserts (with name `s`) that constraint
--- abstraction `c` has the multiset of solutions `vs`.
--- The solutions of `c` are compared with the elements in `vs` w.r.t. `==`.
assertSolutions :: String -> (a->Success) -> [a] -> Assertion a
assertSolutions s x y = AssertSolutions s x y

--- `(assertIO s a r)` asserts (with name `s`) that I/O action `a`
--- yields the result value `r`.
assertIO :: String -> IO a -> a -> Assertion a
assertIO s x y = AssertIO s x y

--- `(assertEqualIO s a1 a2)` asserts (with name `s`) that I/O actions `a1`
--- and `a2` yield equal (w.r.t. `==`) results.
assertEqualIO :: String -> IO a -> IO a -> Assertion a
assertEqualIO s x y = AssertEqualIO s x y

--- Combines two actions and combines their results.
--- Used by the currytest tool.
seqStrActions :: IO (String,Bool) -> IO (String,Bool) -> IO (String,Bool)
seqStrActions a1 a2 =
  do (s1,b1) <- a1
     (s2,b2) <- a2
     return (s1++s2,b1&&b2)

--- Executes and checks an assertion, and process the result
--- by an I/O action.
--- Used by the currytest tool.
--- @param protocol - an action to be applied after test execution
--- @param assertion - an assertion to be tested
--- @return a protocol string and a flag whether the test was successful
checkAssertion :: String -> ((String,Bool) -> IO (String,Bool)) -> Assertion _
               -> IO (String,Bool)
checkAssertion _ prot assrt =
   --catchNDIO asrtfname prot
   (execAsrt assrt)
 where
  execAsrt (AssertTrue name cond) =
    catch (checkAssertTrue name cond) (returnError name) >>= prot
  execAsrt (AssertEqual name call result) =
    catch (checkAssertEqual name call result) (returnError name) >>= prot
  execAsrt (AssertValues name expr results) =
    catch (checkAssertValues name expr results) (returnError name) >>= prot
  execAsrt (AssertSolutions name constr results) =
    catch (checkAssertSolutions name constr results) (returnError name) >>= prot
  execAsrt (AssertIO name action result) =
    catch (checkAssertIO name action result) (returnError name) >>= prot
  execAsrt (AssertEqualIO name action1 action2) =
    catch (checkAssertEqualIO name action1 action2) (returnError name) >>= prot
  
  returnError name err =
    return ("FAILURE of "++name++": "++showError err++"\n",False)
  
-- Execute I/O action for assertion checking and report any failure
-- or non-determinism.
catchNDIO :: String -> ((String,Bool) -> IO (String,Bool))
          -> IO ((String,Bool)) -> IO ((String,Bool))
catchNDIO fname prot a = getAllValues a >>= checkIOActions
 where
  checkIOActions results
    | null results
     = prot ("ERROR in operation "++fname++": computation failed\n",False)
    | not (null (tail results))
     = prot ("ERROR in operation "++fname++
             ": computation is non-deterministic\n",False)
    | otherwise = head results

-- Checks Boolean assertion.
checkAssertTrue :: String -> Bool -> IO (String,Bool)
checkAssertTrue name cond =
  if cond
  then return ("OK: "++name++"\n",True)
  else return ("FAILURE of "++name++": assertion not satisfied:\n",False)

-- Checks equality assertion.
checkAssertEqual :: String -> a -> a -> IO (String,Bool)
checkAssertEqual name call result =
  if call==result
  then return ("OK: "++name++"\n",True)
  else return ("FAILURE of "++name++": equality assertion not satisfied:\n"++
               "Computed answer: "++show call++"\n"++
               "Expected answer: "++show result++"\n",False)

-- Checks all values assertion.
checkAssertValues :: String -> a -> [a] -> IO (String,Bool)
checkAssertValues name call results = do
  rs <- getAllValues call
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": values assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks all solutions of a constraint abstraction.
checkAssertSolutions :: String -> (a->Success) -> [a] -> IO (String,Bool)
checkAssertSolutions name constr results = do
  rs <- getAllSolutions constr
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": solutions assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks an IO assertion.
checkAssertIO :: String -> IO a -> a -> IO (String,Bool)
checkAssertIO name action result = do
  r <- action
  if r==result
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO assertion not satisfied:\n"++
                 "Computed answer: "++show r++"\n"++
                 "Expected answer: "++show result++"\n\n",False)

-- Checks equality of results of two IO assertions.
checkAssertEqualIO :: String -> IO a -> IO a -> IO (String,Bool)
checkAssertEqualIO name action1 action2 = do
  r1 <- action1
  r2 <- action2
  if r1==r2
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO equality assertion not satisfied:\n"++
                 "Computed answer 1: "++show r1++"\n"++
                 "Computed answer 2: "++show r2++"\n\n",False)

--- Prints the results of assertion checking.
--- If failures occurred, the return code is positive.
--- Used by the currytest tool.
writeAssertResult :: (String,Bool) -> IO Int
writeAssertResult (result,flag) =
  if flag
  then putStrLn (result++"All tests successfully passed.")         >> return 0
  else putStrLn (result++"FAILURE occurred in some assertions!\n") >> return 1


----------------------------------------------------------------------------
-- The following entities are used to implement the test GUI:

--- The messages sent to the test GUI.
--- Used by the currytest tool.
data ProtocolMsg = TestModule String | TestCase String Bool | TestFinished
                 | TestCompileError

--- Sends message to GUI for showing test of a module.
--- Used by the currytest tool.
showTestMod :: Int -> String -> IO ()
showTestMod portnum modname = sendToLocalSocket portnum (TestModule modname)

--- Sends message to GUI for showing result of executing a test case.
--- Used by the currytest tool.
showTestCase :: Int -> (String,Bool) -> IO (String,Bool)
showTestCase portnum (s,b) = do
  sendToLocalSocket portnum (TestCase s b)
  return (s,b)

--- Sends message to GUI for showing end of module test.
--- Used by the currytest tool.
showTestEnd :: Int -> IO ()
showTestEnd portnum = sendToLocalSocket portnum TestFinished

--- Sends message to GUI for showing compilation errors in a module test.
--- Used by the currytest tool.
showTestCompileError :: Int -> IO ()
showTestCompileError portnum = sendToLocalSocket portnum TestCompileError

--- Sends protocol message to local socket.
sendToLocalSocket :: Int -> ProtocolMsg -> IO ()
sendToLocalSocket portnum msg = do
  h <- connectToSocket "localhost" portnum
  hPutStrLn h (show msg)
  hClose h

-- end of module Assertion
