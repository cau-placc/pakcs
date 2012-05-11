------------------------------------------------------------------------------
--- Preliminary library to support profiling.
---
--- @author Michael Hanus
--- @version December 2007
------------------------------------------------------------------------------

module Profile(ProcessInfo(..), getProcessInfos, showMemInfo, printMemInfo,
               garbageCollectorOff, garbageCollectorOn, garbageCollect,
               profileTime, profileTimeNF, profileSpace, profileSpaceNF,
               evalTime, evalSpace) where

import List(intersperse)

--- The data type for representing information about the state
--- of a Curry process.
--- @cons RunTime - the run time in milliseconds
--- @cons ElapsedTime - the elapsed time in milliseconds
--- @cons Memory - the total memory in bytes
--- @cons Code - the size of the code area in bytes
--- @cons Stack - the size of the local stack for recursive functions in bytes
--- @cons Heap - the size of the heap to store term structures in bytes
--- @cons Choices - the size of the choicepoint stack
--- @cons GarbageCollections - the number of garbage collections performed
data ProcessInfo = RunTime | ElapsedTime | Memory | Code
                 | Stack | Heap | Choices | GarbageCollections

--- Returns various informations about the current state of the Curry process.
--- Note that the returned values are very implementation dependent
--- so that one should interpret them with care!
getProcessInfos :: IO [(ProcessInfo,Int)]
getProcessInfos external

--- Turns off the garbage collector of the run-time system (if possible).
--- This could be useful to get more precise data of memory usage.
garbageCollectorOff :: IO ()
garbageCollectorOff external

--- Turns on the garbage collector of the run-time system (if possible).
garbageCollectorOn :: IO ()
garbageCollectorOn external

--- Invoke the garbage collector (if possible).
--- This could be useful before run-time critical operations.
garbageCollect :: IO ()
garbageCollect external

--- Get a human readable version of the memory situation from the
--- process infos.
showMemInfo :: [(ProcessInfo,Int)] -> String
showMemInfo infos = concat $ intersperse ", " $
  formatItem Memory "Memory: "  ++
  formatItem Code   "Code: "    ++
  formatItem Stack  "Stack: "   ++
  formatItem Choices"Choices: " ++
  formatItem Heap   "Heap: "
 where
   formatItem i s = maybe [] (\v -> [s ++ showBytes v]) (lookup i infos)

   showBytes b = if b<10000 then show b
                            else show (b `div` 1000) ++ " kb"

--- Print a human readable version of the current memory situation
--- of the Curry process.
printMemInfo = getProcessInfos >>= putStrLn . showMemInfo

--- Print the time needed to execute a given IO action.
profileTime :: IO a -> IO a
profileTime action = do
  garbageCollect
  pi1 <- getProcessInfos
  result <- action
  pi2 <- getProcessInfos
  putStrLn $ "Run time:            "
             ++ (showInfoDiff pi1 pi2 RunTime) ++ " msec."
  putStrLn $ "Elapsed time:        "
             ++ (showInfoDiff pi1 pi2 ElapsedTime) ++ " msec."
  putStrLn $ "Garbage collections: "
             ++ (showInfoDiff pi1 pi2 GarbageCollections)
  return result

--- Evaluates the argument to normal form
--- and print the time needed for this evaluation.
profileTimeNF :: a -> IO ()
profileTimeNF exp = profileTime (seq (id $!! exp) done)

--- Print the time and space needed to execute a given IO action.
--- During the executation, the garbage collector is turned off to get the
--- total space usage.
profileSpace :: IO a -> IO a
profileSpace action = do
  garbageCollect
  garbageCollectorOff
  pi1 <- getProcessInfos
  result <- action
  pi2 <- getProcessInfos
  garbageCollectorOn
  putStrLn $ "Run time:            "
             ++ (showInfoDiff pi1 pi2 RunTime) ++ " msec."
  putStrLn $ "Elapsed time:        "
             ++ (showInfoDiff pi1 pi2 ElapsedTime) ++ " msec."
  putStrLn $ "Garbage collections: "
             ++ (showInfoDiff pi1 pi2 GarbageCollections)
  putStrLn $ "Heap usage:          "
             ++ (showInfoDiff pi1 pi2 Heap) ++ " bytes"
  putStrLn $ "Stack usage:         "
             ++ (showInfoDiff pi1 pi2 Stack) ++ " bytes"
  return result

--- Evaluates the argument to normal form
--- and print the time and space needed for this evaluation.
--- During the evaluation, the garbage collector is turned off to get the
--- total space usage.
profileSpaceNF :: a -> IO ()
profileSpaceNF exp = profileSpace (seq (id $!! exp) done)

showInfoDiff infos1 infos2 item =
  show (maybe 0 id (lookup item infos2) - maybe 0 id (lookup item infos1))

--- Evaluates the argument to normal form (and return the normal form)
--- and print the time needed for this evaluation on standard error.
--- Included for backward compatibility only, use profileTime!
evalTime :: a -> a
evalTime external

--- Evaluates the argument to normal form (and return the normal form)
--- and print the time and space needed for this evaluation on standard error.
--- During the evaluation, the garbage collector is turned off.
--- Included for backward compatibility only, use profileSpace!
evalSpace :: a -> a
evalSpace external

