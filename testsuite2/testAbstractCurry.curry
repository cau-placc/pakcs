------------------------------------------------------------------------------
--- Some tests for AbstractCurry libraries.
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "currycheck testAbstractCurry"
--- 
--- @author Michael Hanus
--- @version September 2015
------------------------------------------------------------------------------

import AbstractCurry.Files
import AbstractCurry.Pretty
import Directory
import FlatCurry.Types
import FlatCurry.Files
import Test.EasyCheck

--- Test for equality of a FlatCurry program with the same program
--- after pretty printing the AbstractCurry program:
readAndTestEqualFcy :: String -> IO Bool
readAndTestEqualFcy mod = do
  prog1 <- readFlatCurryStrict mod
  let modcurry = mod ++ ".curry"
      modbak   = mod ++ ".BAK"
  renameFile modcurry modbak
  copyFile modbak modcurry
  let modpp = mod ++ ".PP"
  readCurry mod >>= writeFile modpp . showCProg
  removeFile modcurry
  renameFile modpp modcurry
  prog2 <- readFlatCurryStrict mod
  removeFile modcurry
  renameFile modbak modcurry
  let flatequal = prog1 == prog2
  unless flatequal $ do
    putStrLn ("Original flat program:        " ++ show prog1)
    putStrLn ("Pretty printed  flat program: " ++ show prog2)
  return flatequal

-- Strictly read a FlatCurry program in order to avoid race conditions
-- due to copying/moving source files:
readFlatCurryStrict :: String -> IO Prog
readFlatCurryStrict mod = do
  prog <- readFlatCurry mod
  id $!! prog `seq` return prog

testAbstractCurryPretty_rev =
  (readAndTestEqualFcy "rev") `returns` True
                  
testAbstractCurryPretty_testSetFunctions =
  (readAndTestEqualFcy "testSetFunctions") `returns` True

testAbstractCurryPretty_testFuncPattern =
  (readAndTestEqualFcy "testFuncPattern") `returns` True
