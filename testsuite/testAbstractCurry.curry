------------------------------------------------------------------------------
--- Some tests for AbstractCurry libraries.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testAbstractCurry"
--- 
--- @author Michael Hanus
--- @version September 2015
------------------------------------------------------------------------------

import AbstractCurry.Files
import AbstractCurry.Pretty
import Directory
import FlatCurry
import Assertion

--- Test for equality of a FlatCurry program with the same program
--- after pretty printing the AbstractCurry program:
readAndTestEqualFcy mod = do
  prog1 <- readFlatCurry mod
  let modcurry = mod ++ ".curry"
      modbak   = mod ++ ".BAK"
  renameFile modcurry modbak
  copyFile modbak modcurry
  let modpp = mod ++ ".PP"
  readCurry mod >>= writeFile modpp . showCProg
  removeFile modcurry
  renameFile modpp modcurry
  prog2 <- readFlatCurry mod
  removeFile modcurry
  renameFile modbak modcurry
  return (prog1 == prog2)

test1  = assertIO "AbstractCurry.Pretty test for rev"
                  (readAndTestEqualFcy "rev") True

test2  = assertIO "AbstractCurry.Pretty test for testFuncPattern"
                  (readAndTestEqualFcy "testFuncPattern") True
