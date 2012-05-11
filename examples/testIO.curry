------------------------------------------------------------------------------
--- Some tests for library IO
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testIO"
--- 
--- @author Michael Hanus
--- @version January 2005
------------------------------------------------------------------------------

import Assertion
import System
import IO

-- Test repositioning on files:
seek = do
  system "rm -f tmpio"
  writeFile "tmpio" "0123456789"
  h <- openFile "tmpio" ReadMode
  hSeek h AbsoluteSeek 0
  c1 <- hGetChar h
  hSeek h AbsoluteSeek 3
  c2 <- hGetChar h
  hSeek h RelativeSeek 2
  c3 <- hGetChar h
  hSeek h SeekFromEnd (-1)
  c4 <- hGetChar h
  hClose h
  system "rm -f tmpio"
  return [c1,c2,c3,c4] --> "0369"

testSeek = assertIO "test IO.hSeek" seek "0369"
