------------------------------------------------------------------------------
--- Some tests for library Global
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "curry check TestGlobal"
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import Global
import System(system)
import Test.Prop

------------------------------------------------------------------------------
-- Testing a simple integer temporary global entity:
points :: Global Int
points = global (div 1 1) Temporary

rwglobal = do
  v1 <- readGlobal points
  writeGlobal points 42
  v2 <- readGlobal points
  return (v1,v2)

testSimpleIntReadGlobalWriteGlobal = rwglobal `returns` (1,42)

------------------------------------------------------------------------------
-- Testing a temporary global entity containing a list structure:
nats :: Global [Int]
nats = global [] Temporary

listrwglobal = do
  writeGlobal nats [1..5]
  v1 <- readGlobal nats
  writeGlobal nats (v1++v1)
  v2 <- readGlobal nats
  return (v1,v2)

testSimpleIntlistReadGlobalWriteGlobal =
  listrwglobal `returns` ([1..5],[1..5]++[1..5])

------------------------------------------------------------------------------
-- Testing the interaction of two integer temporary global entities:
gint1 :: Global Int
gint1 = global 0 Temporary

gint2 :: Global Int
gint2 = global 42 Temporary

rwglobals = do
  v1 <- readGlobal gint1
  v2 <- readGlobal gint2
  writeGlobal gint2 99
  v3 <- readGlobal gint1
  v4 <- readGlobal gint2
  writeGlobal gint1 (v4+1)
  v5 <- readGlobal gint1
  v6 <- readGlobal gint2
  return [v1,v2,v3,v4,v5,v6]

testReadWriteTwoTemporaryGlobals = rwglobals `returns` [0,42,0,99,100,99]

------------------------------------------------------------------------------
-- Testing a simple integer persistent global entity:
ppoints :: Global Int
ppoints = global (3+4) (Persistent "pointsstore")

rwglobalp = do
  v1 <- readGlobal ppoints
  writeGlobal ppoints 42
  v2 <- readGlobal ppoints
  return (v1,v2)

testPersistentIntReadGlobalWriteGlobal = rwglobalp `returns` (7,42)


-- finalize: clean
testCleanUp = (system "rm -r pointsstore*") `returns` 0

