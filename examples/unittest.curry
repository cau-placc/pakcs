-- Examples for the currytest tool

-- In order to use the tool, one has to import the library Assertion
-- which contains the definition of the type of assertions:

import Assertion

-- Now we can test properties of our program:
-- A test is any top-level function of type "Assertion".
-- All functions of this type are considered by the tester
-- provided that they are exported by the module.

-- Note that we state some (logically wrong) assertions in order
-- to show the output of failures with the test tool.

-- This test should succeed:
test1 = assertEqual "++"     ([1,2]++[3,4]) [1,2,3,4]

-- This test should fail:
test2 = assertEqual "head 1" (head [2]) 1

-- This test should fail (no solution):
test3 = assertEqual "head 2" (head []) []

-- This test should fail:
test4 = assertTrue "null test" (null [1])

-- This test should fail (provided that there is no file "xxxx"):
test5 = assertIO "IO test" (readFile "xxxx") ""

