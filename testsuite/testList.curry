-- A simple example for the currytest tool

-- Here we want to test some list processing functions.
-- Thus, we import the modules List and Assertion:

import List
import Assertion

-- Now we can test properties of our program:
-- A test is any top-level function of type "Assertion".
-- All functions of this type are considered by the tester
-- provided that they are exported by the module.

test1 = assertEqual "++"  ([1,2]++[3,4]) [1,2,3,4]

test2 = assertEqual "nub" (nub [1,3,1,2,3,2,4]) [1,3,2,4]

test3 = assertTrue  "all" (all (<5) [1,2,3,4])


