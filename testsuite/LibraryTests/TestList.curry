-- A simple example for the currycheck tool

-- Here we want to test some list processing functions.
-- Thus, we import the modules List and Test.EasyCheck:

import List
import Test.EasyCheck

-- Now we can test properties of our program:

testAppend = ([1,2]++[3,4]) -=- [1,2,3,4]

testNub = (nub [1,3,1,2,3,2,4]) -=- [1,3,2,4]

testAll = always (all (<5) [1,2,3,4])


