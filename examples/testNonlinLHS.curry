-- Testing rules with non-linear left-hand sides:

import Assertion

f x x = x

test1 = assertValues "f 1 2" (f 1 2) []
test2 = assertValues "f 1 1" (f 1 1) [1]
