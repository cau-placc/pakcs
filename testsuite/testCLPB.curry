--- Some tests for library CLPB.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testCLPB"
--- 
--- @author Sebastian Fischer
--- @version May 2006

import CLPB
import Assertion

assert name = assertTrue name . evaluate
assertOp name op = assertSolutions name
                     (\ (x,y) -> satisfied (op x y) & bound [x,y])

testNeg = assertSolutions "neg" (\x -> satisfied (neg x) & bound [x]) [false]
testAnd = assertOp "and" (.&&) [(true,true)]
testOr = assertOp "or" (.||) [(true,false),(false,true),(true,true)]
testXor = assertOp "xor" (./=) [(true,false),(false,true)]
testEq = assertOp "eq" (.==) [(true,true),(false,false)]
testLeq = assertOp "leq" (.<=) [(false,false),(false,true),(true,true)]
testGeq = assertOp "geq" (.>=) [(true,true),(true,false),(false,false)]
testLess = assertOp "less" (.<) [(false,true)]
testGreater = assertOp "greater" (.>) [(true, false)]
testCount = assert "count" $ count [true,false] [1]
testExists = assert "exists" $ exists x x where x free
testCheck = assertTrue "check" $ check (x .|| neg x) where x free
testSimplify = assertSolutions "simplify"
                (\res -> let x free in res =:= simplify (x .|| neg x)) 
                [true]
testEvaluate = assertValues "evaluate" (evaluate unknown) [True,False]

