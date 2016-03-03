--- Some tests for library CLPB.
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "currycheck testCLPB"
--- 
--- @author Sebastian Fischer

import CLPB
import Test.EasyCheck

assertOp op = solutionOf (\ (x,y) -> satisfied (op x y) & bound [x,y])

testNeg = solutionOf (\x -> satisfied (neg x) & bound [x]) <~> false
testAnd = assertOp (.&&) <~> (true,true)
testOr  = assertOp (.||) <~> anyOf [(true,false),(false,true),(true,true)]
testXor = assertOp (./=) <~> anyOf [(true,false),(false,true)]
testEq  = assertOp (.==) <~> anyOf [(true,true),(false,false)]
testLeq = assertOp (.<=) <~> anyOf [(false,false),(false,true),(true,true)]
testGeq = assertOp (.>=) <~> anyOf [(true,true),(true,false),(false,false)]
testLess = assertOp (.<) <~> (false,true)
testGreater = assertOp (.>) <~> (true, false)
testCount  = always $ evaluate $ count [true,false] [1]
testExists = always $ evaluate $ extest
 where
   extest = let x = unknown in exists x x
testCheck  = solutionOf (\res -> let x free in res =:= check (x .|| neg x))
                        <~> True
testSimplify = solutionOf
                (\res -> let x free in res =:= simplify (x .|| neg x)) 
                <~> true
testEvaluate = (evaluate unknown) <~> (True ? False)

