-- Some tests for module Integer
--
-- To run all tests automatically by the currycheck tool, use the command:
-- "curry check TestInteger"

import Integer
import Test.Prop

testPow = [pow 0 0, pow 1 0, pow 0 1, pow 7 2, pow 2 7, pow 9 9]
          -=- [1, 1, 0, 49, 128, 387420489] 

testIlog = (map ilog [1, 5, 10, 15, 50, 100, 999, 1000000000])
           -=- [0, 0, 1, 1, 1, 2, 2, 9]

testIsqrt = map isqrt [0, 1, 3, 4, 24, 25, 26, 99, 100, pow k 2]
            -=- [0, 1, 1, 2, 4, 5, 5, 9, 10, k]
 where k = 33452

testAbs = (map abs [-99, -2, -1, 0, 1, 2, 99])
          -=- [99, 2, 1, 0, 1, 2, 99]

testFactorial = (map factorial [0, 1, 5, 10])
                -=- [1, 1, 120, 3628800]

testBinomial = [binomial 1 1, binomial 10 1, binomial 5 2, binomial 5 3]
               -=- [1, 10, 10, 10]

testMax = [max 0 0, max 1 0, max 0 1, max (-7) 2, max 2 (-7), max (-9) (-9)]
          -=- [0, 1, 1, 2, 2, -9]

testMin = [min 0 0, min 1 0, min 0 1, min (-7) 2, min 2 (-7), min (-9) (-9)]
          -=- [0, 0, 0, -7, -7, -9]

testMax3 = [7, 44, 12] -=-
           [max3 (-2) 5 7,  max3 4 44 (-4), max3 12 1 9]

testMin3 = [-2, -4, 1] -=-
           [min3 (-2) 5 7,  min3 4 44 (-4), min3 12 1 9]

testMaxlist = (maxlist [0, -9, 45, 16, -32]) -=- 45

testMinlist = (minlist [0, -9, 45, 16, -32]) -=- (-32)

testBitTrunc = [0, 7, 0, 11] -=-
               [bitTrunc 0 123, bitTrunc 3 127, bitTrunc 3 128, bitTrunc 11 11]

testBitAnd = [0, 1, 2, 15, 8] -=-
  [bitAnd 0 123, bitAnd 45 1, bitAnd 11 6, bitAnd 15 15,
   bitAnd (0*1+1*2+0*4+1*8) (0*1+0*2+1*4+1*8)]

testBitOr = [123, 45, 15, 15, 14] -=-
  [bitOr 0 123, bitOr 45 1, bitOr 11 6, bitOr 15 15,
   bitOr (0*1+1*2+0*4+1*8) (0*1+0*2+1*4+1*8)]

testBitNot = [0, pow 2 32 - 2, k] -=-
             [bitNot (pow 2 32 - 1), bitNot 1, bitNot (bitNot k)]
  where k = 6798823

testBitXor = [123, 44, 13, 0, 6] -=-
  [bitXor 0 123, bitXor 45 1, bitXor 11 6, bitXor 15 15,
   bitXor (0*1+1*2+0*4+1*8) (0*1+0*2+1*4+1*8)]

testEven = [True,True,False,False,True,False] -=-
           (map even [0,2,1,12345,-99990,-25])

testOdd = [False,True,True,True,False,True] -=-
          (map odd [0,1,3,12345,-99990,-25])
