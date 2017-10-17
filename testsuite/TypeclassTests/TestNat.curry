-- Tests for user-defined instance of Num

import Test.Prop

data Nat = Zero | Succ Nat
  deriving (Eq,Show)

instance Num Nat where
  Zero   + n = n
  Succ n + m = Succ (n + m)

  Zero     * _ = Zero
  (Succ m) * n = n + (m * n)

  fromInteger n = case n of
    0 -> Zero
    _ -> Succ $ fromInteger $ n - 1


testZeroPlus3 = Zero + 3 -=- 3 + Zero

testZeroMult3 = Zero * 3 -=- 0

testZeroMultAny x = Zero * x -=- 0

testElem1 = always $ elem (Succ Zero) [Zero, 1, 42]

testElem42 = always $ elem 42 [Zero, 1, 42]
