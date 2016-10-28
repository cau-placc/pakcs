-- Testing class and instance definitions

import Test.Prop

-- A class with a duplicate operation and two instances.
class Dup a where
  dup :: a -> a

instance Dup Int where
  dup x = x + x

instance Dup Bool where
  dup x = not (not x)

testDupInt :: Int -> Prop
testDupInt x = dup x -=- 2*x

testDupBool :: Bool -> Prop
testDupBool x = dup x -=- x
