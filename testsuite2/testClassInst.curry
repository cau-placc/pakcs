-- Testing class and instance definitions

import Test.Prop

------------------------------------------------------------------------
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

------------------------------------------------------------------------
-- Now we test classes with non-deterministic operations.
-- Note that instances define operations rather than values.
-- Therefore, a non-deterministic operation without arguments
-- must have a non-deterministic behavior like a top-level function.
-- Since this is not conform with the standard dictionary translation,
-- instance operation without arguments are extended with an internal
-- argument.

class Choice a where
  choice :: a

instance Choice Int where
  choice = 0 ? 1

testChoiceInt :: Prop
testChoiceInt = (choice :: Int) <~> (1?0)

-- If we use the standard dictionary translation as in Haskell,
-- the following example would produce only two answer due to the
-- sharing of the `choice` variable.
testTwoChoices :: Prop
testTwoChoices = ([choice,choice] :: [Int]) <~> ([0,0]?[0,1]?[1,0]?[1,1])


------------------------------------------------------------------------
