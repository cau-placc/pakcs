-- Test of the Maybe monad

import Test.Prop

-- Example for the `do` notation for the `Maybe` monad:
doMaybe1 :: Maybe (Bool,Int)
doMaybe1 = do
  x <- return True
  Nothing
  y <- return 42
  return (x,y)

testDoMaybe1 = doMaybe1 -=- Nothing

-- Example for the `do` notation for the `Maybe` monad:
doMaybe2 :: Maybe (Bool,Int)
doMaybe2 = do
  x <- return True
  y <- return 42
  return (x,y)

testDoMaybe2 = doMaybe2 -=- Just (True,42)

