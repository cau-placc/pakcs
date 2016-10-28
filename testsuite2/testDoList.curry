-- Test the right translation of the do notation for the list Monad.

import Test.Prop

-- Returns [42] with list comprehension:
list42Comp :: [Int]
list42Comp = [ x | (True,x) <- [(True,42)] ]

testList42Comp = list42Comp -=- [42]

-- Returns [42] with do notation:
list42Do :: [Int]
list42Do = do
  (True,x) <- return (True,42)
  return x

testList42Do = list42Do -=- [42]

-- Returns [] with list comprehension:
listEmptyComp :: [Int]
listEmptyComp = [ x | (True,x) <- [(False,42)] ]

testListEmptyComp = listEmptyComp -=- []

-- Returns [] with do notation:
listEmptyDo :: [Int]
listEmptyDo = do
  (True,x) <- return (False,42)
  return x

testListEmptyDo = listEmptyDo -=- []

{-
Note:

The latter requires the translation of the do notation with
the following scheme:

do p <- e1
   e2

is translated into

e1 >>= \x -> case x of p -> e1
                       _ -> fail "Pattern match failed"

where x is a fresh variable name.

-}
