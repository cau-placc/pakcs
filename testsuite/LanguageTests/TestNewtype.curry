
-- Testing newtype semantics:

import Test.Prop

newtype LIST a = LIST [a]
 deriving (Eq, Read, Show)

fromLIST :: LIST a -> [a]
fromLIST (LIST xs) = xs

aLIST :: LIST Bool
aLIST = LIST [True]

headIfTrue :: LIST Int -> Bool -> Int
headIfTrue (LIST xs) b = if b then head xs else 42

testNonStrictNT :: Prop
testNonStrictNT = headIfTrue failed False -=- 42

testShowNT :: [Int] -> Prop
testShowNT xs = show (LIST xs) -=- "LIST " ++ show xs

testReadShowNT :: [Int] -> Prop
testReadShowNT xs = LIST xs -=- read ("LIST " ++ show xs)
