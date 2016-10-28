-- Testing Read and Show instances for various types

import Test.EasyCheck

showOfRead :: (Eq a, Read a, Show a) => a -> Prop
showOfRead x = read (show x) -=- x

showOfReadBool :: Bool -> Prop
showOfReadBool = showOfRead

showOfReadInt :: Int -> Prop
showOfReadInt = showOfRead

showOfReadChar :: Char -> Prop
showOfReadChar = showOfRead

showOfReadOrdering :: Ordering -> Prop
showOfReadOrdering = showOfRead
{-
showOfReadString :: String -> Prop
showOfReadString = showOfRead

showOfReadMaybeInt :: Maybe Int -> Prop
showOfReadMaybeInt = showOfRead
-}
showOfReadEitherCharBool :: Either Char Bool -> Prop
showOfReadEitherCharBool = showOfRead

showOfReadPairIntBool :: (Int,Bool) -> Prop
showOfReadPairIntBool = showOfRead

showOfReadMaybeOrdering :: Maybe Ordering -> Prop
showOfReadMaybeOrdering = showOfRead

showOfReadListInt :: [Int] -> Prop
showOfReadListInt = showOfRead

