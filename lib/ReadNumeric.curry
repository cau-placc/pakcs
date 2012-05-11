------------------------------------------------------------------------------
--- Library with some functions for reading and converting numeric tokens.
--
--- @author Michael Hanus, Frank Huch
--- @version September 2006
------------------------------------------------------------------------------

module ReadNumeric(readInt,readNat,readHex,readOct) where

import Char

--- Read a (possibly negative) integer as a first token in a string.
--- The string might contain leadings blanks and the integer is read
--- up to the first non-digit.
--- If the string does not start with an integer token, Nothing is returned,
--- otherwise the result is (Just (v,s)) where v is the value of the integer
--- and s is the remaing string without the integer token.
--readInt :: String -> Maybe (Int,String)
readInt str = case dropWhile (==' ') str of
  []       -> Nothing
  '-':str1 -> maybe Nothing (\ (val,rstr) -> Just (-val,rstr)) (readNat str1)
  str1     -> readNat str1

--- Read a natural number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-digit.
--- If the string does not start with a natural number token, Nothing is returned,
--- otherwise the result is (Just (v,s)) where v is the value of the number
--- and s is the remaing string without the number token.
readNat :: String -> Maybe (Int,String)
readNat str = readNumPrefix (dropWhile (==' ') str) Nothing 10 isDigit digitToInt


--- Read a hexadecimal number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-hexadecimal digit.
--- If the string does not start with a hexadecimal number token, Nothing is returned,
--- otherwise the result is (Just (v,s)) where v is the value of the number
--- and s is the remaing string without the number token.
readHex :: String -> Maybe (Int,String)
readHex l = readNumPrefix (dropWhile (\c->c==' ') l) Nothing 16 isHexDigit digitToInt


--- Read an octal number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-octal digit.
--- If the string does not start with an octal number token, Nothing is returned,
--- otherwise the result is (Just (v,s)) where v is the value of the number
--- and s is the remaing string without the number token.
readOct :: String -> Maybe (Int,String)
readOct l = readNumPrefix (dropWhile (\c->c==' ') l) Nothing 8 isOctDigit digitToInt


--- Read an integral number prefix where the value of an already read number prefix is
--- provided as the second argument. The third argument is the base, the fourth argument
--- is a predicate to distinguish valid digits, and the fifth argument converts
--- valid digits into integer values.
readNumPrefix :: String -> Maybe Int -> Int -> (Char->Bool) -> (Char->Int)
                 -> Maybe (Int,String)
readNumPrefix "" Nothing _ _ _ = Nothing
readNumPrefix "" (Just n) _ _ _ = Just (n,"")
readNumPrefix (c:cs) (Just n) base isdigit valueof
   | isdigit c = readNumPrefix cs (Just (base*n+valueof c)) base isdigit valueof
   | otherwise = Just (n,c:cs)
readNumPrefix (c:cs) Nothing base isdigit valueof
   | isdigit c = readNumPrefix cs (Just (valueof c)) base isdigit valueof
   | otherwise = Nothing


-- end of library ReadNumeric
