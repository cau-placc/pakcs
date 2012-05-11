------------------------------------------------------------------------------
--- Library with some useful functions on characters.
---
--- @author Michael Hanus
--- @version November 2001
------------------------------------------------------------------------------

module Char(isUpper,isLower,isAlpha,isDigit,isAlphaNum,isOctDigit,
            isHexDigit,isSpace,toUpper,toLower,digitToInt,intToDigit) where

--- Returns true if the argument is an uppercase letter.
isUpper       :: Char -> Bool
isUpper c     =  ord c >= ord 'A' && ord c <= ord 'Z'

--- Returns true if the argument is an lowercase letter.
isLower       :: Char -> Bool
isLower c     =  ord c >= ord 'a' && ord c <= ord 'z'

--- Returns true if the argument is a letter.
isAlpha       :: Char -> Bool
isAlpha c     =  isUpper c || isLower c

--- Returns true if the argument is a decimal digit.
isDigit       :: Char -> Bool
isDigit c     =  ord '0' <= ord c && ord c <= ord '9'

--- Returns true if the argument is a letter or digit.
isAlphaNum    :: Char -> Bool
isAlphaNum c  =  isAlpha c || isDigit c

--- Returns true if the argument is an octal digit.
isOctDigit   :: Char -> Bool
isOctDigit c  =  ord c >= ord '0' && ord c <= ord '7'

--- Returns true if the argument is a hexadecimal digit.
isHexDigit    :: Char -> Bool
isHexDigit c  =  isDigit c || ord c >= ord 'A' && ord c <= ord 'F'
                           || ord c >= ord 'a' && ord c <= ord 'f'

--- Returns true if the argument is a white space.
isSpace       :: Char -> Bool
isSpace c     =  c == ' '  || c == '\t' || c == '\n' ||
                 c == '\r' || ord c == 12 -- form feed

--- Converts lowercase into uppercase letters.
toUpper   :: Char -> Char
toUpper c | isLower c = chr (ord c - ord 'a' + ord 'A')
          | otherwise = c

--- Converts uppercase into lowercase letters.
toLower   :: Char -> Char
toLower c | isUpper c = chr (ord c - ord 'A' + ord 'a')
          | otherwise = c

--- Converts a (hexadecimal) digit character into an integer.
digitToInt :: Char -> Int
digitToInt c
  | isDigit c                            =  ord c - ord '0'
  | ord c >= ord 'A' && ord c <= ord 'F' =  ord c - ord 'A' + 10
  | ord c >= ord 'a' && ord c <= ord 'f' =  ord c - ord 'a' + 10
  | otherwise  =  error "Char.digitToInt: argument is not a digit"

--- Converts an integer into a (hexadecimal) digit character.
intToDigit :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9  =  chr (ord '0' + i)
  | i >= 10 && i <= 15  =  chr (ord 'A' + i - 10)
  | otherwise           =  error "Char.intToDigit: argument not a digit value"

