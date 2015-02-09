{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=--foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to support
--- string formatting.
---
--- The format specification follows the C specification for `printf`
--- formatting whch may be found at
--- <http://pubs.opengroup.org/onlinepubs/009695399/functions/fprintf.html>
------------------------------------------------------------------------------

import Assertion
import Format -- required in the pre-processed program

-- Format a string and an integer:
ex1 :: String -> Int -> String
ex1 name age = ``format "Hello %s, you are %i years old.",name,age''

test1 = assertEqual "ex1" (ex1 "Mike" 42) "Hello Mike, you are 42 years old."

-- Format two integers:
ex2 :: Int -> Int -> String
ex2 n1 n2 = ``format "%+.5d and % 10.4i",n1,n2''

test2 = assertEqual "ex2" (ex2 12345 34) "+12345 and       0034"

-- Format a charater:
ex3 :: Char -> String
ex3 c = ``format "This is a char: %c",c''

test3 = assertEqual "ex3" (ex3 'a') "This is a char: a"

-- Format a string with a given width and maximal length:
ex4 :: String -> String
ex4 s = ``format "This is a string: %010.4s!",s''

test4 = assertEqual "ex4" (ex4 "Hello") "This is a string:       Hell!"

-- Format a float with a given width and precision:
ex5 :: Float -> String
ex5 f = ``format "This is a float: %+7.3f",f''

test5 = assertEqual "ex5" (ex5 3.14159) "This is a float:  +3.142"

-- Format a float with an exponent:
ex6 :: Float -> String
ex6 f = ``format "This is another float: % .4E",f''

test6 = assertEqual "ex6" (ex6 3.14159) "This is another float:  3.1416E+00"

-- Format the sum of two integers:
ex7 :: Int -> Int -> String
ex7 n1 n2 = ``format "The sum of %+.5d and %+5i is %+6i",n1,n2,n1+n2''

test7 = assertEqual "ex7" (ex7 1234 890) "The sum of +01234 and  +890 is  +2124"
