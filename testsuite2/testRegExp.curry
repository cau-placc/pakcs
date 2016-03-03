{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to support
--- regular expression matching.
--- The syntax of regular expression is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import RegExp -- required in the pre-processed program
import Test.EasyCheck

check1 :: Bool
check1 = "abc" ``regex abc''

testCheck1 = always check1


check2 :: Bool
check2 = "abaaaaaaaaaaaaac" ``regex aba*c''

testCheck2 = always check2


check3 :: String -> Bool
check3 s = s ``regex (a|(bc*))+''

testCheck3a = always (check3 "abcccbccca")
testCheck3b = always (not (check3 "abcccbcccac"))


check4 :: String -> Bool
check4 s = s ``regex [:alpha:]''

testCheck4a = always (check4 "a")
testCheck4b = always (check4 "A")
testCheck4c = always (not (check4 "0"))
testCheck4d = always (not (check4 "ab"))


check5 :: String -> Bool
check5 s = s ``regex [a-z]+''

testCheck5a = always (check5 "a")
testCheck5b = always (check5 "abc")
testCheck5c = always (not (check5 "abc42"))
testCheck5d = always (not (check5 ""))

-- Tests with parameterized regular expressions:

pregexp1 :: [a] -> a -> a -> Bool
pregexp1 s v1 v2 = s ``regex [<v1>-<v2>]*''

testPregexp1a = always (pregexp1 "abc" 'a' 'z')
testPregexp1b = always (pregexp1 "ABC" 'A' 'Z')
testPregexp1c = always (not (pregexp1 "abc" 'A' 'Z'))


pregexp2 :: [a] -> a -> a -> Bool
pregexp2 s v1 v2 = s ``regex (<v1>|<v2>)*''

testPregexp2a = always (pregexp2 "abaabbb" 'a' 'b')
testPregexp2b = always (not (pregexp2 "abaabbb" 'a' 'z'))


-- A regular expression containing a complex Curry expression:
check6 :: Bool
check6 = "a" ``regex <((\x -\> x) 'a')>'' 

testCheck6 = always check6

