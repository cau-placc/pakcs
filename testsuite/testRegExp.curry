{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=--foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to support
--- regular expression matching.
--- The syntax of regular expression is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import Assertion
import RegExp -- required in the pre-processed program

check1 :: Bool
check1 = "abc" ``regex abc''

test1 = assertTrue "check1" check1


check2 :: Bool
check2 = "abaaaaaaaaaaaaac" ``regex aba*c''

test2 = assertTrue "check2" check2


check3 :: String -> Bool
check3 s = s ``regex (a|(bc*))+''

test3a = assertTrue "check3a" (check3 "abcccbccca")
test3b = assertTrue "check3b" (not (check3 "abcccbcccac"))


check4 :: String -> Bool
check4 s = s ``regex [:alpha:]''

test4a = assertTrue "check4a" (check4 "a")
test4b = assertTrue "check4b" (check4 "A")
test4c = assertTrue "check4c" (not (check4 "0"))
test4d = assertTrue "check4d" (not (check4 "ab"))


check5 :: String -> Bool
check5 s = s ``regex [a-z]+''

test5a = assertTrue "check5a" (check5 "a")
test5b = assertTrue "check5b" (check5 "abc")
test5c = assertTrue "check5c" (not (check5 "abc42"))
test5d = assertTrue "check5d" (not (check5 ""))

-- Tests with parameterized regular expressions:

pregexp1 :: Ord a => [a] -> a -> a -> Bool
pregexp1 s v1 v2 = s ``regex [<v1>-<v2>]*''

test7a = assertTrue "pregexp1a" (pregexp1 "abc" 'a' 'z')
test7b = assertTrue "pregexp1b" (pregexp1 "ABC" 'A' 'Z')
test7c = assertTrue "pregexp1c" (not (pregexp1 "abc" 'A' 'Z'))


pregexp2 :: Ord a => [a] -> a -> a -> Bool
pregexp2 s v1 v2 = s ``regex (<v1>|<v2>)*''

test8a = assertTrue "pregexp2a" (pregexp2 "abaabbb" 'a' 'b')
test8b = assertTrue "pregexp2b" (not (pregexp2 "abaabbb" 'a' 'z'))


-- A regular expression containing a complex Curry expression:
check6 :: Bool
check6 = "a" ``regex <((\x -\> x) 'a')>'' 

test9 = assertTrue "check6" check6

