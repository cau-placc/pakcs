---------------------------------------------------------------------------
-- A simple example for the use of the functional logic parser combinators:
--
-- A parser for palindromes over the alphabet 'a' and 'b'

import Parser
import AllSolutions

-- Terminals:
a = terminal 'a'
b = terminal 'b'

-- Palindromes:
pali = empty <|> a <|> b <|> a<*>pali<*>a <|> b<*>pali<*>b

{-
Examples:

Check correctness of a sentence:

> pali "abaaba" =:= []


Generate palindromes:

> pali [x,y,z] =:= []  where x,y,z free
-}


-- Generate list of all palindromes of length 5:
pali5 = getAllSolutions (\[x1,x2,x3,x4,x5] -> pali [x1,x2,x3,x4,x5] =:= [])

-- Generate palindromes of a given length:
palin len = getAllSolutions (\xs -> strlen xs len & pali xs =:= [])
 where
   -- Has a list a given length?
   strlen [] 0 = True
   strlen (_:xs) n | n>0 = strlen xs (n-1)
