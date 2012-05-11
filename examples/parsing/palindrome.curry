---------------------------------------------------------------------------
-- A simple example for the use of the functional logic parser combinators:
--
-- A parser for palindromes over the alphabet 'a' and 'b'

import Parser

a = terminal 'a'
b = terminal 'b'

pali = empty <|> a <|> b <|> a<*>pali<*>a <|> b<*>pali<*>b

{-
Examples:

Check correctness of a sentence:

 pali "abaaba" =:= []


Generate palindromes:

 pali [x,y,z] =:= []


Generate list of all palindromes of length 5:
-}

pali5 = findall (\[x1,x2,x3,x4,x5] -> pali [x1,x2,x3,x4,x5] =:= [])
