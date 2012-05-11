---------------------------------------------------------------------------
-- A simple example for the use of the functional logic parser combinators:
-- We define a parser for arithmetic expressions over natural numbers.
-- The presentation of this parser is the value of the expression.

import Parser
import Char

expression   =  term t <*> plus_minus op <*> expression e  >>> (op t e)
           <||> term
 where op,t,e free

term         =  factor f <*> prod_div op <*> term t        >>> (op f t)
           <||> factor
 where op,f,t free

factor       =  terminal '(' <*> expression e <*> terminal ')'  >>> e
           <||> num
 where e free

plus_minus   =  terminal '+'  >>> (+)
           <||> terminal '-'  >>> (-)

prod_div     =  terminal '*'  >>> (*)
           <||> terminal '/'  >>> div

num = some digit l >>> numeric_value l
  where l free
        numeric_value ds = foldl1 ((+) . (10*)) (map (\c->ord c - ord '0') ds)

digit = satisfy isDigit


-- example application: expression val "(10+5*2)/4" =:= [] where val free
