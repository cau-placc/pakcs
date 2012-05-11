-- This module defines list concatenation several times

module ModConc(conc,(+),(.+.)) where

infixr 5 +,.+.

(+) :: [a] -> [a] -> [a]
a + b = a Prelude.++ b

(.+.) :: [a] -> [a] -> [a]
a .+. b = a + b

conc :: [a] -> [a] -> [a]
conc = (+)


goal1 = [1] + [2]

goal2 = [1] .+. [2]

goal3 = conc [1] [2]
