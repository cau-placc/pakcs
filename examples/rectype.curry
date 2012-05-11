-- This program test whether the Curry implementation can
-- deal with recursive types provided that they are given
-- by the user (cf. Section 4.2 of the Curry Report):
--
-- This program is well typed but traditional Hindley/Milner
-- type inferencer cannot derive the types if the type definition
-- of 'g' is omitted:

f :: [a] -> [a]
f x = if length x == 0 then fst (g x x) else x

g :: [a] -> [b] -> ([a],[b])
g x y = (f x , f y)

h :: ([Int],[Bool])
h = g [3,4] [True,False]
