---------------------------------------------------------------------------------
-- Test the extension of PAKCS for function patterns as described in:
-- Sergio Antoy, Michael Hanus:
-- Declarative Programming with Function Patterns
-- Proceedings of the International Symposium on Logic-based Program Synthesis
-- and Transformation (LOPSTR'05), appeared in Springer LNCS 3901, 2006
-- http://www-ps.informatik.uni-kiel.de/~mh/papers/LOPSTR05.html
--
-- Note: this requires the setting "curryextensions=yes" in
--  ~/.kics2rc or ~/.pakcsrc
--------------------------------------------------------------------------------

import Assertion
import AllSolutions

someValue e = findfirst (=:=e)

--------------------------------------------------------------------------------
-- define operation last by a function pattern:
last :: [a] -> a
last (_++[x]) = x

test1 = assertEqual  "last1" (last (map (+1) [1..200])) 201

test2 = assertValues "last2" (last (take 10000 (repeat failed) ++ [1])) [1]

--------------------------------------------------------------------------------
-- define a palindrome constraint:
pali :: [a] -> Success
pali (xs ++ reverse xs) = success
--pali l | xs ++ reverse xs =:<= l = success      where xs free

--test3 = assertEqual "palindrome1" (pali "otto") success
--test4 = assertValues "palindrome2" (pali "toto") []

--------------------------------------------------------------------------------
-- define tree transformations and search by function patterns:
data Exp = Lit Int | Var [Char] | Add Exp Exp | Mul Exp Exp

evalTo e = Add (Lit 0) e
         ? Add e (Lit 0)
         ? Mul (Lit 1) e
         ? Mul e (Lit 1)

replace _         []    x = x
replace (Add l r) (1:p) x = Add (replace l p x) r
replace (Add l r) (2:p) x = Add l (replace r p x)
replace (Mul l r) (1:p) x = Mul (replace l p x) r
replace (Mul l r) (2:p) x = Mul l (replace r p x)

simplify :: Exp -> Exp
simplify (replace c p (evalTo x)) = replace c p x
--simplify e | (replace c p (evalTo x)) =:<= e  = replace c p x  where c,p,x free

-- Apply a transformation to some data structure as long as it is defined:
transformAll :: (a -> a) -> a -> IO a
transformAll trans term =
   (getOneValue (trans term)) >>= maybe (return term) (transformAll trans)


test5 = assertEqual "simplify1" (simplify (Mul (Lit 1) (Var "x")))
                                (Var "x")
test6 = assertIO    "simplify2" (transformAll simplify exp) (Var "x")

exp = Mul (Lit 1) (Add (Var "x") (Lit 0))

exp_n n e = if n==0 then e else Add (exp_n (n-1) e) (exp_n (n-1) e)

bigexp | e =:= exp_n 8 exp = e  where e free

-- return some variable occurring in an expression:
varInExp :: Exp -> String
varInExp (replace _ _ (Var v)) = v
--varInExp e | (replace c p (Var v)) =:<= e = v   where c,p,v free

getVarsInExp :: Exp -> IO [String]
getVarsInExp e = getAllValues (varInExp e)

test7 = assertIO "vars" (getVarsInExp bigexp >>= return . length) 256


--------------------------------------------------------------------------------
-- Dijsktra's Dutch National Flag problem with function patterns

data Color = Red | White | Blue

solve (x++[White]++y++[Red  ]++z) = solve (x++[Red]++y++[White]++z)
solve (x++[Blue ]++y++[Red  ]++z) = solve (x++[Red]++y++[Blue]++z)
solve (x++[Blue ]++y++[White]++z) = solve (x++[White]++y++[Blue]++z)
solve flag | isDutchFlag flag = flag
 where isDutchFlag (uni Red ++ uni White ++ uni Blue) = success
       --isDutchFlag flag | uni Red ++ uni White ++ uni Blue =:<= flag = success
       uni _ = []
       uni color = color : uni color

test8 = assertEqual "Dutch Flag"
                    (someValue (solve [White,Red,White,Blue,Red,Blue,White]))
                    [Red,Red,White,White,White,Blue,Blue]

--------------------------------------------------------------------------------
