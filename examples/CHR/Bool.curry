----------------------------------------------------------------------
--- CHR(Curry): Boolean constraint solver
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- Boolean constraints:
data BoolCHR = And Bool Bool Bool | Or Bool Bool Bool | Neg Bool Bool

and = toGoal3 And
or  = toGoal3 Or
neg = toGoal2 Neg

andRules =
 [\[x,y,z] -> and x y z <=> x .=. False |> z .=. False
 ,\[x,y,z] -> and x y z <=> y .=. False |> z .=. False
 ,\[x,y,z] -> and x y z <=> x .=. True  |> y .=. z
 ,\[x,y,z] -> and x y z <=> y .=. True  |> x .=. z
 ,\[x,y,z] -> and x y z <=> z .=. True  |> x .=. True /\ y .=. True
 ,\[x,y,z] -> and x y z <=> x .=. y     |> y .=. z
 ]

orRules =
 [\[x,y,z] -> or x y z <=> x .=. False |> z .=. y
 ,\[x,y,z] -> or x y z <=> y .=. False |> z .=. x
 ,\[x,y,z] -> or x y z <=> x .=. True  |> z .=. True
 ,\[x,y,z] -> or x y z <=> y .=. True  |> z .=. True
 ,\[x,y,z] -> or x y z <=> z .=. False |> x .=. False /\ y .=. False
 ,\[x,y,z] -> or x y z <=> x .=. y |> y .=. z]

negRules =
 [\[x] -> neg False x  <=> x .=. True
 ,\[x] -> neg x False  <=> x .=. True
 ,\[x] -> neg True  x  <=> x .=. False
 ,\[x] -> neg x True   <=> x .=. False
 ,\[x] -> neg x x      <=> fail
 ]

boolRules = andRules ++ orRules ++ negRules

main20 x y z = runCHR boolRules $ and x y z /\ neg False z --> x=y=z=True

-- Application: half adder:
halfAdder a b s c = andCHR [or a b ab, and a b c, neg c nc, and ab nc s]
 where ab,nc free

-- Analyze inputs for carry bit:
main21 a b s c = runCHR boolRules (halfAdder a b s c /\ c .=. True)
main22 a b s c = runCHR boolRules (halfAdder a b s c /\ c .=. False)

----------------------------------------------------------------------
