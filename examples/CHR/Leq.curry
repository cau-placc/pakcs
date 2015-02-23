----------------------------------------------------------------------
--- CHR(Curry): the classical Leq example
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- Leq constraints:
data LEQ a = Leq a a

leq = toGoal2 Leq

reflexivity  [x,y]   = leq x y <=> x .=. y  |> true
antisymmetry [x,y]   = leq x y /\ leq y x  <=> x .=. y
idempotence  [x,y]   = leq x y \\ leq x y  <=> true
transitivity [x,y,z] = leq x y /\ leq y z  ==> leq x z

-- This rule can be used to solve leq on known ground values:
leqval [x,y] = leq x y <=> ground x /\ ground y |> x .<=. y

runLEQ = runCHR [reflexivity,antisymmetry,idempotence,transitivity]

main10 x        = runLEQ $ leq 1 x /\ leq x 1
main11 x y z    = runLEQ $ leq x y /\ leq y z /\ leq z x
main12 x y z z' = runLEQ $ leq x y /\ leq z z'

compileLeq =
  compileCHR "LEQCHR" [reflexivity,antisymmetry,idempotence,transitivity]
