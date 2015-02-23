----------------------------------------------------------------------
--- CHR(Curry): finite domain constraints
---
--- This example also shows how operations defined in Curry can be used
--- as primitive constraints in CHR(Curry)
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR
import qualified List

----------------------------------------------------------------------
-- Finite domain constraints in CHR (influence from SWI-Prolog manual)
data FDom = Dom Int [Int] | Diff Int Int

dom  = toGoal2 Dom
diff = toGoal2 Diff
intersect xs ys zs = anyPrim (\() -> zs =:= List.intersect xs ys)
delete x ys zs = anyPrim $ \() -> zs =:= List.delete x ys
member x xs = anyPrim $ \() -> contains x xs
 where contains z (y:ys) = z=:=y ? contains z ys

-- Rules for `dom` constraint:
dom1 [x]   = dom x [] <=> fail
dom2 [x,y] = dom x [y] <=> x .=. y
dom3 [x]   = dom x xs <=> nonvar x |> member x xs     where xs free
dom4 [x]   = dom x d1 /\ dom x d2 <=> intersect d1 d2 d3 /\ dom x d3
  where d1,d2,d3 free

-- Rules for `diff` constraint:
diff1 [x]   = diff x x <=> fail
diff2 [x,y] = diff x y <=> nonvar x /\ nonvar y |> x ./=. y
diff3 [x,y] = diff x y /\ dom x d1 <=> nonvar y |> delete y d1 d2 /\ dom x d2
  where d1,d2 free
diff4 [x,y] = diff y x /\ dom x d1 <=> nonvar y |> delete y d1 d2 /\ dom x d2
  where d1,d2 free

-- Define domains for a list of variables:
domain :: [Int] -> Int -> Int -> Goal Int FDom
domain xs a b = allCHR (\x -> dom x [a .. b]) xs

-- Define allDifferent constraint for list of arguments:
allDifferent :: [Int] -> Goal Int FDom
allDifferent = andCHR . allDiff
 where
   allDiff [] = []
   allDiff (x:xs) = map (diff x) xs ++ allDiff xs

-- Labeling: if constraints contain (Dom x d), then add the disjunction
-- of (x .=. v), for all values v in d, and solve again.
labeling :: (Goal Int FDom -> [FDom]) -> Goal Int FDom -> [FDom]
labeling solver goal = tryLabeling model
 where
  model = solver goal -- compute model without labeling

  tryLabeling [] = model
  tryLabeling (y:ys) = case y of
    Dom x d -> labeling solver (eqDom x d /\ chrsToGoal model)
    _       -> tryLabeling ys

  eqDom x xs = foldr1 (?) (map (\d -> x .=. d) xs)

runFD1 = runCHR [dom1,dom2,dom3,dom4]
runFD2 = runCHR [dom1,dom2,dom3,dom4,diff1,diff2,diff3,diff4]

main50 x y     = runFD1 $ dom x [1] /\ dom y [1,2] /\ x .=. y --> x=y=1
main51 x       = runFD1 $ dom x [1,2,3] /\ dom x [3,4,5]  --> x=3
main52 [x,y,z] = runFD1 $ domain [x,y,z] 1 3 /\ x .=. y /\ y .=. z /\ y .=. 2

main53 [x,y,z] = labeling runFD2 $ domain [x,y,z] 1 3 /\ allDifferent [x,y,z]

-- Map coloring:
{-
 This is our actual map:

 --------------------------
 |                |       |
 |       |--------|       |
 |       |   L2   |       |
 |  L1   |--------|  L4   |
 |       |        |       |
 |       |   L3   |       |
 |       |        |       |
 --------------------------
-}
main55 [l1,l2,l3,l4] = labeling runFD2 $
  domain [l1,l2,l3,l4] 1 4 /\
  diff l1 l4 /\ diff l1 l2 /\ diff l1 l3 /\ diff l2 l3 /\
  diff l2 l4 /\ diff l3 l4 /\ l2 .=. 3  -- fix color of L2


compileFD = compileCHR "FDCHR" [dom1,dom2,dom3,dom4,diff1,diff2,diff3,diff4]
