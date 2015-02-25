----------------------------------------------------------------------
--- CHR(Curry): union-find algorithm in CHR
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- Naive union-find algorithm (according to Schrijvers/Fruehwirth TPLP 2006)
-- Advantage to Prolog CHR: polymorphic type-safe rules!
data UF a = Root a | Arrow a a -- data structure
          | Make a | Union a a | Find a a | Link a a -- operations

root  = toGoal1 Root
make  = toGoal1 Make
(~>)  = toGoal2 Arrow
union = toGoal2 Union
find  = toGoal2 Find 
link  = toGoal2 Link 

makeI    [a]       = make a <=> root a
unionI   [a,b,x,y] = union a b <=> find a x /\ find b y /\ link x y
findNode [a,b,x]   = a ~> b \\ find a x <=> find b x
findRoot [a,x]     = root a \\ find a x <=> x .=. a
linkEq   [a]       = link a a <=> true
linkTo   [a,b]     = link a b /\ root a /\ root b <=> b ~> a /\ root a

runUF = runCHR [makeI,unionI,findNode,findRoot,linkEq,linkTo]

main60 = runUF $ andCHR [make 1, make 2, make 3, make 4, union 1 2]
--> [Root 1,Arrow 2 1,Root 4,Root 3]
main61 x = runUF $ andCHR [2 ~> 1,root 1,root 4,root 3, find 2 x] --> x=1

main62 x y = runUF $ andCHR [2 ~> 1,root 1,root 4,root 3, find 2 x,
                             union 3 2, find 2 y]
--> x=1, y=3

main63 = runUF $ andCHR [make 1, make 2, make 3, make 4, make 5,
                         union 1 2, union 3 4, union 5 3]

main64 x y = runUF $ andCHR
  [make 1, make 2, make 3, make 4, make 5,
   union 1 2, union 3 4, union 5 3, find 2 x, find 4 y] --> x=1, y=5

-- union/find on character elements:
main65 x y =
  runUF $ andCHR $ map make "abcde" ++
            [union 'a' 'b', union 'c' 'd', union 'e' 'c',
             find 'b' x, find 'd' y] --> x='a', y='e'


compileUF = compileCHR "UFCHR" [makeI,unionI,findNode,findRoot,linkEq,linkTo]
-- solveCHR $ andC [make 1, make 2, make 3, make 4, make 5, union 1 2, union 3 4, union 5 3, find 2 x, find 4 y]
--> x=1, y=5
