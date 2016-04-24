----------------------------------------------------------------------
--- CHR(Curry): solving linear arithmetic equations with Gaussian elimination
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR
import Float
import Sort(mergeSortBy)
import Unsafe(compareAnyTerm)

infix  7 :*:
infixr 6 :+:
infix  5 :=:

----------------------------------------------------------------------
-- Gaussian elimination to solve linear equalities

type Poly = [(Float,Float)] -- a polynom is a list of (coeff,variable)

data Gauss = Equals Poly Float 
           | ArithOp (Float->Float->Float) Float Float Float

-- Notational abbreviations:
(:*:) :: Float -> Float -> Poly
a :*: x = [(a,x)]

(:+:) :: Poly -> Poly -> Poly
p :+: q = p ++ q

-- CHR constraints:
(:=:)   = toGoal2 Equals
arithop = toGoal4 ArithOp
plus    = arithop (+.)
mult    = arithop (*.)

-- Specific primitive constraints:
-- Find and delete some element in a list.
select x xs zs = anyPrim $ \() -> del xs zs
 where
   del (y:p) q = (y =:= x & q =:= p)
               ? (let q1 free in del p q1 & q =:= y:q1)

-- Multiply a polynomial p with a constant c.
polyMult c p q = anyPrim $ \() -> q =:= map (\ (a,x) -> (a*.c,x)) p

-- Add two polynomials
polyAdd ps qs rs = anyPrim $ \() ->
 let ops = mergeSortBy (\ (_,x) (_,y) -> compareAnyTerm x y /= GT) ps
     oqs = mergeSortBy (\ (_,x) (_,y) -> compareAnyTerm x y /= GT) qs
  in rs =:= addP ops oqs
 where
  addP [] ys = ys
  addP (x:xs) [] = x:xs
  addP ((i,x):xs) ((j,y):ys)
    | compareAnyTerm x y == EQ = (i+.j,x) : addP xs ys
    | compareAnyTerm x y == LT = (i,x) : addP xs ((j,y):ys)
    | otherwise                = (j,y) : addP ((i,x):xs) ys

-- CHR rules:
-- main rule: eliminate a term in a polynomial:
eliminate [a,x,c1,c2,b,c,c1c,c3] = let p,p1,p2,p1c,p3 free in
  ((a,x):p1) :=: c1 \\ p :=: c2 <=> select (b,x) p p2 |>
   c .=. 0.0 -. b /. a /\
   mult c1  c c1c /\ polyMult c p1 p1c /\
   plus c1c c2 c3 /\ polyAdd p2 p1c p3 /\
   p3 :=: c3

-- remove constant monomials:
constM [a,x,c] = let p,q free in
  p :=: c <=> select (a,x) p q /\ nonvar x |> q :=: (c -. a *. x)

-- empty polynomials have zero value:
emptyP [c] = [] :=: c <=> c .=. 0.0

-- bind a variable if unique:
bindVar [a,x,c] = [(a,x)] :=: c <=> x .=. c /. a

-- Simplify arithmetic relations:
arithrule [x,y,z] = arithop op x y z <=> nonvar x /\ nonvar y |>
                                         z .=. x `op` y
  where op free

runGauss = runCHR [arithrule,emptyP,constM,eliminate,bindVar]

main80 x y = runGauss $ 3:*:x :=: 6 /\ 2:*:x :+: 6:*:y :=: 10.0
main81 x y = runGauss $
  1.0:*:x :+: 1.0:*:y :=: 7.0 /\ 1.0:*:x :+: (-1.0):*:y :=: 3.0
main82 x y = runGauss $
  1.0:*:x :+: (-1.0):*:y :=: 0.0 /\ 1.0:*:x :+: 1.0:*:y :=: 4.0

main83 x = runGauss $ 2:*:x :+: 4:*:2 :=: 10.0

-- Application: circuit analysis:

data Circuit = Resistor Float
             | Series Circuit Circuit
             | Parallel Circuit Circuit

cvi :: Circuit -> Float -> Float -> Goal Float Gauss

cvi (Resistor r) v i =  r :*: i :+: (-1) :*: v :=: 0

cvi (Series   c1 c2) v i = let v1,v2 free in
  cvi c1 v1 i /\ cvi c2 v2 i /\ 1:*:v1 :+: 1:*:v2 :+: (-1):*:v :=: 0

cvi (Parallel c1 c2) v i = let i1,i2 free in
  cvi c1 v i1 /\ cvi c2 v i2 /\ 1:*:i1 :+: 1:*:i2 :+: (-1):*:i :=: 0


main85 i = runGauss $ cvi (Series (Resistor 180) (Resistor 470)) 5 i
-- i=0.007692307692307692
main86 i = runGauss $ cvi (Parallel (Resistor 180) (Resistor 470)) 5 i
-- i=0.038416075650118

----------------------------------------------------------------------
compileGauss = compileCHR "GAUSSCHR" [arithrule,emptyP,constM,eliminate,bindVar]
-- [(3.0,x)] :=: 6.0 /\ [(2.0,x),(6.0,y)] :=: 10.0
--> x=2.0, y=1.0
