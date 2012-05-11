--- This library provides a Boolean Constraint Solver based on BDDs.
---
--- @author Sebastian Fischer
--- @version May 2006
module CLPB (

  Boolean, true, false,

  neg, (.&&), (.||), (./=), (.==), (.<=), (.>=), (.<), (.>), count, exists,

  satisfied, check, bound, simplify, evaluate

  ) where

infixr 3 .&&
infixr 2 .||
infix  1 ./=, .==

data Boolean = B Int  -- abstract

--- The always satisfied constraint
true :: Boolean
true = B 1

--- The never satisfied constraint
false :: Boolean
false = B 0

--- Result is true iff argument is false.
neg :: Boolean -> Boolean
neg (B x) = B (prim_neg $! x)

prim_neg :: Int -> Int
prim_neg external

--- Result is true iff both arguments are true.
(.&&) :: Boolean -> Boolean -> Boolean
B x .&& B y = B ((prim_and $!y) $! x)

prim_and :: Int -> Int -> Int
prim_and external

--- Result is true iff at least one argument is true.
(.||) :: Boolean -> Boolean -> Boolean
B x .|| B y = B ((prim_or $!y) $! x)

prim_or :: Int -> Int -> Int
prim_or external

--- Result is true iff exactly one argument is true.
(./=) :: Boolean -> Boolean -> Boolean
B x ./= B y = B ((prim_xor $!y) $! x)

prim_xor :: Int -> Int -> Int
prim_xor external

--- Result is true iff both arguments are equal.
(.==) :: Boolean -> Boolean -> Boolean
x .== y = neg x ./= y

--- Result is true iff the first argument implies the second.
(.<=) :: Boolean -> Boolean -> Boolean
x .<= y = neg x .|| y

--- Result is true iff the second argument implies the first.
(.>=) :: Boolean -> Boolean -> Boolean
x .>= y = y .<= x

--- Result is true iff the first argument is false and the second is true.
(.<) :: Boolean -> Boolean -> Boolean
x .< y = neg x .&& y

--- Result is true iff the first argument is true and the second is false.
(.>) :: Boolean -> Boolean -> Boolean
x .> y = y .< x

--- Result is true iff the count of valid constraints in the first list
--- is an element of the second list.
count :: [Boolean] -> [Int] -> Boolean
count bs ns = B ((card $!! map ensureNotFree (ensureSpine ns))
                       $!! map int (ensureSpine bs))

card :: [Int] -> [Int] -> Int
card external

--- Result is true, if the first argument is a variable which can be
--- instantiated such that the second argument is true.
exists :: Boolean -> Boolean -> Boolean
exists (B v) (B b) = B ((prim_exists $!! v) $!! b)

prim_exists :: Int -> Int -> Int
prim_exists external

--- Checks the consistency of the constraint with regard to the accumulated
--- constraints, and, if the check succeeds, tells the constraint.
satisfied :: Boolean -> Success
satisfied (B b) = sat $!! b

sat :: Int -> Success
sat external

--- Asks whether the argument (or its negation) is now entailed by the
--- accumulated constraints. Fails if it is not. 
check :: Boolean -> Bool
check (B b) = (prim_check $!! b) == 1

prim_check :: Int -> Int
prim_check external

--- Instantiates given variables with regard to the accumulated constraints.
bound :: [Boolean] -> Success
bound bs = labeling $!! map int (ensureSpine bs)

labeling :: [Int] -> Success
labeling external

--- Simplifies the argument with regard to the accumulated constraints.
simplify :: Boolean -> Boolean
simplify b | satisfied (a .== b) = a where a free

--- Evaluates the argument with regard to the accumulated constraints.
evaluate :: Boolean -> Bool
evaluate x | bound [y] = int y == 1
 where
  y = simplify x

-- private
int :: Boolean -> Int
int (B x) = x

