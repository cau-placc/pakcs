------------------------------------------------------------------------
--- This module contains a prototypical implementation of
--- set functions in PAKCS. The general idea of set functions
--- is described in:
---
--- S. Antoy, M. Hanus: Set Functions for Functional Logic Programming
--- Proc. 11th International Conference on Principles and Practice
--- of Declarative Programming (PPDP'09), pp. 73-82, ACM Press, 2009
---
--- Intuition: If f is an n-ary function, then (setn f) is a set-valued
--- function that collects all non-determinism caused by f (but not
--- the non-determinism caused by evaluating arguments!) in a set.
--- Thus, (setn f a1 ... an) returns the set of all
--- values of (f b1 ... bn) where b1,...,bn are values
--- of the arguments a1,...,an (i.e., the arguments are
--- evaluated "outside" this capsule so that the non-determinism
--- caused by evaluating these arguments is not captured in this capsule
--- but yields several results for (setn...).
--- Similarly, logical variables occuring in a1,...,an are not bound
--- inside this capsule (but causes a suspension until they are bound).
--- The set of values returned by a set function is represented
--- by an abstract type "Values" on which several operations are
--- defined in this module.
---
--- Restrictions:
--- 1. The set is a multiset, i.e., it might contain multiple values.
--- 2. The multiset of values is completely evaluated when demanded.
---    Thus, if it is infinite, its evaluation will not terminate
---    even if only some elements (e.g., for a containment test)
---    are demanded. However, for the emptiness test, at most one
---    value will be computed
--- 3. The arguments of a set function are strictly evaluated before
---    the set functions itself will be evaluated.
---
--- Since this implementation is restricted and prototypical,
--- the interface is not stable and might change.
---
--- @author Michael Hanus
--- @version Fri Apr 15 03:40:12 CEST 2011
------------------------------------------------------------------------

module SetFunctions
         (set0,set1,set2,set3,set4,set5,set6,set7,
          Values,isEmpty,valueOf,contains,
          mapValues,foldValues,minValue,maxValue,
          values2list,printValues,sortValues,sortValuesBy)
 where

import Sort(mergeSort)

--- Combinator to transform a 0-ary function into a corresponding set function.
set0 :: b -> Values b
set0 f = Values (hasNoValue f) (findall (=:=f))

--- Combinator to transform a unary function into a corresponding set function.
set1 :: (a1 -> b) -> a1 -> Values b
set1 f x | x=:=x = Values (hasNoValue (f x)) (findall (=:=(f x)))

--- Combinator to transform a binary function into a corresponding set function.
set2 :: (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2 f x1 x2
  | x1=:=x1 & x2=:=x2
  = Values (hasNoValue (f x1 x2)) (findall (=:=(f x1 x2)))

--- Combinator to transform a function of arity 3
--- into a corresponding set function.
set3 :: (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3 f x1 x2 x3
  | x1=:=x1 & x2=:=x2 & x3=:=x3
  = Values (hasNoValue (f x1 x2 x3)) (findall (=:=(f x1 x2 x3)))

--- Combinator to transform a function of arity 4
--- into a corresponding set function.
set4 :: (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> Values b
set4 f x1 x2 x3 x4
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4
  = Values (hasNoValue (f x1 x2 x3 x4)) (findall (=:=(f x1 x2 x3 x4)))

--- Combinator to transform a function of arity 5
--- into a corresponding set function.
set5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5 f x1 x2 x3 x4 x5
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4 & x5=:=x5
  = Values (hasNoValue (f x1 x2 x3 x4 x5)) (findall (=:=(f x1 x2 x3 x4 x5)))

--- Combinator to transform a function of arity 6
--- into a corresponding set function.
set6 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6 f x1 x2 x3 x4 x5 x6
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4 & x5=:=x5 & x6=:=x6
  = Values (hasNoValue (f x1 x2 x3 x4 x5 x6))
           (findall (=:=(f x1 x2 x3 x4 x5 x6)))

--- Combinator to transform a function of arity 7
--- into a corresponding set function.
set7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7 f x1 x2 x3 x4 x5 x6 x7
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4 & x5=:=x5 & x6=:=x6 & x7=:=x7
  = Values (hasNoValue (f x1 x2 x3 x4 x5 x6 x7))
           (findall (=:=(f x1 x2 x3 x4 x5 x6 x7)))


------------------------------------------------------------------------
--- Abstract type representing multisets of values.

data Values a = Values Bool [a]

-- The first component is true iff the multiset is empty (this enables
-- also an emptiness check for infinite sets of values).
-- The second component contains the set of all values (it will be
-- completely evaluated if its value is demanded).

--- Test whether some expression has a value.
--- This implementation is specific to PAKCS in order to
--- work also for infinite result sets.
hasNoValue :: _ -> Bool
hasNoValue e = null (findall (=:= (findfirst (=:=e))))

--- Is a multiset of values empty?
isEmpty :: Values _ -> Bool
isEmpty (Values empty _) = empty

--- Is some value an element of a multiset of values?
valueOf :: a -> Values a -> Bool
valueOf e (Values _ s) = e `elem` s

--- Do not use. Use valueOf!
contains :: a -> Values a -> Bool
contains = valueOf

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
mapValues :: (a -> b) -> Values a -> Values b
mapValues f (Values empty s) = Values empty (map f s)

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
foldValues :: (a -> a -> a) -> a -> Values a -> a
foldValues f z (Values _ s) = foldr f z s

--- Returns the minimal element of a non-empty multiset of values
--- with respect to a given total ordering on the elements.
minValue :: (a -> a -> Bool) -> Values a -> a
minValue leq (Values _ s) = minOf s
 where
  minOf [x] = x
  minOf (x:y:ys) = let m1 = minOf (y:ys)
                    in if leq x m1 then x else m1

--- Returns the maximal element of a non-empty multiset of value
--- with respect to a given total ordering on the elements.
maxValue :: (a -> a -> Bool) -> Values a -> a
maxValue leq (Values _ s) = maxOf s
 where
  maxOf [x] = x
  maxOf (x:y:ys) = let m1 = maxOf (y:ys)
                    in if leq x m1 then m1 else x

--- Puts all elements of a multiset of values in a list.
--- Since the order of the elements in the list might depend on
--- the time of the computation, this operation is an I/O action.
values2list :: Values a -> IO [a]
values2list (Values _ s) = return s

--- Prints all elements of a multiset of values.
printValues :: Values _ -> IO ()
printValues s = values2list s >>= mapIO_ print

--- Transforms a multiset of values into a list sorted by
--- the standard term ordering. As a consequence, the multiset of values
--- is completely evaluated.
sortValues :: Values a -> [a]
sortValues = sortValuesBy (<=)

--- Transforms a multiset of values into a list sorted by a given ordering
--- on the values. As a consequence, the multiset of values
--- is completely evaluated.
--- In order to ensure that the result of this operation is independent of the
--- evaluation order, the given ordering must be a total order.
sortValuesBy :: (a -> a -> Bool) -> Values a -> [a]
sortValuesBy leq (Values _ s) = mergeSort leq s

------------------------------------------------------------------------
