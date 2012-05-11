----------------------------------------------------------------------------
--- Some useful operations for constraint programming.
---
--- @author Michael Hanus
--- @version September 2010
----------------------------------------------------------------------------

module Constraint((<:),(>:),(<=:),(>=:),andC,orC,allC,anyC)
 where

infix  4 <:, >:, <=:, >=:

--- Less-than on ground data terms as a constraint.
(<:)   :: a -> a -> Success
x <: y = (x<y) =:= True

--- Greater-than on ground data terms as a constraint.
(>:)   :: a -> a -> Success
x >: y = (x>y) =:= True

--- Less-or-equal on ground data terms as a constraint.
(<=:)  :: a -> a -> Success
x <=: y = (x<=y) =:= True

--- Greater-or-equal on ground data terms as a constraint.
(>=:)  :: a -> a -> Success
x >=: y = (x>=y) =:= True

--- Evaluates the conjunction of a list of constraints.
andC :: [Success] -> Success
andC = foldr (&) success

--- Evaluates the disjunction of a list of constraints.
orC :: [Success] -> Success
orC = foldr (?) failed

--- Is a given constraint abstraction satisfied by all elements in a list?
allC :: (a -> Success) -> [a] -> Success
allC c = andC . map c

--- Is there an element in a list satisfying a given constraint?
anyC :: (a -> Success) -> [a] -> Success
anyC c = orC . map c

-- end module Constraint
