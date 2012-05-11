-----------------------------------------------------------------------------
-- Pattern completeness analysis for Curry programs
--
-- This analysis checks for each function in a Curry program  whether
-- this function is completely defined, i.e., reducible on all ground
-- constructor terms
--
-- Johannes Koj, October 2000
-- Michael Hanus, June 2006
-----------------------------------------------------------------------------

module PatternComplete(CompletenessType(..),
                       analyseCompleteness,
                       analyseTotallyDefined) where

import FlatCurry
import Dependency(analyseWithDependencies)

------------------------------------------------------------------------------
-- The completeness analysis must be applied to complete programs,
-- i.e., modules together with all their imported modules (although
-- functions are locally checked, the definition of all data types
-- used in the patterns are needed).
-- It assigns to a FlatCurry program the list of all qualified function names
-- together with a flag indicating whether this function is completely
-- defined on its input types (i.e., reducible for all ground data terms).

-- The possible outcomes of the completeness analysis:
data CompletenessType =
     Complete       -- completely defined
   | InComplete     -- incompletely defined
   | InCompleteOr   -- incompletely defined in each branch of an "Or"

analyseTotallyDefined :: [TypeDecl] -> [FuncDecl] -> [(QName,CompletenessType)]
analyseTotallyDefined types = analyseWithDependencies (analyseCompleteness types) cAnd
 where cAnd = foldr combineAndResults Complete

analyseCompleteness :: [TypeDecl] -> FuncDecl -> CompletenessType
analyseCompleteness types fdecl = anaFun fdecl
 where
  anaFun (Func _ _ _ _ (Rule _ e)) = isComplete types e
  anaFun (Func _ _ _ _ (External _)) = Complete

--isComplete :: [TypeDecl] -> [(Expr,TypeExpr)] -> Expr -> CompletenessType
isComplete _ (Var _)      = Complete
isComplete _ (Lit _)      = Complete
isComplete types (Comb _ f es) =
  if f==("Prelude","commit") && length es == 1
  then isComplete types (head es)
  else Complete
isComplete _ (Free _ _) = Complete
isComplete _ (Let _ _) = Complete
isComplete types (Or e1 e2) =
   combineOrResults (isComplete types e1) (isComplete types e2)
-- if there is no branch, it is incomplete:
isComplete _ (Case _ _ []) = InComplete
-- for literal branches we usually assume that not all alternatives are provided:
isComplete _ (Case _ _ (Branch (LPattern _)   _ : _)) = InComplete
isComplete types (Case _ _ (Branch (Pattern cons pvs) bexp : ces)) =
    checkAllCons (getConstructors cons types) (Branch (Pattern cons pvs) bexp : ces)
  where
    -- check for occurrences of all constructors in each case branch:
    checkAllCons []    _  = Complete
    checkAllCons (_:_) [] = InComplete
    checkAllCons (_:_) (Branch (LPattern _)   _ : _) = InComplete -- should not occur
    checkAllCons (c:cs) (Branch (Pattern i _) e : ps) =
        combineAndResults (checkAllCons (removeConstructor i (c:cs)) ps)
                          (isComplete types e)

-- Combines the completeness results in different Or branches.
combineOrResults Complete     _            = Complete
combineOrResults InComplete   Complete     = Complete
combineOrResults InComplete   InComplete   = InCompleteOr
combineOrResults InComplete   InCompleteOr = InCompleteOr
combineOrResults InCompleteOr Complete     = Complete
combineOrResults InCompleteOr InComplete   = InCompleteOr
combineOrResults InCompleteOr InCompleteOr = InCompleteOr

-- Combines the completeness results in different case branches.
combineAndResults InComplete   _            = InComplete
combineAndResults Complete     Complete     = Complete
combineAndResults Complete     InComplete   = InComplete
combineAndResults Complete     InCompleteOr = InCompleteOr
combineAndResults InCompleteOr Complete     = InCompleteOr
combineAndResults InCompleteOr InComplete   = InComplete
combineAndResults InCompleteOr InCompleteOr = InCompleteOr


-- get the list of all constructors of the same datatype for a given constructor:
getConstructors :: QName -> [TypeDecl] -> [ConsDecl]
getConstructors _ [] = error "Internal compiler error: case datatype not found!"
getConstructors cons (TypeSyn _ _ _ _ : types) = getConstructors cons types
getConstructors cons (Type _ _ _ cdecls : types) =
 if hasCons cdecls then cdecls
                   else getConstructors cons types
 where
   hasCons [] = False
   hasCons (Cons cs _ _ _ : cstrs) = cons==cs || hasCons cstrs

-- remove a constructor from a list of constructors
removeConstructor :: QName -> [ConsDecl] -> [ConsDecl]
removeConstructor c1 [] = error ("Constructor "++show c1++" not found!!")
removeConstructor c1 ((Cons c2 a vis ts):cs)
        | c1==c2    = cs
        | otherwise = (Cons c2 a vis ts):(removeConstructor c1 cs)
