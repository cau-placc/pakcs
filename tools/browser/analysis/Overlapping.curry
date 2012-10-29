------------------------------------------------------------------------------
-- Overlapping analysis:
-- check whether functions are defined with overlapping left-hand sides
-- (i.e., whether they are defined with OR expressions)
--
-- Michael Hanus, February 2012
------------------------------------------------------------------------------

module Overlapping(isOverlappingFunction,orInExpr) where

import FlatCurry

------------------------------------------------------------------------------
-- The overlapping analysis can be applied to individual functions.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this function is defined with overlapping left-hand sides.

isOverlappingFunction :: FuncDecl -> Bool
isOverlappingFunction (Func _ _ _ _ (Rule _ e))   = orInExpr e
isOverlappingFunction (Func _ _ _ _ (External _)) = False

--------------------------------------------------------------------------
-- Check an expression for occurrences of OR:
orInExpr :: Expr -> Bool
orInExpr (Var _) = False
orInExpr (Lit _) = False
orInExpr (Comb _ f es) = f==("Prelude","?") || foldr (||) False (map orInExpr es)
orInExpr (Free _ e) = orInExpr e
orInExpr (Let bs e) = any orInExpr (map snd bs) || orInExpr e
orInExpr (Or _ _) = True
orInExpr (Case _ e bs) = orInExpr e || any orInBranch bs
                   where orInBranch (Branch _ be) = orInExpr be

