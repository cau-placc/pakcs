------------------------------------------------------------------------------
-- Analysis for solution completeness:
-- check whether functions are solution complete, i.e., calls only
-- non-rigid functions
--
-- Michael Hanus, January 2004
------------------------------------------------------------------------------

module SolutionComplete(analyseSolutionComplete)   where

import FlatCurry
import List
import Dependency

------------------------------------------------------------------------------
-- The operational completeness analysis must be applied to complete programs,
-- i.e., modules together with all their imported modules.
-- It assigns to a FlatCurry program the list of all qualified function names
-- together with a flag which is True if this function is operationally
-- complete, i.e., does not call a rigid function.

analyseSolutionComplete :: [FuncDecl] -> [(QName,Bool)]
analyseSolutionComplete = analyseWithDependencies isFlexDefined and

-- (isFlexDefined fundecl f):
-- Is a function f defined by a flexible rule?
isFlexDefined :: FuncDecl -> Bool
isFlexDefined (Func _ _ _ _ def) = isFlexRule def

isFlexRule (Rule _ e) = isFlexExpr e
isFlexRule (External f) =
   f `elem` ["Prelude.=:=","Prelude.success","Prelude.&",
             "Prelude.&>","Prelude.return"]

-- Checks whether an expression is flexible, i.e., can only suspend
-- because of calls to other possibly rigid functions.

isFlexExpr :: Expr -> Bool
isFlexExpr (Var _)           = True
isFlexExpr (Lit _)           = True
isFlexExpr (Comb _ f args) =
       f/=("Prelude","apply") -- apply suspends if arg 1 is unbound
    && f/=("Prelude","commit")
    && all isFlexExpr args
isFlexExpr (Free _ e)        = isFlexExpr e
isFlexExpr (Let bs e)        = all isFlexExpr (map snd bs) && isFlexExpr e
isFlexExpr (Or e1 e2)        = isFlexExpr e1 && isFlexExpr e2
isFlexExpr (Case ctype e bs) = ctype==Flex &&
                         all isFlexExpr (e : map (\(Branch _ be)->be) bs)


-- end of SolutionComplete
