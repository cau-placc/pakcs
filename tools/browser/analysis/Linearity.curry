------------------------------------------------------------------------------
-- Linearity analysis:
-- check whether functions are defined by right-linear rules.
--
-- Michael Hanus, June 2005
------------------------------------------------------------------------------

module Linearity(analyseRightLinearity,hasRightLinearRules,linearExpr) where

import FlatCurry
import Maybe
import List
import Dependency(analyseWithDependencies)

------------------------------------------------------------------------------
-- The right-linearity analysis is a global analysis that must be applied to
-- the complete list of defined functions.
-- It assigns to a list of functions the list of all function names
-- together with a flag which is True if this function is right-linear, i.e.,
-- defined by right-linear rules and depend only on functions defined by
-- right-linear rules.

analyseRightLinearity :: [FuncDecl] -> [(QName,Bool)]
analyseRightLinearity = analyseWithDependencies hasRightLinearRules and

------------------------------------------------------------------------------
-- The right-linearity analysis can also be applied to individual functions.
-- It returns True for a function if it is defined by right-linear rules.

hasRightLinearRules :: FuncDecl -> Bool
hasRightLinearRules (Func _ _ _ _ rule) = isRightLinearRule rule

isRightLinearRule (Rule _ e)   = linearExpr e
isRightLinearRule (External _) = True

--------------------------------------------------------------------------
-- Check an expression for linearity:
linearExpr :: Expr -> Bool
linearExpr e = maybe False (const True) (linearVariables e)

-- Return list of variables in an expression, if it is linear, otherwise: Nothing
linearVariables :: Expr -> Maybe [Int]
linearVariables (Var i) = Just [i]
linearVariables (Lit _) = Just []
linearVariables (Comb _ f es)
 | f==("Prelude","?") && length es == 2  -- treat "?" as Or:
  = linearVariables (Or (head es) (head (tail es)))
 | otherwise
  = mapMMaybe linearVariables es >>- \esvars ->
    let vars = concat esvars
     in if nub vars == vars
        then Just vars
        else Nothing
linearVariables (Free vs e) =
  linearVariables e >>- \evars -> Just (evars \\ vs)
linearVariables (Let bs e) =
  mapMMaybe linearVariables (map snd bs) >>- \bsvars ->
  linearVariables e >>- \evars ->
  let vars = concat (evars : bsvars)
   in if nub vars == vars
      then Just (vars \\ (map fst bs))
      else Nothing
linearVariables (Or e1 e2) =
  linearVariables e1 >>- \e1vars ->
  linearVariables e2 >>- \e2vars ->
  Just (union e1vars e2vars)
linearVariables (Case _ e bs) =
  linearVariables e >>- \evars ->
  mapMMaybe linearVariables (map (\ (Branch _ be) -> be) bs) >>- \bsvars ->
  let vars = foldr union [] (map (\ (branch,bsv) -> bsv \\ patternVars branch)
                                 (zip bs bsvars)) ++ evars
   in if nub vars == vars
      then Just vars
      else Nothing
 where
  patternVars (Branch (Pattern _ vs) _) = vs
  patternVars (Branch (LPattern _)   _) = []
