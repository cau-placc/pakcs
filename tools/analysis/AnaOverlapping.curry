------------------------------------------------------------------------------
-- Overlapping analysis:
-- check whether functions are defined with overlapping left-hand sides
-- (i.e., whether they are defined with OR expressions)
--
-- Michael Hanus, February 2005
------------------------------------------------------------------------------

module AnaOverlapping(analyseOverlappings,orInExpr) where

import FlatCurry

------------------------------------------------------------------------------
-- The overlapping analysis can be applied to individual modules.
-- It assigns to a FlatCurry module the list of all qualified function names
-- together with a flag which is True if this function is defined
-- with overlapping left-hand sides.

analyseOverlappings :: Prog -> [((String,String),Bool)]
analyseOverlappings (Prog _ _ _ funs _) = map overlapFun funs
  where
    overlapFun (Func name _ _ _ (Rule _ e))   = (name, orInExpr e)
    overlapFun (Func name _ _ _ (External _)) = (name, False)

--------------------------------------------------------------------------
-- Check an expression for occurrences of OR:
orInExpr :: Expr -> Bool
orInExpr (Var _) = False
orInExpr (Lit _) = False
orInExpr (Comb _ f es) =
  if f==("Prelude","commit")
  then False -- OR in committed choice have only local effects
  else any orInExpr es
orInExpr (Free _ e) = orInExpr e
orInExpr (Let bs e) = any orInExpr (map snd bs) || orInExpr e
orInExpr (Or _ _) = True
orInExpr (Case _ e bs) = orInExpr e || any orInBranch bs
                   where orInBranch (Branch _ be) = orInExpr be

