------------------------------------------------------------------------------
-- Nondeterminism analysis:
-- check whether functions are set-valued/nondeterministic, i.e., might
-- have several results even for ground argument terms.
--
-- Michael Hanus, March 2012
------------------------------------------------------------------------------

module Nondeterminism(analyseNondeterminism,analyseSetValued) where

import FlatCurry
import Overlapping(orInExpr)
import Dependency(analyseWithDependencies)

------------------------------------------------------------------------------
-- The non-determinism analysis is a global analysis that must be applied to
-- the complete list of defined functions.
-- It assigns to a list of functions the list of all function names
-- together with a flag which is True if this function might be non-deterministic,
-- might have two different evaluations for given ground arguments.

analyseNondeterminism :: [FuncDecl] -> [(QName,Bool)]
analyseNondeterminism = analyseWithDependencies isNondeterministicDefined or

-- Is a function nondeterministically defined, i.e., does the rule contain or's?
isNondeterministicDefined :: FuncDecl -> Bool
isNondeterministicDefined (Func _ _ _ _ rule) = isNondeterministicRule rule

isNondeterministicRule (Rule _ e) = orInExpr e
isNondeterministicRule (External _) = False


------------------------------------------------------------------------------
-- The set-valued analysis is a global analysis that must be applied to
-- the complete list of defined functions.
-- It assigns to a list of functions the list of all function names
-- together with a flag which is True if this function might be set-valued,
-- might reduce to different values for given ground arguments.

analyseSetValued :: [FuncDecl] -> [(QName,Bool)]
analyseSetValued = analyseWithDependencies isSetValuedDefined or

-- Is a function f defined to be potentially set-valued, i.e., is the rule
-- nondeterministic or does it contain extra variables?
isSetValuedDefined :: FuncDecl -> Bool
isSetValuedDefined (Func f _ _ _ rule) =
  f `notElem` [pre "failed",pre "$!!",pre "$##"] && isSetValuedRule rule

isSetValuedRule (Rule _ e) = orInExpr e || extraVarInExpr e
isSetValuedRule (External _) = False


-- check an expression for occurrences of extra variables:
extraVarInExpr :: Expr -> Bool
extraVarInExpr (Var _) = False
extraVarInExpr (Lit _) = False
extraVarInExpr (Comb _ _ es) = or (map extraVarInExpr es)
extraVarInExpr (Free vars e) = (not (null vars)) || extraVarInExpr e
extraVarInExpr (Let bs e) = any extraVarInExpr (map snd bs) || extraVarInExpr e
extraVarInExpr (Or e1 e2) = extraVarInExpr e1 || extraVarInExpr e2
extraVarInExpr (Case _  e bs) = extraVarInExpr e || any extraVarInBranch bs
                where extraVarInBranch (Branch _ be) = extraVarInExpr be


pre n = ("Prelude",n)
