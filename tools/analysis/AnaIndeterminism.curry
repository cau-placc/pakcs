------------------------------------------------------------------------------
-- Indeterminism analysis:
-- check whether functions are indeterministic, i.e., contain an
-- indirect/implicit call to a committed choice.
--
-- Michael Hanus, September 1999
------------------------------------------------------------------------------

module AnaIndeterminism(analyseIndeterminism,indetFunctions,
                        choiceInExpr)   where

import FlatCurry
import List
import AnaDependency

------------------------------------------------------------------------------
-- The indeterminism analysis must be applied to complete programs,
-- i.e., modules together with all their imported modules.
-- It assigns to a FlatCurry program the list of all qualified function names
-- together with a flag which is True if this function might be defined
-- in a indeterministic manner (i.e., contains an indirect/implicit call
-- to a committed choice).

analyseIndeterminism :: Prog -> [((String,String),Bool)]
analyseIndeterminism (Prog _ _ _ funs _) = map anaFun alldeps
  where
    anaFun (name,depfuns) = (name, any (isIndetDefined funs) (name:depfuns))

    alldeps = indirectlyDependent funs


-- Computes the list of all indeterministic functions:
indetFunctions funs = map fst (filter isIndet alldeps)
  where
    isIndet (name,depfuns) = any (isIndetDefined funs) (name:depfuns)

    alldeps = indirectlyDependent funs


-- (isIndetDefined fundecls f):
-- Is a function f defined by an indeterministic rule?
isIndetDefined :: [FuncDecl] -> (String,String) -> Bool
isIndetDefined [] _ = False -- this case should occur only for constructors
isIndetDefined (Func f _ _ _ def : funs) f1 =
 if f==f1 then isIndetRule def
          else isIndetDefined funs f1


--- is a function directly (i.e., by its rhs) indeterministic?
isIndeterministic (Func _ _ _ _ rule) = isIndetRule rule

isIndetRule (Rule _ e) = choiceInExpr e
isIndetRule (External _) = False

-- check an expression for occurrences of committed choice or send:
choiceInExpr :: Expr -> Bool
choiceInExpr (Var _) = False
choiceInExpr (Lit _) = False
choiceInExpr (Comb _ f es) = f==("Prelude","commit") ||
                             f==("Ports","send") || f==("Ports","doSend") ||
                             any choiceInExpr es
choiceInExpr (Free _ e) = choiceInExpr e
choiceInExpr (Let bs e) = any choiceInExpr (map snd bs) || choiceInExpr e
choiceInExpr (Or e1 e2) = choiceInExpr e1 || choiceInExpr e2
choiceInExpr (Case _  e bs) = choiceInExpr e || any choiceInBranch bs
                where choiceInBranch (Branch _ be) = choiceInExpr be

-- end of AnaIndeterminism
