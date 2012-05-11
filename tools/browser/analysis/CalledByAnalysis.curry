-----------------------------------------------------------------------------
-- Compute for each function the functions which call it.
--
-- Michael Hanus, April 2005
-----------------------------------------------------------------------------

module CalledByAnalysis(calledBy) where

import FlatCurry
import List

-- Computes the list of all functions that calls some function.
-- Argument: a list of function declarations
-- Result: a list of pairs of qualified functions names and the corresponding
--         caller functions
calledBy :: [FuncDecl] -> [(QName,[QName])]
calledBy funs = map cfFun funs
 where
   cfFun (Func fname _ _ _ _) = (fname, concatMap (getCalls fname) funs)

getCalls :: QName -> FuncDecl -> [QName]
getCalls _ (Func _ _ _ _ (External _)) = []
getCalls name1 (Func name2 _ _ _ (Rule _ e)) | isCalled name1 e = [name2]
                                             | otherwise        = []

isCalled :: QName -> Expr -> Bool
isCalled _ (Var _) = False
isCalled _ (Lit _) = False
isCalled name (Comb _ c es) | c==name   = True
                            | otherwise = any (isCalled name) es
isCalled name (Free _ e)   = isCalled name e
isCalled name (Let bs e) = any (isCalled name) (e : map snd bs)
isCalled name (Or e1 e2)     = (isCalled name e1) || (isCalled name e2)
isCalled name (Case _ _ ces) = any isCalledCase ces
  where isCalledCase (Branch _ e) = isCalled name e

-- end of CalledByAnalysis

