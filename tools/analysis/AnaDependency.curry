-----------------------------------------------------------------------------
-- A few base functions for analysing dependencies in FlatCurry programs:
--
-- Michael Hanus, February 2004
-----------------------------------------------------------------------------

module AnaDependency(funsInExpr, indirectlyDependent) where

import FlatCurry
import List
import SetRBT
import Sort(leqString)

-- Computes the list of indirect dependencies for all functions.
-- Argument: a list of function declarations
-- Result: a list of pairs of qualified functions names and the corresponding
--         called functions
indirectlyDependent :: [FuncDecl] -> [(QName,[QName])]
indirectlyDependent funs = map (\ (f,ds) -> (f,setRBT2list ds))
                               (depsClosure (map directlyDependent funs))

-- list of direct dependencies for all functions
directlyDependent :: FuncDecl -> (QName,SetRBT QName)
directlyDependent (Func f _ _ _ (Rule _ e))   = (f,funsInExpr e)
directlyDependent (Func f _ _ _ (External _)) = (f,emptySet)

-- compute all transitive dependencies between functions:
depsClosure :: [(QName,SetRBT QName)] -> [(QName,SetRBT QName)]
depsClosure directdeps = map (\ (f,ds)->(f,closure ds (setRBT2list ds)))
                             directdeps
 where
  closure olddeps [] = olddeps
  closure olddeps (f:fs) =
     let newdeps = filter (\e->not (elemRBT e olddeps))
                          (setRBT2list (getDeps directdeps f))
      in closure (foldr insertRBT olddeps newdeps) (newdeps++fs)

  getDeps [] _ = emptySet
  getDeps ((f,ds):fdeps) f1 = if f==f1 then ds
                                       else getDeps fdeps f1


-- Gets the set of all functions (including partially applied functions)
-- called in an expression:
funsInExpr :: Expr -> SetRBT QName
funsInExpr (Var _) = emptySet
funsInExpr (Lit _) = emptySet
funsInExpr (Comb ct f es) =
  if isConstructorComb ct then unionMap funsInExpr es
                          else insertRBT f (unionMap funsInExpr es)
funsInExpr (Free _ e) = funsInExpr e
funsInExpr (Let bs e) = unionRBT (unionMap (funsInExpr . snd) bs) (funsInExpr e)
funsInExpr (Or e1 e2) = unionRBT (funsInExpr e1) (funsInExpr e2)
funsInExpr (Case _ e bs) = unionRBT (funsInExpr e) (unionMap funsInBranch bs)
                     where funsInBranch (Branch _ be) = funsInExpr be

isConstructorComb ct = case ct of
  ConsCall       -> True
  ConsPartCall _ -> True
  _              -> False

unionMap f = foldr unionRBT emptySet . map f

emptySet = emptySetRBT leqQName

leqQName (m1,n1) (m2,n2) = leqString (m1++('.':n1)) (m2++('.':n2))

-- end of AnaDependency

