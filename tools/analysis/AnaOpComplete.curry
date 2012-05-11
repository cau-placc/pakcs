------------------------------------------------------------------------------
-- Analysis for operational completeness:
-- check whether functions are operational complete, i.e., calls only
-- non-rigid functions
--
-- Michael Hanus, January 2004
------------------------------------------------------------------------------

module AnaOpComplete(analyseOpCompleteness)   where

import FlatCurry
import List
import AnaDependency

------------------------------------------------------------------------------
-- The operational completeness analysis must be applied to complete programs,
-- i.e., modules together with all their imported modules.
-- It assigns to a FlatCurry program the list of all qualified function names
-- together with a flag which is True if this function is operationally
-- complete, i.e., does not call a rigid function.

analyseOpCompleteness :: Prog -> [((String,String),Bool)]
analyseOpCompleteness (Prog _ _ _ funs _) = map anaFun alldeps
  where
    anaFun (name,depfuns) = (name, all (isFlexDefined funs) (name:depfuns))

    alldeps = indirectlyDependent funs


-- (isFlexDefined fundecls f):
-- Is a function f defined by a flexible rule?
isFlexDefined :: [FuncDecl] -> (String,String) -> Bool
isFlexDefined [] f =  -- this case should occur only for constructors
                      -- and some special predefined functions
   f `notElem` [("Prelude","findall"),("Prelude","findfirst")]
isFlexDefined (Func f _ _ _ def : funs) f1 =
 if f==f1 then isFlexRule def
          else isFlexDefined funs f1

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


-- end of AnaOpComplete
