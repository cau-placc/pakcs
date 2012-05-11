-----------------------------------------------------------------------------
-- Completeness analysis for Curry programs
--
-- This analysis checks for each function in a Curry program  whether
-- this function is completely defined, i.e., reducible on all ground
-- constructor terms
--
-- Johannes Koj, October 2000
-- Michael Hanus, January 2002
-----------------------------------------------------------------------------

module AnaCompleteness(CompletenessType(..),analyseCompleteness) where

import FlatCurry


------------------------------------------------------------------------------
-- The completeness analysis must be applied to complete programs,
-- i.e., modules together with all their imported modules (although
-- functions are locally checked, the definition of all data types
-- used in the patterns are needed).
-- It assigns to a FlatCurry program the list of all qualified function names
-- together with a flag which is True if this function is completely
-- defined on its input types (i.e., reducible for all ground data terms).

-- The possible outcomes of the completeness analysis:
data CompletenessType =
     Complete       -- completely defined
   | InComplete     -- incompletely defined
   | InCompleteOr   -- incompletely defined in each branch of an "Or"

analyseCompleteness :: Prog -> [((String,String),CompletenessType)]
analyseCompleteness (Prog _ _ types funs _) = map anaFun funs
 where
  anaFun (Func name _ _ ftype (Rule vs e)) =
                        (name, isComplete types (initialTypeEnv vs ftype) e)
  anaFun (Func name _ _ _ (External _)) = (name, Complete)

isComplete :: [TypeDecl] -> [(Expr,TypeExpr)] -> Expr -> CompletenessType
isComplete _ _ (Var _)      = Complete
isComplete _ _ (Lit _)      = Complete
isComplete types typeEnv (Comb _ f es) =
  if f==("Prelude","commit") && length es == 1
  then isComplete types typeEnv (head es)
  else Complete
isComplete _ _ (Free _ _) = Complete
isComplete _ _ (Let _ _) = Complete
isComplete types typeEnv (Or e1 e2) =
    combineOrResults (isComplete types typeEnv e1)
                     (isComplete types typeEnv e2)
isComplete types typeEnv (Case _ exp ces) =
  case exp of
    Var _ -> aux (getConstructors (lookupType exp typeEnv) types) ces
    _ -> InComplete -- if the case argument is not a variable, we give up
                    -- since we do not want to infer the expression type here
  where
    -- check for occurrences of all constructors in each case branch:
    aux []    _  = Complete
    aux (_:_) [] = InComplete
    aux (_:_) (Branch (LPattern _)   _ : _) = InComplete
    aux (c:cs) (Branch (Pattern i vs) e : ps) =
        combineAndResults (aux (removeConstructor i (c:cs)) ps)
              (isComplete types
                          (makeTypeEnv vs (getConsArgTypes i (c:cs)) ++typeEnv)
                          e)

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


removeConstructor c1 [] = error ("Constructor "++show c1++" not found!!")
removeConstructor c1 ((Cons c2 a vis ts):cs)
        | c1==c2    = cs
        | otherwise = (Cons c2 a vis ts):(removeConstructor c1 cs)

getConsArgTypes c1 [] = error ("Constructor "++show c1++" not found!!")
getConsArgTypes c1 (Cons c2 _ _ ts : cs)
        | c1==c2    = ts
        | otherwise = getConsArgTypes c1 cs

-- Gets the list of all constructors for a given type.
getConstructors :: TypeExpr -> [TypeDecl] -> [ConsDecl]
getConstructors t (TypeSyn _ _ _ _ : types) = getConstructors t types
getConstructors (TCons c1 as1) (Type c2 _ as2 cs : types)
    | c1==("Prelude","Int")  || c1==("Prelude","Float")  || 
      c1==("Prelude","Char") || c1==("Prelude","String")
       = [Cons ("","") 0 Private []]
    | c1==c2 = replace as2 as1 cs
    | otherwise = getConstructors (TCons c1 as1) types
 where
   replace _ _ [] = []
   replace ts1 ts2 ((Cons c ar vis ts):cns) =
                    (Cons c ar vis (repTypes ts1 ts2 ts)):(replace ts1 ts2 cns)

   repTypes ts1 ts2 ts = map (repType ts1 ts2) ts
   repType ts1 ts2 (TVar v) = lookup ts1 ts2 v
   repType ts1 ts2 (FuncType t1 t2) =
                    FuncType (repType ts1 ts2 t1) (repType ts1 ts2 t2)
   repType ts1 ts2 (TCons c ts) = TCons c (repTypes ts1 ts2 ts)

   lookup [] _ v = TVar v
   lookup (t1:ts1) (t2:ts2) v | v==t1     = t2
                              | otherwise = lookup ts1 ts2 v

-- This function returns the type of a case argument. Here we
-- assume that a case argument is always a variable.
lookupType e1 ((e2,t):ass) | e1==e2    = t
                           | otherwise = lookupType e1 ass

-- Computes an initial type environment for the parameters variables
-- (represented by their indices) w.r.t. a given function type.
initialTypeEnv :: [Int] -> TypeExpr -> [(Expr,TypeExpr)]
initialTypeEnv [] _ = []
initialTypeEnv (v:vs) (FuncType t1 t2) = (Var v,t1):(initialTypeEnv vs t2)

-- Computes a type environment for the parameters of a case branch.
makeTypeEnv [] _ = []
makeTypeEnv (v:vs) (t:ts) = (Var v,t):(makeTypeEnv vs ts)

