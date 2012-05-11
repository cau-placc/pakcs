------------------------------------------------------------------------------
-- Generate an interface description or a human-readable
-- presentation of a Curry module.
--
-- The interface description contains the type declarations
-- for all entities defined and exported by this module.
--
-- The human-readable presentation is (almost) Curry source code
-- generated from a FlatCurry program.
--
-- Michael Hanus, September 2003
------------------------------------------------------------------------------

module ShowFlatCurry(showInterface,showCurryMod,showFlatProg,
                     showFuncDeclAsCurry,showFuncDeclAsFlatCurry,leqFunc)
 where

import FlatCurry
import FlatCurryShow
import FlatCurryGoodies
import FlexRigid
import List
import Char(isAlpha)
import Sort(mergeSort,leqString)


-----------------------------------------------------------------------
-- Generate interface description for a program:
-- If first argument is True, generate stubs (...external) for
-- all functions so that the resulting interface is a valid Curry program.
showInterface :: Bool -> Prog -> String
showInterface genstub (Prog mod imports types funcs ops) =
  concatMap showInterfaceImport imports ++ "\n" ++
  concatMap showInterfaceOpDecl (mergeSort leqOp ops) ++
  (if null ops then "" else "\n") ++
  concatMap (showInterfaceType (showQNameInModule mod))
            (mergeSort leqType types) ++ "\n" ++
  concatMap (showInterfaceFunc (showQNameInModule mod) genstub)
            (mergeSort leqFunc funcs) ++ "\n"

-- show import declaration
showInterfaceImport impmod = if impmod=="Prelude" then ""
                                           else "import "++impmod++"\n"

-- show operator declaration
showInterfaceOpDecl (Op op InfixOp  prec) = "infix "++show prec++" "++showOp op++"\n"
showInterfaceOpDecl (Op op InfixlOp prec) = "infixl "++show prec++" "++showOp op++"\n"
showInterfaceOpDecl (Op op InfixrOp prec) = "infixr "++show prec++" "++showOp op++"\n"

showOp (_,on) = if isAlpha (head on) then '`':on++"`"
                                     else on

-- show type declaration
showInterfaceType tt (Type (_,tcons) vis tvars constrs) =
  if vis==Public
  then "data " ++ tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
       " = " ++
       concat (intersperse " | "
                 (map (showExportConsDecl tt)
                      (filter (\(Cons _ _ cvis _)->cvis==Public) constrs)))
       ++ "\n"
  else ""
showInterfaceType tt (TypeSyn (_,tcons) vis tvars texp) =
  if vis==Public
  then "type " ++ tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
       " = " ++ showCurryType tt True texp ++ "\n"
  else ""

showExportConsDecl tt (Cons (_,cname) _ _ argtypes) =
  cname ++ concatMap (\t->" "++showCurryType tt True t) argtypes

-- show function type declaration
showInterfaceFunc ttrans genstub (Func (_,fname) _ vis ftype _) =
  if vis==Public
  then showCurryId fname ++ " :: " ++
       showCurryType ttrans False ftype ++ "\n" ++
       (if genstub then showCurryId fname ++ " external\n\n" else "")
  else ""


---------------------------------------------------------------------------
-- generate a human-readable representation of a Curry module:

showCurryMod :: Bool -> Prog -> String
showCurryMod ascase (Prog mod imports types funcs ops) =
  "module "++mod++"("++showTypeExports types++
  showFuncExports funcs++") where\n\n"++
  concatMap showInterfaceImport imports ++ "\n" ++
  concatMap showInterfaceOpDecl ops ++
  (if null ops then "" else "\n") ++
  concatMap (showCurryDataDecl (showQNameInModule mod)) types
  ++ "\n" ++
  concatMap (showCurryFuncDecl (showQNameInModule mod)
                               (showQNameInModule mod) ascase) funcs
  ++ "\n-- end of module " ++ mod ++ "\n"

showTypeExports types = concatMap (++",") (concatMap exptype types)
 where
   exptype (Type tcons vis _ cdecls) =
     if vis==Public
     then [snd tcons++let cs = expcons cdecls in (if cs=="()" then "" else cs)]
     else []
   exptype (TypeSyn tcons vis _ _) = if vis==Public then [snd tcons] else []

   expcons cds = "(" ++ concat (intersperse "," (concatMap expc cds)) ++ ")"
   expc (Cons cname _ vis _) = if vis==Public then [snd cname] else []

showFuncExports funcs = concat (intersperse "," (concatMap expfun funcs))
 where
   expfun (Func fname _ vis _ _) = if vis==Public then [snd fname] else []

showCurryDataDecl tt (Type tcons _ tvars constrs) =
  "data " ++ snd tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
  " = " ++
  concat (intersperse " | " (map (showCurryConsDecl tt) constrs))
  ++ "\n"
showCurryDataDecl tt (TypeSyn tcons _ tvars texp) =
  "type " ++ snd tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++
  " = " ++ showCurryType tt True texp ++ "\n"

showCurryConsDecl tt (Cons cname _ _ argtypes) =
  snd cname ++ concatMap (\t->" "++showCurryType tt True t) argtypes


-- generate function definitions:
showCurryFuncDecl tt tf ascase (Func fname _ _ ftype frule) =
  showCurryId (snd fname) ++" :: "++ showCurryType tt False ftype ++ "\n" ++
  showCurryRule tf ascase fname frule

showCurryRule _ _ fname (External _) = showCurryId (snd fname) ++ " external\n\n"
showCurryRule tf ascase fname (Rule lhs rhs) =
  if ascase then showCurryRuleAsCase tf fname (Rule lhs rhs)
            else showCurryRuleAsPatterns tf fname (Rule lhs rhs)

-- format rule as case expression:
showCurryRuleAsCase tf fname (Rule lhs rhs)
 | length lhs == 2 && not (isAlpha (head (snd fname))) -- infix op
  = showCurryVar (head lhs) ++ " " ++ tf fname ++ " " ++ showCurryVar (lhs!!1) ++
    " = " ++ showCurryExpr tf False 0 rhs ++ "\n\n"
 | otherwise
  = tf fname ++ " " ++ concat (intersperse " " (map showCurryVar lhs)) ++
    " = " ++ showCurryExpr tf False 0 rhs ++ "\n\n"
showCurryRuleAsCase _ fname (External _) = showCurryId (snd fname) ++ " external\n"

-- format rule as set of pattern matching rules:
showCurryRuleAsPatterns tf fname (Rule lhs rhs) =
   showEvalAnnot (getFlexRigid rhs) ++
   concatMap (\(l,r)->showCurryPatternRule tf l r) patternrules
   ++ "\n"
 where
   showEvalAnnot ConflictFR = tf fname ++ " eval rigid&flex -- CONFLICTING!!\n"
   showEvalAnnot UnknownFR = ""
   showEvalAnnot KnownRigid = tf fname ++ " eval rigid\n"
   showEvalAnnot KnownFlex  = "" --tf fname ++ " eval flex\n"

   patternrules = rule2equations (shallowPattern2Expr fname lhs) rhs

splitFreeVars exp = case exp of
  Free vars e -> (vars,e)
  _ -> ([],exp)

showCurryPatternRule tf l r = let (vars,e) = splitFreeVars r in
   showCurryExpr tf False 0 l ++
   showCurryCRHS tf e ++
   (if vars==[] then "" else
    " where " ++ concat (intersperse "," (map showCurryVar vars)) ++ " free")
   ++ "\n"

showCurryCRHS tf r =
   if isGuardedExpr r
   then " | " ++ showCurryCondRule r
   else " = " ++ showCurryExpr tf False 2 r
 where
   showCurryCondRule (Comb _ _ [e1,e2]) =
     showCurryExpr tf False 2 e1 ++ " = " ++ showCurryExpr tf False 4 e2


-- transform a rule consisting of a left- and a right-hand side
-- (represented as expressions) into a set of pattern matching rules:
rule2equations :: Expr -> Expr -> [(Expr,Expr)]
rule2equations lhs (Or e1 e2) =
   rule2equations lhs e1 ++ rule2equations lhs e2
rule2equations lhs (Case ctype e bs) =
   if isVarExpr e then let Var i = e  in  caseIntoLhs lhs i bs
                  else [(lhs,Case ctype e bs)]
rule2equations lhs (Var i) = [(lhs,Var i)]
rule2equations lhs (Lit l) = [(lhs,Lit l)]
rule2equations lhs (Comb ct name args) = [(lhs,Comb ct name args)]
rule2equations lhs (Free vs e) = [(lhs,Free vs e)]
rule2equations lhs (Let bs e) = [(lhs,Let bs e)]

caseIntoLhs _ _ [] = []
caseIntoLhs lhs vi (Branch (Pattern c vs) e : bs) =
  rule2equations (substitute [vi] [shallowPattern2Expr c vs] lhs) e
  ++ caseIntoLhs lhs vi bs
caseIntoLhs lhs vi (Branch (LPattern lit) e : bs) =
  rule2equations (substitute [vi] [Lit lit] lhs) e
  ++ caseIntoLhs lhs vi bs

shallowPattern2Expr name vars =
               Comb ConsCall name (map (\i->Var i) vars)


-- (substitute vars exps expr) = expr[vars/exps]
-- i.e., replace all occurrences of vars by corresponding exps in the
-- expression expr
substitute vars exps expr = substituteAll vars exps 0 expr

-- (substituteAll vars exps base expr):
-- substitute all occurrences of variables by corresonding expressions:
-- * substitute all occurrences of var_i by exp_i in expr
--   (if vars=[var_1,...,var_n] and exps=[exp_1,...,exp_n])
-- * substitute all other variables (Var j) by (Var (base+j))
--
-- here we assume that the new variables in guards and case patterns
-- do not occur in the list "vars" of replaced variables!

substituteAll :: [Int] -> [Expr] -> Int -> Expr -> Expr
substituteAll vars exps b (Var i) = replaceVar vars exps i
  where replaceVar [] [] var = Var (b+var)
        replaceVar (v:vs) (e:es) var = if v==var then e
                                                 else replaceVar vs es var
substituteAll _  _  _ (Lit l) = Lit l
substituteAll vs es b (Comb combtype c exps) =
                 Comb combtype c (map (substituteAll vs es b) exps)
substituteAll vs es b (Let bindings exp) =
                 Let (map (\(x,e)->(x+b,substituteAll vs es b e)) bindings)
                     (substituteAll vs es b exp)
substituteAll vs es b (Free vars e) =
                 Free (map (+b) vars) (substituteAll vs es b e)
substituteAll vs es b (Or e1 e2) =
                 Or (substituteAll vs es b e1) (substituteAll vs es b e2)
substituteAll vs es b (Case ctype e cases) =
   Case ctype (substituteAll vs es b e) (map (substituteAllCase vs es b) cases)

substituteAllCase vs es b (Branch (Pattern l pvs) e) =
                 Branch (Pattern l (map (+b) pvs)) (substituteAll vs es b e)
substituteAllCase vs es b (Branch (LPattern l) e) =
                 Branch (LPattern l) (substituteAll vs es b e)


-- Is the expression a guarded expressions?
isGuardedExpr :: Expr -> Bool
isGuardedExpr e = case e of
  Comb _ f _ -> f == ("Prelude","cond")
  _ -> False

-- Is the expression a variable?
isVarExpr :: Expr -> Bool
isVarExpr e = case e of
  Var _ -> True
  _ -> False


-------- Definition of some orderings:
leqOp (Op (_,op1) _ p1) (Op (_,op2) _ p2) =
                         p1>p2 || p1==p2 && leqString op1 op2

leqType t1 t2 = leqString (tname t1) (tname t2)
 where tname (Type    (_,tn) _ _ _) = tn
       tname (TypeSyn (_,tn) _ _ _) = tn

leqFunc (Func (_,f1) _ _ _ _) (Func (_,f2) _ _ _ _) = leqString f1 f2

---------------------------------------------------------------------------
-- Show individual functions:

showFuncDeclAsCurry fd =
  showCurryFuncDecl (showQNameInModule (funcModule fd))
                    (showQNameInModule (funcModule fd)) False fd

showFuncDeclAsFlatCurry fd =
  showCurryFuncDecl (showQNameInModule (funcModule fd))
                    (showQNameInModule (funcModule fd)) True fd

