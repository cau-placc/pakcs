------------------------------------------------------------------------------
--- A compiler for Curry into JavaScript programs for WUIs.
---
--- @author Michael Hanus
--- @version January 16, 2007
------------------------------------------------------------------------------

import JavaScript
import List
import FlatCurry
import Integer
import System(system,getArgs)
import Directory
import CompactFlatCurry
import Char(isAlphaNum)
import Unsafe
import Distribution(installDir,findFileInLoadPath)
import FlatCurryShow
import Maybe
import ReadNumeric(readNat)

------------------------------------------------------------------------------
-- General definitions:

-- Should higher-order calls (i.e., "apply") be implemented as an explicit
-- apply function? Otherwise, they are implemented as anonymous functions.
explicitApply = False


-- Optimization options:

-- Optimize single occurrences of variables in JavaScript code?
optimizeSingleVars = True
-- Optimize cases on data types containing only a single constructor?
optimizeUniqueCase = True
-- Should strings be lazily converted into character lists?
lazyStringConversion = True

-- The name of the prelude module:
prelude = "Prelude"
-- The name of the WUI module:
wuiModName = "WUIjs"

------------------------------------------------------------------------------
-- Translator:

-- Translate a Curry program into a JavaScript program
flatprog2JS :: Prog -> [JSFDecl]
flatprog2JS (Prog _ _ tdecls allfdecls _) =
  map (flat2JS tdecls) fdecls ++
  (if explicitApply then [genApply (pafsOfFuncs fdecls)] else [])
 where
   fdecls = filter isRelevantFunction allfdecls

isRelevantFunction (Func fname _ _ _ (External _))
 | fname `elem` ignoredFunctions = False
 | otherwise = error $ "External function "++show fname++" not handled!"
isRelevantFunction (Func fname _ _ _ (Rule _ _)) =
  fname `notElem` ignoredFunctions


genApply fs =
  JSFDecl "fullapply" [1,2]
    [JSSwitch (JSIArrayIdx 1 0)
              (concatMap branch4fun fs ++
               [JSDefault [JSReturn (JSFCall "fullconsapply"
                                             [JSIVar 1, JSIVar 2])]])]
 where
  branch4fun (fname,ar) = if ar==0 then [] else
    [JSCase (qname2JS fname)
      [JSReturn (curryFunc2JSFunc fname
                         (map (\i->JSIArrayIdx 1 i) [2..ar] ++ [JSIVar 2]))]]


flat2JS :: [TypeDecl] -> FuncDecl -> JSFDecl
flat2JS tdecls (Func fname _ _ _ (Rule args rhs)) =
  JSFDecl (qname2JS fname) args
          (removeSingleVarsJSStatements
                   ([JSVarDecl retvar] ++
                     flatExp2JS tdecls retvar unknown [] retvar rhs ++
                     [JSReturn (JSIVar retvar)]))
 where
  retvar = maxlist (maxVarIndexInExp rhs : args) + 1


-- Translate a FlatCurry expression into JavaScript:
flatExp2JS _ maxo maxn patvars retvar (Var i) | maxn=:=maxo =
  maybe [JSAssign (JSIVar retvar) (JSIVar i)]
        (\ (cvar,pos) -> [JSAssign (JSIVar retvar) (JSIArrayIdx cvar pos)])
        (lookup i patvars)

flatExp2JS _ maxo maxn _ retvar (Lit (Intc i)) | maxn=:=maxo =
  [JSAssign (JSIVar retvar) (JSInt i)]

flatExp2JS _ _ _ _ _ (Lit (Floatc _)) =
  error "Float constants not yet supported!"

flatExp2JS _ maxo maxn _ retvar (Lit (Charc c)) | maxn=:=maxo =
  [JSAssign (JSIVar retvar) (JSString [c])]

flatExp2JS decls maxo maxn patvars retvar (Comb FuncCall fname args) =
 let maxa = maxo + length args
     argvars = [(maxo+1)..maxa]
  in flatExps2JS decls maxa maxn patvars (zip argvars args) ++
     [JSAssign (JSIVar retvar)
               (curryFunc2JSFunc fname (map JSIVar argvars))]

flatExp2JS decls maxo maxn patvars retvar (Comb ConsCall cname args)
 | fst cname == prelude && snd cname `elem` ["True","False"]
   = maxn=:=maxo &> [JSAssign (JSIVar retvar) (JSBool (snd cname == "True"))]
 | otherwise =
 let maxa = maxo + length args
     argvars = [(maxo+1)..maxa]
  in flatExps2JS decls maxa maxn patvars (zip argvars args) ++
     [JSAssign (JSIVar retvar)
               (jsConsTerm (consQName2JS cname) (map JSIVar argvars))]

flatExp2JS decls maxo maxn patvars retvar (Comb (FuncPartCall m) fname args) =
 if explicitApply
 then
  let maxa = maxo + length args
      argvars = [(maxo+1)..maxa]
   in flatExps2JS decls maxa maxn patvars (zip argvars args) ++
      [JSAssign (JSIVar retvar)
                (JSFCall "new Array"
                         (JSString (qname2JS fname) : JSInt m :
                          map JSIVar argvars))]
 else
  let maxa = maxo + length args + m
      argvars = [(maxo+1)..(maxo+length args)]
      missvars = [(maxo+length args+1)..maxa]
      genLambda ai =
        if ai==maxa
        then JSLambda [ai] [JSReturn (curryFunc2JSFunc fname
                                            (map JSIVar (argvars++missvars)))]
        else JSLambda [ai] [JSReturn (genLambda (ai+1))]
   in flatExps2JS decls maxa maxn patvars (zip argvars args) ++
     [JSAssign (JSIVar retvar) (genLambda (head missvars))]


flatExp2JS decls maxo maxn patvars retvar (Comb (ConsPartCall m) cname args) =
 if explicitApply
 then
  let maxa = maxo + length args
      argvars = [(maxo+1)..maxa]
   in flatExps2JS decls maxa maxn patvars (zip argvars args) ++
      [JSAssign (JSIVar retvar)
                (JSFCall "new Array"
                         (JSString (consQName2JS cname) : JSInt m :
                          map JSIVar argvars))]
 else
  let maxa = maxo + length args + m
      argvars = [(maxo+1)..(maxo+length args)]
      missvars = [(maxo+length args+1)..maxa]
      genLambda ai =
        if ai==maxa
        then JSLambda [ai] [JSReturn (jsConsTerm (consQName2JS cname)
                                            (map JSIVar (argvars++missvars)))]
        else JSLambda [ai] [JSReturn (genLambda (ai+1))]
   in flatExps2JS decls maxa maxn patvars (zip argvars args) ++
     [JSAssign (JSIVar retvar) (genLambda (head missvars))]

flatExp2JS decls maxo maxn patvars retvar (Let bindings exp) =
  maybe (error "Translation of recursive Let bindings not supported!")
        (\seqbs -> let maxi free in
                   flatExps2JS decls maxo maxi patvars seqbs ++
                   flatExp2JS decls maxi maxn patvars retvar exp)
        (trySequentializeLetBindings bindings)

flatExp2JS decls maxo maxn patvars retvar (Case _ cexp branches) =
  if length branches == 2 && hasConstPattern "True"  (branches!!0)
                          && hasConstPattern "False" (branches!!1)
  then ite2JS decls maxo maxn patvars retvar cexp
              (expOfBranch (branches!!0)) (expOfBranch (branches!!1))
  else case2JS decls maxo maxn patvars retvar cexp branches
 where
  hasConstPattern c b = case b of
   Branch (Pattern ("Prelude",pc) []) _ -> pc == c
   _ -> False

  expOfBranch (Branch _ exp) = exp

flatExp2JS _ _ _ _ _ (Free _ _) =
  error "Translation of Free not yet supported!"

flatExp2JS _ _ _ _ _ (Or _ _) =
  error "Translation of Or not yet supported!"


-- Translate list of FlatCurry expressions:
flatExps2JS _ maxo maxn _ [] | maxn=:=maxo = []
flatExps2JS decls maxo maxn patvars ((retvar,exp):retvarexps) =
  JSVarDecl retvar : flatExp2JS decls maxo maxe patvars retvar exp ++
  flatExps2JS decls maxe maxn patvars retvarexps
 where
   maxe free


-- Translate case expressions:
case2JS decls maxo maxn patvars retvar cexp branches =
 let casevar = maxo+1
     max1 free
  in [JSVarDecl casevar] ++
     flatExp2JS decls casevar max1 patvars casevar cexp ++
     caseStringProlog casevar ++
     if optimizeUniqueCase && length branches == 1
                           && branchWithUniqueCase (head branches)
     then let [JSCase _ cstats] =

                    branch2JS decls max1 maxn patvars casevar retvar branches
           in cstats
     else [JSSwitch (JSIArrayIdx casevar 0)
                 (branch2JS decls max1 maxn patvars casevar retvar branches)]
 where
  caseStringProlog var =
    if lazyStringConversion && listBranches branches
    then [JSIf (JSOp "==" (JSFCall "typeof" [JSIVar var]) (JSString "string"))
               [JSAssign (JSIVar var) (JSFCall "string2charlist" [JSIVar var])]
               []]
    else []

  listBranches bs = case bs of
    (Branch (Pattern cname _) _ : _) -> snd cname `elem` ["[]",":"]
    _ -> False

  branchWithUniqueCase (Branch (LPattern _) _) = False
  branchWithUniqueCase (Branch (Pattern cname _) _) =
    isUniqueConstructor decls cname

branch2JS _ maxo maxn _ _ _ [] | maxn=:=maxo = []
branch2JS decls maxo maxn patvars casevar retvar
          (Branch (Pattern cname cargs) bexp : branches) =
 let maxa = maxo + length cargs
     maxi free
     newpatvars = map (\(pos,pi) -> (pi,(casevar,pos))) (zip [1..] cargs)
  in JSCase (consQName2JS cname)
            (flatExp2JS decls maxa maxi (newpatvars++patvars) retvar bexp) :
     branch2JS decls maxi maxn patvars casevar retvar branches

-- Check whether some constructor is the only constructor of a type:
isUniqueConstructor :: [TypeDecl] -> QName -> Bool
isUniqueConstructor [] c =
  error ("Internal error in isUniqueConstructor: definition of " ++ show c ++
         " not found!")
isUniqueConstructor (TypeSyn _ _ _ _ : tdecls) c = isUniqueConstructor tdecls c
isUniqueConstructor (Type _ _ _ cdecls : tdecls) c =
  if c `elem` (map (\ (Cons cname _ _ _) -> cname) cdecls)
  then length cdecls == 1
  else isUniqueConstructor tdecls c

-- Translate if-then-else
ite2JS decls maxo maxn patvars retvar bexp exp1 exp2 =
 let ifretvar = maxo+1
     max1,max2 free
  in [JSVarDecl ifretvar] ++
     flatExp2JS decls ifretvar max1 patvars ifretvar bexp ++
     [JSIf (JSIVar ifretvar)
           (flatExp2JS decls max1 max2 patvars retvar exp1)
           (flatExp2JS decls max2 maxn patvars retvar exp2)]

-- translates a function call into the corresponding JavaScript operator
-- or function call:
curryFunc2JSFunc fname args = case args of
  [a1,a2] -> maybe (let jsfname = qname2JS fname in
                    if jsfname/="apply" || explicitApply
                    then JSFCall (qname2JS fname) [a1,a2]
                    else JSApply a1 a2)
                   (\jsop -> JSOp jsop a1 a2)
                   (lookup fname jsOperators)
  _       -> JSFCall (qname2JS fname) args

consQName2JS qname@(md,f)
  | take 2 f == "(," = f
  | otherwise = maybe (md ++ "_" ++ encodeCurryId f)
                      id
                      (lookup qname jsConstructors)


qname2JS qname@(md,f) =
  maybe (md ++ "_" ++ encodeCurryId f)
        id
        (lookup qname (jsFunctions++jsOperators))

-- encode a Curry identifier into a form allowed in JavaScript:
encodeCurryId [] = []
encodeCurryId (c:cs)
  | isAlphaNum c = c : encodeCurryId cs
  | otherwise =  let oc = ord c
    in '_' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : encodeCurryId cs
 where
   int2hex i = if i<10 then chr (ord '0' + i)
                       else chr (ord 'A' + i - 10)

jsFunctions =
  [((prelude,"apply"),"apply"),
   ((prelude,"$#"),"apply"),
   ((prelude,"$##"),"apply"),
   ((prelude,"$!"),"apply"),
   ((prelude,"$!!"),"apply"),
   ((prelude,"div"),"preludeDiv"),
   ((prelude,"ord"),"preludeOrd"),
   ((prelude,"chr"),"preludeChr"),
   ((prelude,"failed"),"alertFailed"),
   ((prelude,"=="),"boolEq")]

jsConstructors =
  [((prelude,":"),":"),
   ((prelude,"[]"),"[]")
  ]

jsOperators =
  [((prelude,"+"),"+"),
   ((prelude,"-"),"-"),
   ((prelude,"*"),"*"),
   ((prelude,"mod"),"%"),
   ((prelude,"&&"),"&&"),
   ((prelude,"||"),"||"),
   ((prelude,"not"),"!"),
   ((prelude,">="),">="),
   ((prelude,"<="),"<="),
   ((prelude,">"),">"),
   ((prelude,"<"),"<")
  ]

ignoredFunctions =
  map fst (jsFunctions ++ jsOperators) ++
  [(prelude,"prim_Int_plus"),(prelude,"prim_Int_minus"),
   (prelude,"prim_Int_times"),(prelude,"prim_Int_div"),
   (prelude,"prim_Int_mod"),
   (prelude,"prim_ord"),(prelude,"prim_chr"),
   (prelude,"compare")]

------------------------------------------------------------------------------
--- Try to sequentialize a list of let bindings.
--- Returns Nothing if this is not possible due to a recursive let.
trySequentializeLetBindings :: [(Int,Expr)] -> Maybe [(Int,Expr)]
trySequentializeLetBindings = trySequentializeLetBinding 0

trySequentializeLetBinding :: Int -> [(Int,Expr)] -> Maybe [(Int,Expr)]
trySequentializeLetBinding try bindings
  | bindings==[] = Just []
  | try >= length bindings = Nothing -- no non-recursive binding found
  | any (`elem` (freeVarsInExp (snd (bindings!!try)))) (map fst bindings)
    = trySequentializeLetBinding (try+1) bindings -- try next binding
  | otherwise
    = maybe Nothing
            (\newbs -> Just ((bindings!!try) : newbs))
            (trySequentializeLetBinding 0
                                 (take try bindings ++ drop (try+1) bindings))

-- get all unbound variables of an expression:
freeVarsInExp :: Expr -> [Int]
freeVarsInExp (Var v) = [v]
freeVarsInExp (Lit _) = []
freeVarsInExp (Comb _ _ exps) = concatMap freeVarsInExp exps
freeVarsInExp (Or e1 e2) = freeVarsInExp e1 ++ freeVarsInExp e2
freeVarsInExp (Let bs e) = let (bvs,bes) = unzip bs in
  filter (`notElem` bvs) (concatMap freeVarsInExp (e : bes))
freeVarsInExp (Free vs exp) = filter (`notElem` vs) (freeVarsInExp exp)
freeVarsInExp (Case _ e bs) = freeVarsInExp e ++ concatMap freeVarsInBranch bs

freeVarsInBranch (Branch (Pattern _ vs) e) =
                                        filter (`notElem` vs) (freeVarsInExp e)
freeVarsInBranch (Branch (LPattern _) e) = freeVarsInExp e


-- get maximum variable index occurring in an expression:
maxVarIndexInExp :: Expr -> Int
maxVarIndexInExp exp = let allvars = allVarsInExp exp in
  if null allvars then 0 else maxlist allvars
 where
  allVarsInExp (Var v) = [v]
  allVarsInExp (Lit _) = []
  allVarsInExp (Comb _ _ exps) = concatMap allVarsInExp exps
  allVarsInExp (Or e1 e2) = allVarsInExp e1 ++ allVarsInExp e2
  allVarsInExp (Let bs e) = let (bvs,bes) = unzip bs
                             in bvs ++ concatMap allVarsInExp (e : bes)
  allVarsInExp (Free vs e) = vs ++ allVarsInExp e
  allVarsInExp (Case _ e bs) = allVarsInExp e ++ concatMap allVarsInBranch bs
  
  allVarsInBranch (Branch (Pattern _ vs) e) = vs ++ allVarsInExp e
  allVarsInBranch (Branch (LPattern _) e) = allVarsInExp e

------------------------------------------------------------------------------
-- compute all partially applied functions in a program:
pafsOfProg (Prog _ _ _ fdecls _) =
  pafsOfFuncs (filter isRelevantFunction fdecls)

pafsOfFuncs fdecls = mapUnion (map pafsOfFunc fdecls)

pafsOfFunc (Func _ _ _ _ (External _)) = []
pafsOfFunc (Func _ _ _ _ (Rule _ exp)) = pafsOfExpr exp

pafsOfExpr (Var _) = []
pafsOfExpr (Lit _) = []
pafsOfExpr (Comb FuncCall _ args) = mapUnion (map pafsOfExpr args)
pafsOfExpr (Comb ConsCall _ args) = mapUnion (map pafsOfExpr args)
pafsOfExpr (Comb (ConsPartCall _) _ args) = mapUnion (map pafsOfExpr args)
pafsOfExpr (Comb (FuncPartCall m) f args) =
  union [(f,m+length args)] (mapUnion (map pafsOfExpr args))
pafsOfExpr (Case _ cexp branches) =
  mapUnion (map pafsOfExpr (cexp : map (\ (Branch _ exp) -> exp) branches))
pafsOfExpr (Let defs exp) =
  mapUnion (map pafsOfExpr (exp : map snd defs))
pafsOfExpr (Free _ exp) = pafsOfExpr exp
pafsOfExpr (Or exp1 exp2) = union (pafsOfExpr exp1) (pafsOfExpr exp2)

mapUnion = foldr union []

------------------------------------------------------------------------------
-- compute the name of all functions in a FlatCurry program
-- occurring in a "withConditionJS" or "withConditionJSName"
-- together with a flag which is true if the
-- occurrence was in a "withConditionJS":
jscOfProg :: Prog -> [(QName,Bool)]
jscOfProg (Prog _ _ _ fdecls _) = jscOfFuncs fdecls

jscOfFuncs fdecls = mapUnion (map jscOfFunc fdecls)

jscOfFunc (Func _ _ _ _ (External _)) = []
jscOfFunc (Func _ _ _ _ (Rule _ exp)) = jscOfExpr exp

jscOfExpr (Var _) = []
jscOfExpr (Lit _) = []
jscOfExpr (Comb ct (m,f) args)
 | ct==FuncCall && m==wuiModName && f=="withConditionJS"
  = union (maybe [] (\i->[(i,True)]) (getCurryFunc (args!!1))) jscOfArgs
 | ct==FuncCall && m==wuiModName && f=="withConditionJSName"
  = union (maybe [] (\i->[(i,False)]) (getCurryFunc (fstFlatCurry (args!!1))))
          jscOfArgs
 | otherwise = jscOfArgs
 where
  getCurryFunc cpred = case cpred of
    Comb (FuncPartCall 1) fname [] -> Just fname
    _ -> Nothing

  fstFlatCurry (Comb _ _ [arg,_]) = arg

  jscOfArgs = mapUnion (map jscOfExpr args)

jscOfExpr (Case _ cexp branches) =
  mapUnion (map jscOfExpr (cexp : map (\ (Branch _ exp) -> exp) branches))
jscOfExpr (Let defs exp) =
  mapUnion (map jscOfExpr (exp : map snd defs))
jscOfExpr (Free _ exp) = jscOfExpr exp
jscOfExpr (Or exp1 exp2) = union (jscOfExpr exp1) (jscOfExpr exp2)


------------------------------------------------------------------------------
-- Transform a WUI application program:
-- * replace all functions occurring in a "withConditionJS" by
--   "withConditionJSName" including the JavaScript function name
-- * replace (wCons<j> cons) by ((wCons<j>JS "cons") cons)
--   if cons is an j-ary data constructor, .e.g., replace
--   Comb FuncCall ("Prelude","apply")
--     [Comb FuncCall ("WUI","wCons3") [],
--      Comb (ConsPartCall 3) ("mod","Date") []]
--   
--   by
--   
--   Comb FuncCall ("Prelude","apply")
--     [Comb (FuncPartCall 4) ("WUI","wCons3JS")
--           [(Comb ConsCall ("Prelude","Just") 
--                  [Comb (FuncPartCall 1) ("JavaScript","jsConsTerm")
--                        [<"mod_Date" FlatCurry string]])],
--      Comb (ConsPartCall 3) ("mod","Date") []]

replaceJscOfProg :: Prog -> Prog
replaceJscOfProg (Prog mname imps ddecls fdecls ops) =
  Prog mname imps ddecls (map replaceJscOfFunc fdecls) ops

replaceJscOfFunc fdecl@(Func _ _ _ _ (External _)) = fdecl
replaceJscOfFunc (Func f ar vis typ (Rule lhs exp)) =
  Func f ar vis typ (Rule lhs (replaceJscOfExpr exp))

replaceJscOfExpr (Var i) = Var i
replaceJscOfExpr (Lit l) = Lit l
replaceJscOfExpr (Comb ct (m,f) args)
  | ct==FuncCall && m==wuiModName && f=="withConditionJS" &&
    isJSTranslatable (args!!1)
   = Comb ct (m,"withConditionJSName")
             [replaceJscOfExpr (head args),
              Comb ConsCall (prelude,"(,)") (replaceCurryFunc (args!!1))]
  | ct==FuncCall && m==prelude && f=="apply" && 
    isWCons (args!!0) && isDataCons (args!!1)
   = let arity = wConsArity (args!!0)
      in Comb ct (m,f)
              [Comb (FuncPartCall (arity+1))
                    (wuiModName,"wCons"++show arity++"JS")
                    [Comb ConsCall (prelude,"Just")
                      [Comb (FuncPartCall 1) ("JavaScript","jsConsTerm")
                        [flatString (consQName2JS (dataConsName (args!!1)))]]],
                     (args!!1)]
  | otherwise
   = Comb ct (m,f) (map replaceJscOfExpr args)
 where
  isWCons exp = case exp of
    Comb FuncCall (mod,fname) [] -> mod==wuiModName && take 5 fname == "wCons"
    _ -> False

  wConsArity (Comb _ (_,fname) []) = fst (fromJust (readNat (drop 5 fname)))

  isDataCons exp = case exp of
    Comb (ConsPartCall 3) _ [] -> True
    _ -> False

  dataConsName (Comb _ cname _) = cname

  isJSTranslatable cpred = case cpred of
    Comb (FuncPartCall 1) _ [] -> True
    _ -> False

  replaceCurryFunc pred@(Comb _ fname _) = [pred, flatString (qname2JS fname)]

replaceJscOfExpr (Case ct cexp branches) =
  Case ct (replaceJscOfExpr cexp)
         (map (\ (Branch cl exp) -> Branch cl (replaceJscOfExpr exp)) branches)
replaceJscOfExpr (Let defs exp) =
  Let (map (\ (i,d) -> (i,replaceJscOfExpr d)) defs) (replaceJscOfExpr exp)
replaceJscOfExpr (Free fvs exp) = Free fvs (replaceJscOfExpr exp)
replaceJscOfExpr (Or exp1 exp2) =
  Or (replaceJscOfExpr exp1) (replaceJscOfExpr exp2)

-- is a FlatCurry expression the representation of an empty list:
flatEmptyList e = case e of
  Comb ConsCall ("Prelude","[]") [] -> True
  _ -> False

-- generate FlatCurry representation of a string:
flatString []     = Comb ConsCall (prelude,"[]") []
flatString (c:cs) = Comb ConsCall (prelude,":") [Lit (Charc c), flatString cs]


------------------------------------------------------------------------------
-- JavaScript code optimization:

-- Type of variable definitions used in code optimization.
-- (SimpleVar i) represents definition of ... = xi
-- (ComplexDef exp n) represents definition of ... = exp where n contains
-- the number of occurrences of the defined variable in the subsequent code
data VarDef = SimpleVar Int
            | ComplexDef JSExp Int -- value / number of applications

-- compute the number of uses of variables with a unique assignment
-- in a sequence of statements:
uniqueDefsOfStats :: [(Int,Maybe VarDef)] -> [JSStat] -> [(Int,Maybe VarDef)]
uniqueDefsOfStats defs [] = defs
uniqueDefsOfStats defs (stat:stats) =
  uniqueDefsOfStats (uniqueDefsOfStat defs stat) stats


uniqueDefsOfStat defs (JSAssign lhs rhs) =
  case lhs of
    JSIVar i -> maybe ((i,Just (rhs2vardef rhs)):rdefs)
                      (\_ -> updateAssoc i Nothing rdefs)
                      (lookup i rdefs)
    _ -> uniqueDefsOfExp rdefs lhs
 where
  rdefs = uniqueDefsOfExp defs rhs

  rhs2vardef exp = case exp of
    JSIVar i -> SimpleVar i
    _ -> ComplexDef exp 0

uniqueDefsOfStat defs (JSIf bexp s1 s2) =
  uniqueDefsOfStats (uniqueDefsOfStats (uniqueDefsOfExp defs bexp) s1) s2

uniqueDefsOfStat defs (JSSwitch exp branches) =
  uniqueDefsOfStats (uniqueDefsOfExp defs exp) (concatMap statsOf branches)
 where
  statsOf (JSCase _ stats) = stats
  statsOf (JSDefault stats) = stats

uniqueDefsOfStat defs (JSPCall _ exps) = uniqueDefsOfExps defs exps

uniqueDefsOfStat defs (JSReturn exp) = uniqueDefsOfExp defs exp

uniqueDefsOfStat defs (JSVarDecl _) = defs


uniqueDefsOfExp defs (JSString _) = defs
uniqueDefsOfExp defs (JSInt _) = defs
uniqueDefsOfExp defs (JSBool _) = defs
uniqueDefsOfExp defs (JSIVar i) =
  maybe defs
        (maybe defs
               (\vd -> updateAssoc i (Just (incVarDef vd)) defs))
        (lookup i defs)
 where
  incVarDef (ComplexDef val occ) = ComplexDef val (occ+1)
  incVarDef (SimpleVar vi) = SimpleVar vi

uniqueDefsOfExp defs (JSIArrayIdx i _) =
  maybe defs
        (maybe defs
               (\vd -> updateAssoc i (newVarDef vd) defs))
        (lookup i defs)
 where
  newVarDef (SimpleVar vi) = Just (SimpleVar vi)
  newVarDef (ComplexDef _ _) = Nothing -- don't optimize complex array vars

uniqueDefsOfExp defs (JSOp _ exp1 exp2) =
  uniqueDefsOfExp (uniqueDefsOfExp defs exp1) exp2
uniqueDefsOfExp defs (JSFCall _ exps) = uniqueDefsOfExps defs exps
uniqueDefsOfExp defs (JSApply exp1 exp2) =
  uniqueDefsOfExp (uniqueDefsOfExp defs exp1) exp2
uniqueDefsOfExp defs (JSLambda _ body) = uniqueDefsOfStats defs body

uniqueDefsOfExps defs [] = defs
uniqueDefsOfExps defs (exp:exps) =
  uniqueDefsOfExps (uniqueDefsOfExp defs exp) exps

updateAssoc r newval ((i,val):assocs) =
  if r==i then (i,newval) : assocs
          else (i,val) : updateAssoc r newval assocs


-- Optimize a sequence of statements by removing single-assigned variables:
removeSingleVarsJSStatements :: [JSStat] -> [JSStat]
removeSingleVarsJSStatements stats =
  if optimizeSingleVars
  then removeSingleVarsInStats (uniqueDefsOfStats [] stats) $## stats
  else --trace (show (uniqueDefsOfStats [] $## stats) ++ "\n")
       stats

removeSingleVarsInStats defs stats =
  concatMap (removeSingleVarsInStat defs) stats


maybeReplaceVar rep _ (SimpleVar _) = rep
maybeReplaceVar rep notrep (ComplexDef _ occ) =
  if occ<=1 then rep else notrep

removeSingleVarsInStat defs (JSAssign lhs rhs) =
  case lhs of
    JSIVar i -> maybe [JSAssign lhs newrhs]
                      (maybe [JSAssign lhs newrhs]
                             (maybeReplaceVar [] [JSAssign lhs newrhs]))
                      (lookup i defs)
    _ -> [JSAssign (removeSingleVarsInExp defs lhs) newrhs]
 where
  newrhs = removeSingleVarsInExp defs rhs

removeSingleVarsInStat defs (JSIf bexp s1 s2) =
  [JSIf  (removeSingleVarsInExp defs bexp)
         (removeSingleVarsInStats defs s1)
         (removeSingleVarsInStats defs s2)]

removeSingleVarsInStat defs (JSSwitch exp branches) =
  [JSSwitch (removeSingleVarsInExp defs exp)
            (map removeInBranch branches)]
 where
  removeInBranch (JSCase s sts) = JSCase s (removeSingleVarsInStats defs sts)
  removeInBranch (JSDefault sts) = JSDefault (removeSingleVarsInStats defs sts)

removeSingleVarsInStat defs (JSPCall s exps) =
  [JSPCall s (map (removeSingleVarsInExp defs) exps)]

removeSingleVarsInStat defs (JSReturn exp) =
  [JSReturn (removeSingleVarsInExp defs exp)]

removeSingleVarsInStat defs stat@(JSVarDecl i) =
  maybe [stat]
        (maybe [stat] (maybeReplaceVar [] [stat]))
        (lookup i defs)


removeSingleVarsInExp _ (JSString s) = JSString s
removeSingleVarsInExp _ (JSInt i) = JSInt i
removeSingleVarsInExp _ (JSBool b) = JSBool b
removeSingleVarsInExp defs (JSIVar i) =
  maybe (JSIVar i)
        (maybe (JSIVar i) replaceVar)
        (lookup i defs)
 where
  replaceVar (SimpleVar v) = removeSingleVarsInExp defs (JSIVar v) --dereference
  replaceVar (ComplexDef exp occ) =
    if occ<=1 then removeSingleVarsInExp defs exp
              else JSIVar i

removeSingleVarsInExp defs (JSIArrayIdx i j) =
  maybe (JSIArrayIdx i j)
        (maybe (JSIArrayIdx i j) replaceVar)
        (lookup i defs)
 where
  replaceVar (SimpleVar v) = JSIArrayIdx v j
  replaceVar (ComplexDef _ _) =
    error "Internal error in removeSingleVars in JSIArrayIdx"

removeSingleVarsInExp defs (JSOp op exp1 exp2) =
  JSOp op (removeSingleVarsInExp defs exp1) (removeSingleVarsInExp defs exp2)
removeSingleVarsInExp defs (JSFCall f exps) =
  JSFCall f (map (removeSingleVarsInExp defs) exps)
removeSingleVarsInExp defs (JSApply exp1 exp2) =
  JSApply (removeSingleVarsInExp defs exp1) (removeSingleVarsInExp defs exp2)
removeSingleVarsInExp defs (JSLambda params body) =
  JSLambda params (removeSingleVarsInStats defs body)


------------------------------------------------------------------------------
-- Top-level functions:

--- Reads a (Flat)Curry program and compiles it to JavaScript code that
--- is returned.
curry2js :: String -> IO String
curry2js modname = do
  prog <- readFlatCurry modname
  return (concatMap showJSFDecl (flatprog2JS prog))


-- Transforms a WUI program and some imported modules by inserting
-- JavaScript code for WUI conditions with ...`withConditionJS(Name)`
transformWUI :: String -> [String] -> String -> IO ()
transformWUI mainmodname imports target = do
  jscmodfuns <- mapIO getAndTransformWUIConditions (mainmodname:imports)
  let jscfuntags = concat jscmodfuns
      jscfuns = nub (map fst jscfuntags)
  putStrLn "WUI conditions (Curry predicates) to be translated into JavaScript:"
  putStr (concatMap ((++"\n") . showQName) jscfuns)
  targetnewer <- fileExistsAndNewerThan target (flatCurryFileName mainmodname)
  if targetnewer && not (or (map snd jscfuntags))
   then putStrLn "Nothing to be done!"
   else generateJavaScript mainmodname imports jscfuns target

-- Transforms a module containing WUI specifications by replacing
-- `withConditionsJS` by `withConditionsJSName` and return all functions
-- contained in such conditions.
getAndTransformWUIConditions :: String -> IO [(QName,Bool)]
getAndTransformWUIConditions modname = do
  prog <- readFlatCurry modname
  fcyname <- findFileInLoadPath (modname++".fcy")
  let jscfuns = jscOfProg prog
      newflatprogname = fcyname++"_withjs"
  if or (map snd jscfuns) -- is there some withConditionJS to be transformed?
   then do
    putStr $ "Changing WUI conditions in FlatCurry module '"++modname++"'..."
    writeFCY newflatprogname (replaceJscOfProg prog)
    system $ "mv "++newflatprogname++" "++fcyname
    putStrLn "done"
   else done
  return jscfuns

-- Generate a JavaScript program for Curry functions w.r.t. a main module:
generateJavaScript :: String -> [String] -> [QName] -> String -> IO ()
generateJavaScript mainmodname imports mainfuns target = do
  putStrLn "Computing Curry functions to be translated..."
  prog <- computeCompactFlatCurry (InitFuncs mainfuns : map Import imports)
                                  mainmodname
  putStrLn "Translating Curry functions to JavaScript..."
  writeFile target
            (concatMap showJSFDecl (flatprog2JS prog) ++
             "var LazyStringConversion = " ++
                 (if lazyStringConversion then "true" else "false") ++ ";\n\n")
  readFile (installDir++"/include/curry2js_prims.js") >>= appendFile target
  readFile (installDir++"/include/wui_prims.js") >>= appendFile target
  putStrLn $ "JavaScript program written into \""++target++"\""
  system $ "chmod 644 "++target
  done

-- Does newfile exists and is it newer than oldfile?
fileExistsAndNewerThan newfile oldfile = do
  nfexists <- doesFileExist newfile
  ofexists <- doesFileExist oldfile
  if nfexists && ofexists
   then do oftime <- getModificationTime oldfile
           nftime <- getModificationTime newfile
           return (nftime > oftime)
   else return False

showQName (m,f) = m++'.':f


-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
    [prog]                     -> curry2js prog >>= putStrLn
    ["-o",target,prog]         -> curry2js prog >>= writeFile target
    ("-wui":"-o":target:prog:imps) -> transformWUI prog imps target
    ("-wui":prog:imps)         -> transformWUI prog imps (prog++".js")
    _ -> putStrLn $ "ERROR: Illegal arguments: " ++
                    concat (intersperse " " args) ++ "\n" ++
                    "Usage: curry2js [-wui] [-o <targetfile>] <main_module_name> <imports_with_wuis>"


------------------------------------------------------------------------------

-- Testing:

mp = curry2js "test" >>= putStrLn

mo = do
  jscode <- curry2js "test"
  writeFile "test.js" (jscode ++ "\nalert(test_main());\n")
  system "chmod 644 test.js"
  done

------------------------------------------------------------------------------
