------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Curry Partial Evaluator 
-- v1.2 (May 28, 2003)
--
--
-- by Elvira Albert, German Vidal (UPV), and Michael Hanus (CAU Kiel)
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import Flat
import List
import Flat2Fcy
import Unsafe(trace)
import System
import Distribution(inCurrySubdir)

------------------------------------------------------------------------------
-- some global definitions:

printVersion :: IO ()
printVersion =
  do putStrLn "Curry Partial Evaluator (Version 1.2 of 28/05/2003)"
     putStrLn "(UP Valencia, CAU Kiel)\n"

-- should some debug information be printed during partial evaluation?
pevalDebug :: Bool
pevalDebug = False

-- which kind of abstraction operator:
--pevalAbs = 0     -- no generalization (termination is not ensured)
--pevalAbs = 1     -- generalization based on a well-founded order (termination ensured)
pevalAbs :: Int
pevalAbs = 2     -- generalization based on a well-quasi order 
                 -- (termination is ensured even for integers by translating them to expressions)

-- note that only 0 and 2 really pass the "KMP-test"...
 
-- maximal number of variables occurring in each unfolded expression

-- (used in getMatchedRHS to produce a safe renaming):
maxExpVars :: Int
maxExpVars = 1000

-- special variable:
varBottom :: Expr
varBottom = Var (-1)

-- treatment of PEVAL annotations in the program to be partially evaluated:
-- is this call a PEVAL annotations?
isPevalAnnot :: Expr -> Bool
isPevalAnnot (Comb ctype name args) =
  if ctype==FuncCall && length args == 1 && take 5 (reverse name) == "LAVEP"
  then True  -- this is a call annotated with PEVAL
  else False

-- get the annotation expression (provided that this is a PEVAL annotation):
getPevalAnnotExpr :: Expr -> Expr
getPevalAnnotExpr (Comb _ _ args) = head args

------------------------------------------------------------------------------
-- data structures used in the partial evaluator:

-- data type for substitutions:
data Subst = Sub [Int] [Expr] | FSub
  deriving Eq

-- encapsulating the state during the peval process:
-- the state contains:
-- * the list of all function declarations
-- * index for renamed variables
-- * number of allowed unfoldings of function calls

data PEvalState = PEState [Expr] [FuncDecl] Int Int
 deriving Eq

getExps :: PEvalState -> [Expr]
getExps          (PEState exps _ _ _) = exps
getFuncDecls :: PEvalState -> [FuncDecl]
getFuncDecls     (PEState _ fds _ _) = fds
getRenamingIndex :: PEvalState -> Int
getRenamingIndex (PEState _ _ i _) = i
getDepth :: PEvalState -> Int
getDepth         (PEState _ _ _ d) = d

-- update peval state with new index for renamed variables:
updateRenamingIndex :: PEvalState -> Int -> PEvalState
updateRenamingIndex (PEState exps fds _ d) i = PEState exps fds i d

incrDepth :: PEvalState -> PEvalState
incrDepth (PEState exps fds i d) = PEState exps fds i (d+1)

decrDepth :: PEvalState -> PEvalState
decrDepth (PEState exps fds i d) = PEState exps fds i (d-1)

-- compute arity of a function symbol:
getArity :: PEvalState -> String -> Int
getArity pst f = getArityF (getFuncDecls pst) f

getArityF :: [FuncDecl] -> String -> Int
getArityF funs f
 | f=="+"    = 2
 | f=="-"    = 2
 | f=="*"    = 2
 | f=="<"    = 2
 | f==">"    = 2
 | f=="<="   = 2
 | f==">="   = 2
 | f=="&"    = 2
 | f=="&&"   = 2
 | f=="=:="  = 2
 | f=="=="   = 2
 | f=="if_then_else" = 3
 | f=="map"          = 2
 | f=="foldl"        = 3
 | f=="foldr"        = 3
 | f=="filter"       = 2
 | f=="iterate"      = 2
 | otherwise = findArity funs
 where
    findArity [] = error ("INTERNAL ERROR: cannot find arity of "++f)
    findArity (Func fn fa _ _ : fds) =
        if fn==f then fa else findArity fds

------------------------------------------------------------------------------
-- Top-level functions..
------------------------------------------------------------------------------

-- Top-level function for calling the partial evaluator as a saved state:
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
   then putStrLn $ "ERROR: Illegal arguments: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: peval.state <module_name>"
   else mainProg (stripCurrySuffix (head args))

stripCurrySuffix :: String -> String
stripCurrySuffix n = let rn = reverse n in
  if take 6 rn == "yrruc." then reverse (drop 6 rn) else
  if take 7 rn == "yrrucl." then reverse (drop 7 rn) else n


-- Execute peval with the name of a program.
-- The expressions to be partially evaluated must be marked by (PEVAL ...)
-- where PEVAL is the identity function (e.g., defined in the prelude)
mainProg :: String -> IO ()
mainProg p =
  do printVersion
     annprog <- readFlatCurry p
     let (prog,es) = getEvalExprs annprog
     main_aux p (removeRenamings (removeVarsLits es)) annprog prog

-- Execute peval with the name of a program and a list of expressions
-- to be partially evaluated:
mainWithExprs :: String -> [Expr] -> IO ()
mainWithExprs p es =
  do printVersion
     prog <- readFlatCurry p
     main_aux p (removeRenamings (removeVarsLits es)) prog prog

main_aux :: String -> [Expr] -> Prog -> Prog -> IO ()
main_aux p [] prog _ = 
  do putStrLn "There are no calls to start partial evaluation..."
     putStrLn("\nWriting original program into \""++pefile++"\"...\n")
     writeFCY pefile prog
  where pefile = inCurrySubdir (p++"_pe") ++ ".fcy"
main_aux p (e:es) annprog prog =
  do putStrLn ("Annotated expressions to be partially evaluated:\n")
     putStrLn (concatMap (\ex->ppExpr 0 ex ++"\n") (e:es))
     putStrLn ("Starting peval for program \""++p++"\"...\n")
     putStrLn("Independent Renaming:\n")
     ppRen exps2
     --putStrLn("\nSpecialized Program (before renaming):\n")
     --ppResultants resultantsAux
     putStrLn("\nSpecialized Program:\n")
     ppResultants resultants
     putStrLn("\nAfter Post-unfolding:\n")
     ppResultants postResultants
     --putStrLn("\nAfter transformation into program rules:\n")
     --ppFuncDecls specFuncs
     putStrLn("\nWriting specialized program into \""++pefile++"\"...\n")
     writeFCY pefile (addPEFuncDecls exps2 annprog specFuncs)
  where
    exps = reverse (pevalProg prog (e:es))
    exps2 = build_ren exps 0
    --resultantsAux = buildResultants2 prog exps exps2
    resultants = buildResultants prog exps exps2
    postResultants = postUnfolding resultants (concat (map (originalFunc prog exps2) (e:es)))
    specFuncs = map resultant2fundecl postResultants
    pefile = inCurrySubdir (p++"_pe") ++ ".fcy"

-- transform a resultant into a function declaration:
resultant2fundecl :: (Expr,Expr) -> FuncDecl
resultant2fundecl (Comb FuncCall f vars, rhs) =
    Func f arity (genPolyType arity) (Rule (map (\(Var i)->i) vars) rhs)
    -- here we don't compute the real type of the function (TO DO!)
  where
    arity = length vars

    -- generate polymorphic type of the form (t1 -> t2 -> ... -> tn)
    -- where all ti's are different type variables:
    genPolyType n = if n==0 then TVar 0
                            else FuncType (TVar n) (genPolyType (n-1))

-- add all function declarations from the partially evaluated program
-- to the original program and replace also the PEVAL calls in the
-- original program by the renamed pe-functions calls:
addPEFuncDecls :: [(Expr,Expr)] -> Prog -> [FuncDecl] -> Prog
addPEFuncDecls renaming
               (Prog modname imports types oldfuns ops trans)
               newfuns =
    Prog modname imports types (modfuns++newfuns) ops (trans++newtrans)
  where
    -- new peval function names in translation table to make them available:
    newtrans = map (\f->Trans (if take (length modname + 1) f == modname++"."
                               then drop (length modname + 1) f
                               else f)  f)
                   (map (\(Func f _ _ _)->f) newfuns)

    -- replace PEVAL calls by renamed expressions:
    modfuns = map (replacePevalAnnotInFunc oldfuns renaming) oldfuns

-- replace all function declarations of a given program by new ones:
-- (to do: modify also operator and translation tables correctly)
replaceFuncDecls :: Expr -> [FuncDecl] -> Expr
replaceFuncDecls (Prog modname imports types oldfuns ops trans) newfuns =
    Prog modname imports types newfuns ops (typetrans++newtrans)
  where
    -- keep names of types in translation table:
    oldfunnames = map (\(Func f _ _ _)->f) oldfuns
    typetrans = filter (\(Trans _ name) -> name `notElem` oldfunnames) trans
    -- new names in translation table to make them available:
    newtrans = map (\f->Trans f f) (map (\(Func f _ _ _)->f) newfuns)


---------------------------------------------------------------------------
-- extract all expressions e embedded in a call (PEVAL e) in a program
-- and replace these calls by e:

getEvalExprs :: Prog -> (Prog,[Expr])
getEvalExprs (Prog modname imports types oldfuns ops trans) =
  (Prog modname imports types newfuns ops trans, evalexps)
 where
   funexppairs = map getFuncEvalExprs oldfuns
   newfuns = map fst funexppairs
   evalexps = concatMap snd funexppairs
   
getFuncEvalExprs :: FuncDecl -> (FuncDecl,[Expr])
getFuncEvalExprs (Func name arity ftype (External ename)) =
  (Func name arity ftype (External ename), [])
getFuncEvalExprs (Func name arity ftype (Rule lhs rhs)) =
  (Func name arity ftype (Rule lhs newrhs), evalexps)
 where
   (newrhs,evalexps) = getEvalExprsInExp rhs

getEvalExprsInExp :: Expr -> (Expr,[Expr])
getEvalExprsInExp (Var i) = (Var i, [])
getEvalExprsInExp (Lit l) = (Lit l, [])
getEvalExprsInExp (Comb ctype name args)
  | isPevalAnnot (Comb ctype name args)
              = let expr = getPevalAnnotExpr (Comb ctype name args)
                 in (expr,[expr])
  | otherwise = let (newargs,pexps) = getEvalExprsInExps args
                 in (Comb ctype name newargs, pexps)
getEvalExprsInExp (Apply e1 e2) =
 let (ne1,pe1) = getEvalExprsInExp e1
     (ne2,pe2) = getEvalExprsInExp e2
  in (Apply ne1 ne2, pe1++pe2)
getEvalExprsInExp (Constr vars e) =
 let (ne,pe) = getEvalExprsInExp e
  in (Constr vars ne, pe)
getEvalExprsInExp (Or e1 e2) =
 let (ne1,pe1) = getEvalExprsInExp e1
     (ne2,pe2) = getEvalExprsInExp e2
  in (Or ne1 ne2, pe1++pe2)
getEvalExprsInExp (Case ctype e bs) =
 let (ne,pe1) = getEvalExprsInExp e
     (nbs,pe2) = getEvalExprsInBranches bs
  in (Case ctype ne nbs, pe1++pe2)
getEvalExprsInExp (Choice e) =
 let (ne,pe) = getEvalExprsInExp e
  in (Choice ne, pe)
getEvalExprsInExp (GuardedExpr vars e1 e2) =
 let (ne1,pe1) = getEvalExprsInExp e1
     (ne2,pe2) = getEvalExprsInExp e2
  in (GuardedExpr vars ne1 ne2, pe1++pe2)

getEvalExprsInExps :: [Expr] -> ([Expr],[Expr])
getEvalExprsInExps [] = ([],[])
getEvalExprsInExps (e:es) = let (ne,pe1) = getEvalExprsInExp e
                                (nes,pe2) = getEvalExprsInExps es
                             in (ne:nes, pe1++pe2)

getEvalExprsInBranches :: [BranchExpr] -> ([BranchExpr],[Expr])
getEvalExprsInBranches [] = ([],[])
getEvalExprsInBranches (Branch p e : bs) =
   let (ne,pe1) = getEvalExprsInExp e
       (nbs,pe2) = getEvalExprsInBranches bs
    in (Branch p ne : nbs, pe1++pe2)


---------------------------------------------------------------------------
-- replace all PEVAL annotations by renamed expressions:

replacePevalAnnotInFunc :: [FuncDecl] -> [(Expr,Expr)] -> FuncDecl -> FuncDecl
replacePevalAnnotInFunc _ _ (Func name arity ftype (External ename)) =
  Func name arity ftype (External ename)
replacePevalAnnotInFunc funs ren (Func name arity ftype (Rule lhs rhs)) =
  Func name arity ftype (Rule lhs (replacePevalAnnotInExp funs ren rhs))

replacePevalAnnotInExp :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> Expr
replacePevalAnnotInExp _ _ (Var i) = Var i
replacePevalAnnotInExp _ _ (Lit l) = Lit l
replacePevalAnnotInExp funs ren (Comb ctype name args)
  | isPevalAnnot (Comb ctype name args)
              = let expr = getPevalAnnotExpr (Comb ctype name args)
                 in ren_rho funs ren (SQ expr)
  | otherwise = Comb ctype name (map (replacePevalAnnotInExp funs ren) args)
replacePevalAnnotInExp funs ren (Apply e1 e2) =
  Apply (replacePevalAnnotInExp funs ren e1)
        (replacePevalAnnotInExp funs ren e2)
replacePevalAnnotInExp funs ren (Constr vars e) =
  Constr vars (replacePevalAnnotInExp funs ren e)
replacePevalAnnotInExp funs ren (Or e1 e2) =
  Or (replacePevalAnnotInExp funs ren e1) (replacePevalAnnotInExp funs ren e2)
replacePevalAnnotInExp funs ren (Case ctype e bs) =
  Case ctype (replacePevalAnnotInExp funs ren e)
             (map replacePevalAnnotInBranch bs)
 where
   replacePevalAnnotInBranch (Branch p be) =
                              Branch p (replacePevalAnnotInExp funs ren be)
replacePevalAnnotInExp funs ren (Choice e) =
  Choice (replacePevalAnnotInExp funs ren e)
replacePevalAnnotInExp funs ren (GuardedExpr vars e1 e2) =
  GuardedExpr vars (replacePevalAnnotInExp funs ren e1)
                   (replacePevalAnnotInExp funs ren e2)


---------------------------------------------------------------------------
---post-unfolding:

originalFunc :: Prog -> [(Expr,Expr)] -> Expr -> [String]
originalFunc (Prog _ _ _ funs _ _) rho e = originalF funs rho e

originalF :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> [String]
originalF funs rho e = funcsInExp ren
  where ren = ren_rho funs rho (SQ e)

funcsInExp :: Expr -> [String]
funcsInExp (Var _) = []
funcsInExp (Lit _) = []
funcsInExp (Comb t f es) 
  | t==ConsCall   = funcsInExpL es
  | t==PartCall   = funcsInExpL es
  | otherwise     = [f]++(funcsInExpL es)
funcsInExp (Apply e1 e2) = (funcsInExp e1)++(funcsInExp e2)
funcsInExp (Constr _ e) = funcsInExp e
funcsInExp (Case _ e cases) = 
  (funcsInExp e)++(funcsInExpL (branches cases))
funcsInExp (GuardedExpr _ gc e) = (funcsInExp gc)++(funcsInExp e)
funcsInExp (Or e1 e2) = (funcsInExp e1)++(funcsInExp e2)
funcsInExp (Choice e) = funcsInExp e

funcsInExpL :: [Expr] -> [String]
funcsInExpL [] = []
funcsInExpL (e:es) = 
   (funcsInExp e)++(funcsInExpL es)

postUnfolding :: [(Expr,Expr)] -> [String] -> [(Expr,Expr)]
postUnfolding prog funcs =
  if newProg==prog then prog
     else postUnfolding newProg funcs 
     where newProgTemp = postUnfoldingAux prog prog
           newProgAux = removeRedundantRules newProgTemp newProgTemp funcs
           newProg = removeFails newProgAux

postUnfoldingAux :: [(Expr,Expr)] -> [(Expr,Expr)] -> [(Expr,Expr)]
postUnfoldingAux _ [] = []
postUnfoldingAux prog ((l,r):es) =
  if r==rUnf then (l,r) : (postUnfoldingAux prog es)
             else (l,rUnf) : es --DO NOT APPLY IT TO 'es'..
       where rUnf = try_unfold prog r

try_unfold :: [(Expr,Expr)] -> Expr -> Expr
try_unfold _ (Var x) = Var x
try_unfold _ (Lit l) = Lit l
try_unfold prog (Comb ConsCall c es) = Comb ConsCall c (try_unfoldL prog es)
try_unfold prog (Comb PartCall c es) = Comb PartCall c (try_unfoldL prog es)
try_unfold prog (Comb FuncCall f es) = 
  if nonRecursive prog f
     then unfoldFC prog prog (Comb FuncCall f es)
     else if uselessFunc prog f
             then unfoldFC prog prog (Comb FuncCall f es)
             else Comb FuncCall f (try_unfoldL prog es)
--  if n==1 then unfoldFC prog prog (Comb FuncCall f es)
--          else if nonRecursive prog f
--                  then unfoldFC prog prog (Comb FuncCall f es)
--                  else Comb FuncCall f (try_unfoldL prog es)
--  where n = countFuncCalls prog f
try_unfold prog (Apply e1 e2) = 
  if (isPartCall e1)
     then try_unfold_apply prog e1 e2
     else Apply (try_unfold prog e1) (try_unfold prog e2)
try_unfold prog (Constr gc e) = Constr gc (try_unfold prog e)
try_unfold prog (Case ctype e cases) = 
  Case ctype (try_unfold prog e) (map (try_unf_cases prog) cases)
try_unfold prog (GuardedExpr vars gc e) = GuardedExpr vars gc (try_unfold prog e)
try_unfold prog (Or e1 e2) = 
  Or (try_unfold prog e1) (try_unfold prog e2)
try_unfold prog (Choice e) = Choice (try_unfold prog e)
--try_unfold funs prog (SQ e) = SQ (try_unfold funs prog e)

try_unfoldL :: [(Expr,Expr)] -> [Expr] -> [Expr]
try_unfoldL _ [] = []
try_unfoldL prog (e:es) = (try_unfold prog e) : (try_unfoldL prog es)

try_unf_cases :: [(Expr,Expr)] -> BranchExpr -> BranchExpr
try_unf_cases prog (Branch p e) = Branch p (try_unfold prog e) 

try_unfold_apply :: [(Expr,Expr)] -> Expr -> Expr -> Expr
try_unfold_apply prog (Comb PartCall f es) e2 =
  if (getArityP prog f)==(length es + 1)
     then Comb FuncCall f (es++[e2])
     else Comb PartCall f (es++[e2])

getArityP :: [(Expr,Expr)] -> String -> Int
getArityP prog f
 | f=="+"    = 2
 | f=="-"    = 2
 | f=="*"    = 2
 | f=="<"    = 2
 | f==">"    = 2
 | f=="<="   = 2
 | f==">="   = 2
 | f=="&"    = 2
 | f=="&&"   = 2
 | f=="=:="  = 2
 | f=="=="   = 2
 | f=="if_then_else" = 3
 | f=="map"          = 2
 | f=="foldl"        = 3
 | f=="foldr"        = 3
 | f=="filter"       = 2
 | f=="iterate"      = 2
 | otherwise = findAr prog
 where
    findAr [] = error ("INTERNAL ERROR: cannot find arity of "++f)
    findAr ((Comb FuncCall fn es, _) : fds) =
        if fn==f then (length es) else findAr fds


unfoldFC :: [(Expr,Expr)] -> [(Expr,Expr)] -> Expr -> Expr
unfoldFC [] prog (Comb FuncCall f es) = Comb FuncCall f (try_unfoldL prog es)
unfoldFC (((Comb FuncCall g vars),r):rules) prog (Comb FuncCall f es) = 
  if f==g && n==0 then propCaseVars (substitute (vars2index vars) es r)
                  else unfoldFC rules prog (Comb FuncCall f es)
  where n = countFuncCalls [((Comb FuncCall g vars),r)] g

propCaseVars :: Expr -> Expr
propCaseVars e = case e of
  (Case ctype (Var x) ces) ->
              Case ctype (Var x) (map propCaseVarsBranch (propBinding x ces))
  _ -> e
propCaseVarsBranch :: BranchExpr -> BranchExpr
propCaseVarsBranch (Branch pat e) = Branch pat (propCaseVars e)

nonRecursive :: [(Expr,Expr)] -> String -> Bool
nonRecursive [] _ = False
nonRecursive ((Comb FuncCall g _, r) : rules) f =
  if f==g then (countFuncs r)==0     --only rhs's with ctes and vbles are allowed.. TO DO..
          else nonRecursive rules f

--note that countFuncs counts all defined functions and countFuncCalls only the
--occurrences of a given function..
countFuncs :: Expr -> Int
countFuncs (Var _) = 0
countFuncs (Lit _) = 0
countFuncs (Comb t f es) 
  | t==ConsCall    = countFuncsL es
  | legalBuiltin f = countFuncsL es
  | f=="success"   = 0
  | f=="failed"    = 0
  | otherwise      = 1   -- + (countFuncsL es)  --not needed for its current use..
countFuncs (Apply e1 e2) = (countFuncs e1) + (countFuncs e2)
countFuncs (Constr _ e) = countFuncs e
countFuncs (Case _ e cases) = 
  (countFuncs e) + (countFuncsL (branches cases))
countFuncs (GuardedExpr _ gc e) = (countFuncs gc) + (countFuncs e)
countFuncs (Or e1 e2) = (countFuncs e1) + (countFuncs e2)
countFuncs (Choice e) = countFuncs e
--countFuncs (SQ e) = countFuncs e

countFuncsL :: [Expr] -> Int
countFuncsL [] = 0
countFuncsL (e:es) =
   (countFuncs e) + (countFuncsL es)

uselessFunc :: [(Expr,Expr)] -> String -> Bool
uselessFunc prog f = (countFuncCalls prog f)==1

countFuncCalls :: [(Expr,Expr)] -> String -> Int
countFuncCalls [] _ = 0
countFuncCalls ((_,r):es) f = (countFC r f) + (countFuncCalls es f)

countFC :: Expr -> String -> Int
countFC (Var _) _ = 0
countFC (Lit _) _ = 0
--countFC (Comb ConsCall _ es) f = countFCList es f
--countFC (Comb PartCall _ es) f = countFCList es f
countFC (Comb _ g es) f = 
  if f==g then 1 + (countFCList es f) else countFCList es f
countFC (Apply e1 e2) f = (countFC e1 f) + (countFC e2 f)
countFC (Constr _ e) f = countFC e f
countFC (Case _ e cases) f = 
  (countFC e f) + (countFCList (branches cases) f)
countFC (GuardedExpr _ gc e) f = (countFC gc f) + (countFC e f)
countFC (Or e1 e2) f = (countFC e1 f) + (countFC e2 f)
countFC (Choice e) f = countFC e f
--countFC (SQ e) f = countFC e f

countFCList :: [Expr] -> String -> Int
countFCList [] _ = 0
countFCList (e:es) f =
   (countFC e f) + (countFCList es f)
   
removeRedundantRules :: [(Expr,a)] -> [(Expr,Expr)] -> [String] -> [(Expr,a)]
removeRedundantRules [] _ _ = []
removeRedundantRules (rule:es) prog funcs =
  if (funcRule rule) `elem` funcs then rule : (removeRedundantRules es prog funcs)
     else if (countFuncCalls prog (funcRule rule))==0
          then removeRedundantRules es prog funcs
          else rule : (removeRedundantRules es prog funcs)

funcRule :: (Expr,_) -> String
funcRule ((Comb FuncCall f _),_) = f

removeFails :: [(Expr,Expr)] -> [(Expr,Expr)]
removeFails [] = []
removeFails ((lhs,rhs):es) = (lhs,(removeFB rhs)) : (removeFails es)

removeFB :: Expr -> Expr
removeFB x = case x of
 -- (Case _ _ [Branch _ (Comb FuncCall "failed" [])]) -> Comb FuncCall "failed" []
 (Case ctype e ces) -> let newces = removeFailedBranches ces in
                       if newces==[] 
                       then Comb FuncCall "failed" [] 
                       else Case ctype (removeFB e) (map removeFBbranch newces)
 (Comb c f es) -> Comb c f (map removeFB es)
 (Apply a b) -> Apply (removeFB a) (removeFB b)
 (Constr v e) -> Constr v (removeFB e)
 (GuardedExpr v gc e) -> GuardedExpr v (removeFB gc) (removeFB e)
 (Or a b) -> Or (removeFB a) (removeFB b)
 (Choice e) -> Choice (removeFB e)
 --removeAux (SQ e) = SQ (removeFB e)
 e -> e

removeFailedBranches :: [BranchExpr] -> [BranchExpr]
removeFailedBranches [] = []
removeFailedBranches ((Branch pat e):es) = 
  if e==(Comb FuncCall "failed" []) 
     then removeFailedBranches es
     else (Branch pat e) : (removeFailedBranches es)

removeFBbranch :: BranchExpr -> BranchExpr
removeFBbranch (Branch pat e) = Branch pat (removeFB e)

---end post-unfolding
---------------------------------------------------------------------------

--partial evaluation of a program (only the defined functions are used)
pevalProg :: Prog -> [Expr] -> [Expr]
pevalProg (Prog _ _ _ funs _ _) es = map delSQ (peval0 funs 0 es [])

--global iterative algorithm for PE:
-- (pairs/newpairs are used to record the PE of terms in order to avoid the
--  recomputation of the same terms once and again..)
--
peval0 :: [FuncDecl] -> Int -> [Expr] -> [(Expr,Expr)] -> [Expr]
peval0 funs n es pairs = if (equalMR es newEs) then es 
  else peval0 funs (n+1) newEs newpairs
  where newpairs = pevalL funs n es pairs
        newElements = diffPairs (map snd newpairs) (map snd pairs)
        newEs = removeRenamings (abstract funs n newElements es)
--        newEs = removeRenamings (abstract funs n (map snd newpairs) es)

-- we only need to apply abstract over the "new" elements..
diffPairs :: [Expr] -> [Expr] -> [Expr]
diffPairs [] _ = []
diffPairs (e:es) olds = 
  if e `elem` olds then diffPairs es olds else e : (diffPairs es olds)

--peval of a list of expressions
pevalL :: [FuncDecl] -> Int -> [Expr] -> [(Expr,Expr)] -> [(Expr,Expr)]
pevalL _ _ [] _ = []
pevalL funs n (e:es) pairs = 
  if spe == varBottom
     then  pevalL_aux funs n (e:es) pairs
     else  (e,spe) : (pevalL funs n es pairs)
  where spe = searchPair e pairs

pevalL_aux :: [FuncDecl] -> Int -> [Expr] -> [(Expr,Expr)] -> [(Expr,Expr)]
pevalL_aux funs n (e:es) pairs
  | if pevalDebug
    then trace (ppExpr 0 e ++ " => " ++ ppExpr 0 spe ++ "\n") success
    else success
  = strict ((e,spe) : (pevalL funs n es ((e,spe):pairs)))
 where spe = peval (PEState (map snd pairs) funs (maxVarIndex e) 0) n e
-- if we replace "strict" by "id" above, the computation is lazy
-- but needs much more time (90 times longer for KMP benchmark)!
strict :: a -> a
strict x = id $!! x

--recover the stored partial evaluation of a given term (or returns varBottom)
searchPair :: Expr -> [(Expr,Expr)] -> Expr
searchPair _ [] = varBottom
searchPair e ((x,xpe) : es) = if e==x then xpe else searchPair e es

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-- global level:


-- abstraction operator:
abstract:: [FuncDecl] -> Int -> [Expr] -> [Expr] -> [Expr]
abstract _ _ [] es = es
abstract funs n (e:es) old = abstract funs n es (abs funs n e old)

--abstraction for terms not within square brackets:
abs :: [FuncDecl] -> Int -> Expr -> [Expr] -> [Expr]
abs _ _ (Var _) es = es
abs _ _ (Lit _) es = es
abs funs n (Comb FuncCall c e) es 
  | pevalAbs==0  = absFun funs n (Comb FuncCall c e) es
  | pevalAbs==1  = if definedFunc2 funs c
                      then absFunWfo funs n (Comb FuncCall c e) es
                      else abstract funs n e es
  | pevalAbs==2  = if definedFunc2 funs c
                      then absFunWqo funs n (Comb FuncCall c e) es
                      else abstract funs n e es
abs funs n (Comb ConsCall _ e) es = abstract funs n e es
abs funs n (Comb PartCall f e) es = addPartCall funs n f e es
abs funs n (Apply e1 e2) es = abstract funs n [e1,e2] es
abs funs n (Constr _ e) es = abs funs n e es
abs funs n (Case _ e cases) es = abstract funs n (e:(branches cases)) es
abs funs n (Or e1 e2) es = abstract funs n [e1,e2] es
abs funs n (GuardedExpr _ gc e) es = abstract funs n [gc,e] es
abs funs n (Choice e) es = abs funs n e es
abs funs n (SQ e) es
  | pevalAbs==0  = absSQ funs e es
  | pevalAbs==1  = absFunWfo funs n e es
  | pevalAbs==2  = absFunWqo funs n e es

absSQ :: [FuncDecl] -> Expr -> [Expr] -> [Expr]
absSQ funs e es =
  if constructorInstance funs e es  --existsRen es e
     then es
     else e : es

addPartCall :: [FuncDecl] -> Int -> String -> [Expr] -> [Expr] -> [Expr]
addPartCall funs n f e es = 
  if definedFunc2 funs f
     then addPC funs f e (getArityF funs f) es n
     else abstract funs n e es

addPC :: [FuncDecl] -> String -> [Expr] -> Int -> [Expr] -> Int -> [Expr]
addPC funs f e n es i = 
  abs funs n (Comb FuncCall f args) es
  where args = complete e n i

absFun :: [FuncDecl] -> Int -> Expr -> [Expr] -> [Expr]
absFun funs n (Comb FuncCall c e) es =
  if constructorInstance funs e2 es  --existsRen es e2
      then es
      else if (definedFunc2 funs c) -- || (legalBuiltin c)
            then e2 : es
            else abstract funs n e es
  where e2 = delSQ (Comb FuncCall c e)

--abstraction based on a well-founded order:
absFunWfo :: [FuncDecl] -> Int -> Expr -> [Expr] -> [Expr]
absFunWfo funs n e es
  | com==Var (-1)                    = e2 : es  --no comparable (same root) term: added
  | existsRen es e2                  = es       --there is already a renaming: ignored
  | smaller e2 com                   = e2 : es  --it is smaller: added
  | constructorInstance funs e2 es   = es       --there is a constructor instance: ignored
  | definedFunc2 funs (headF e2)     = abstract funs n (msgT e2 com) res  --es instead of res??
  | isCase e                         = abstract funs n (splitCase e) es
  | otherwise                        = e2 : es  --for builtins, Apply, etc..
  -- | otherwise                       = abs funs n e es
 where e2 = delSQ e
       (com,res) = comparable e2 es

headF :: Expr -> String
headF e =  case e of
             (Comb _ f _)  -> f
             _             -> "no function"

--split a Case expression into a Case expresion with a variable argument and the
--original argument:
splitCase :: Expr -> [Expr]
splitCase (Case ctype e ces) =
  [Case ctype (Var ((maxVarIndex (Case ctype e ces))+1)) ces, e]

--abstraction based on a well-quasi order:
absFunWqo :: [FuncDecl] -> Int -> Expr -> [Expr] -> [Expr]
absFunWqo funs n e es
  | com==Var (-1)                    = e2 : es  --no comparable (same root) term: added
  | existsRen es e2                  = es       --there is already a renaming: ignored
  | constructorInstance funs e2 es   = es       --there is a constructor instance: ignored
  | otherwise                        = absFunWqoAux funs n es e2
  where e2 = delSQ e
        (com,_) = comparable e2 es

--note that above there is no call to the closedness test: it is embedded
--into the call to msg..

absFunWqoAux :: [FuncDecl] -> Int -> [Expr] -> Expr -> [Expr]
absFunWqoAux funs n es e
  | emb==Var (-1)               = e : es     --it doesn't embed a previous term: added
  | definedFunc2 funs (headF e) = abstract funs n (msgT e emb) res  --es instead of res??
  | isCase e                    = abstract funs n (splitCase e) es
  | otherwise                   = e : es    -- for builtins, Apply, etc..
  where (emb,res) = embedsPre e es

legalBuiltin :: String -> Bool
legalBuiltin c
  | c=="=="           = True
  | c=="if_then_else" = True
  | c=="&&"           = True
  | c=="&"            = True
  | c=="=:="          = True
  | c=="*"            = True
  | c=="+"            = True
  | c=="-"            = True
  | c=="<"            = True
  | c==">"            = True
  | c=="<="           = True
  | c==">="           = True
  | c=="map"          = True
  | c=="foldl"        = True
  | c=="foldr"        = True
  | c=="filter"       = True
  | c=="iterate"      = True
  | otherwise         = False

arithBuiltin :: String -> Bool
arithBuiltin c
  | c=="=="           = True
  | c=="if_then_else" = False
  | c=="&&"           = True
  | c=="&"            = True
  | c=="=:="          = True
  | c=="*"            = True
  | c=="+"            = True
  | c=="-"            = True
  | c=="<"            = True
  | c==">"            = True
  | c=="<="           = True
  | c==">="           = True
  | c=="map"          = False
  | c=="foldl"        = False
  | c=="foldr"        = False
  | c=="filter"       = False
  | c=="iterate"      = False
  | otherwise         = False

nameConversion :: String -> String
nameConversion c
  | c=="=="           = "rigidEq"
  | c=="if_then_else" = "ite"
  | c=="&&"           = "seqAnd"
  | c=="&"            = "conAnd"
  | c=="=:="          = "flexEq"
  | c=="*"            = "mult"
  | c=="+"            = "sum"
  | c=="-"            = "subs"
  | c=="<"            = "lesser"
  | c==">"            = "greater"
  | c=="<="           = "lte"
  | c==">="           = "gte"
  | otherwise         = c

branches :: [BranchExpr] -> [Expr]
branches [] = []
branches ((Branch _ e):es) = e : branches es

constructorInstance :: [FuncDecl] -> Expr -> [Expr] -> Bool
constructorInstance funs e es = 
  if sub==FSub then False else isConstructor funs sub
  where (sub,_) = search_instance e es

isConstructor :: [FuncDecl] -> Subst -> Bool
isConstructor funs (Sub _ expr) = andL (map (isCons funs) expr)
--this definition considers any non-defined symbol as constructor..
isCons :: [FuncDecl] -> Expr -> Bool
isCons _ (Var _) = True
isCons _ (Lit _) = True
isCons funs (Comb FuncCall f e) = if (definedFunc2 funs f) || (legalBuiltin f)
  then False 
  else andL (map (isCons funs) e)
isCons funs (Comb ConsCall _ e) = andL (map (isCons funs) e)
isCons funs (Comb PartCall f e) = if (definedFunc2 funs f) || (legalBuiltin f)
  then False 
  else andL (map (isCons funs) e)
isCons funs (Apply e1 e2) = andL (map (isCons funs) [e1,e2])
isCons funs (Constr _ e) = isCons funs e
isCons funs (Case _ e ces) = if (isCons funs e) 
  then isConsCase funs ces
  else False
isCons funs (Or e1 e2) = andL (map (isCons funs) [e1,e2])
isCons funs (GuardedExpr _ e1 e2) = andL (map (isCons funs) [e1,e2])
isCons funs (Choice e) = isCons funs e
--isCons _ (SQ _) = False  --isCons funs e

isConsCase :: [FuncDecl] -> [BranchExpr] -> Bool
isConsCase _ [] = True
isConsCase funs (Branch _ e : les) =
  if (isCons funs e)
  then isConsCase funs les
  else False

--completes the arguments of a partcall with fresh variables
complete :: [Expr] -> Int -> Int -> [Expr]
complete [] n i = completeVars n i
complete (e:es) n i = e : (complete es (n-1) i)

completeVars :: Int -> Int -> [Expr]
completeVars n i = 
  if n==0 then [] else (Var ((maxExpVars*i)-n)) : (completeVars (n-1) i)

-------------------------------------------------
--well-founded order based on the size of a term:

--size of a term:
size :: Expr -> Int
size (Var _) = 0
size (Lit _) = 0
size (Comb _ _ []) = 0
size (Comb _ _ (e:es)) = 1 + (foldl (+) 0 (map size (e:es)))
size (Apply e1 e2) = 1 + (size e1) + (size e2)
size (Constr _ e) = 1 + (size e)
size (Case _ e ces) = 1 + (size e) + (foldl (+) 0 (map sizeBranch ces))
size (GuardedExpr _ c e) = 1 + (size c) + (size e)
size (Choice e) = 1 + (size e)
--size (SQ e) = size e

sizeBranch :: BranchExpr -> Int
sizeBranch (Branch _ e) = size e

--test based on a well-founded order:
smaller :: Expr -> Expr -> Bool
smaller x y = (size x) < (size y) 

--------------------------------------------------------------------
--well-quasi order based on the embedding:
--(integers are translated to expressions to ensure termination..)

embedsPre :: Expr -> [Expr] -> (Expr,[Expr])
embedsPre e es = embedsP e [] es

embedsP :: Expr -> [Expr] -> [Expr] -> (Expr,[Expr])
embedsP _ es [] = (Var (-1),es)
embedsP e es (h:t) =
  if headExpr e == headExpr h && embeds e h  --here we require to have the same head symbol
     then (h, es++t)
     else embedsP e (es++[h]) t

embeds :: Expr -> Expr -> Bool
embeds e h =
  if headExpr e == headExpr h && embedsSameHead e h
     then True
     else embedsArg e h

embedsSameHead :: Expr -> Expr -> Bool
embedsSameHead (Var _) (Var _) = True
embedsSameHead (Lit (Charc x)) (Lit (Charc y)) = x==y
embedsSameHead (Lit (Floatc x)) (Lit (Floatc y)) = x==y
embedsSameHead (Lit (Intc x)) (Lit (Intc y)) = embeds (int2Expr x) (int2Expr y)
embedsSameHead (Comb c1 f1 e1) (Comb c2 f2 e2) =
  if c1==c2 && f1==f2 
     then embedsL e1 e2
     else False
embedsSameHead (Apply e1 e2) (Apply f1 f2) = embedsL [e1,e2] [f1,f2]
embedsSameHead (Constr _ e1) (Constr _ e2) = embeds e1 e2
embedsSameHead (Or e1 e2) (Or f1 f2) = embedsL [e1,e2] [f1,f2]
embedsSameHead (Case c1 e1 ces1) (Case c2 e2 ces2) =
  if c1==c2 
     then (embeds e1 e2) && (embedsCase ces1 ces2)
     else False
embedsSameHead (GuardedExpr _ c1 e1) (GuardedExpr _ c2 e2) =
  embedsL [c1,e1] [c2,e2]
embedsSameHead (Choice e1) (Choice e2) = embeds e1 e2

embedsL :: [Expr] -> [Expr] -> Bool
embedsL [] _ = True
embedsL (e:es) (f:fs) = (embeds e f) && (embedsL es fs)

embedsCase :: [BranchExpr] -> [BranchExpr] -> Bool
embedsCase [] _ = True
embedsCase (Branch _ e1 : ces1) (Branch _ e2 : ces2) =
  (embeds e1 e2) && (embedsCase ces1 ces2)

embedsArg :: Expr -> Expr -> Bool
embedsArg (Var _) _ = False
embedsArg (Lit _) _ = False
embedsArg (Comb _ _ args) e = embedsArgL args e
embedsArg (Apply e1 e2) e = embedsArgL [e1,e2] e
embedsArg (Constr _ c) e = embeds c e
embedsArg (Or e1 e2) e = embedsArgL [e1,e2] e
embedsArg (Case _ c ces) e = embeds c e || embedsArgCase ces e
embedsArg (GuardedExpr _ c r) e = embedsArgL [c,r] e
embedsArg (Choice c) e = embeds c e

embedsArgL :: [Expr] -> Expr -> Bool
embedsArgL [] _ = False
embedsArgL (e:es) s = 
  if embeds e s then True else embedsArgL es s

embedsArgCase :: [BranchExpr] -> Expr -> Bool
embedsArgCase [] _ = False
embedsArgCase (Branch _ e : ces) s =
  if embeds e s then True else embedsArgCase ces s

--translates integers to expressions (to apply standard embedding)
int2Expr :: Int -> Expr
int2Expr x =
  if x < 10
     then Comb ConsCall (show x) []
     else int2ExprAux x

int2ExprAux :: Int -> Expr
int2ExprAux x = Comb ConsCall ":" [a,b]
  where a = Comb ConsCall (show (x `div` 10)) []
        b = int2Expr (x `mod` 10)

------------------------------------------------------------------------------
-- msg stuff

--msgT returns w U ran(theta) U ran(sigma)
--  where w = msg(e1,e2), e1 = theta(w), and e2 = sigma(w)
msgT :: Expr -> Expr -> [Expr]
msgT e f = (w:(subTerms sigma))++(subTerms theta)
  where (w,sigma,theta) = computeMsg e f

computeMsg :: Expr -> Expr -> (Expr,Subst,Subst)
computeMsg e1 e2 = (w, sub1, sub2)
  where (w,_) = msg e1 e2 (max (maxVarIndex e1) (maxVarIndex e2))
        sub1 = instanceOf e1 w
        sub2 = instanceOf e2 w

-- msg e1 e2 i = (w,i')
-- w is the msg of e1 and e2 and i' is the last number used in the 
-- generation of fresh variables during the computation of w 

msg :: Expr -> Expr -> Int -> (Expr,Int)
msg x y i =
  if x==y then (x, i)
     else if headExpr x == headExpr y
             then msgSameHead x y i
             else (Var (i+1), i+1)

headExpr :: Expr -> String
headExpr (Var _) = "Var"
headExpr (Lit _) = "Lit"
headExpr (Comb _ f _) = f
headExpr (Case Rigid _ _) = "Rigid"
headExpr (Case Flex _ _) = "Flex"
headExpr (Constr _ _) = "Constr"
headExpr (Choice _) = "Choice"
headExpr (Or _ _) = "Or"
headExpr (GuardedExpr _ _ _) = "GuardedExpr"
headExpr (Apply _ _) = "Apply"

msgSameHead :: Expr -> Expr -> Int -> (Expr, Int)

msgSameHead (Var x) (Var y) i =
  if x==y then (Var x, i)
          else (Var (i+1), i+1)
msgSameHead (Lit x) (Lit y) i =
  if x==y then (Lit x, i)
          else (Var (i+1), i+1)
msgSameHead (Comb c1 f1 e1) (Comb c2 f2 e2) i =
  if c1==c2 && f1==f2 then msgArgs c1 f1 e1 e2 [] i
                      else (Var (i+1), i+1)
msgSameHead (Case c1 e1 ces1) (Case c2 e2 ces2) i
  | c1==c2 && e1==e2 && ces1==ces2  = (Case c1 e1 ces1, i)
  | otherwise                       = (Var (i+1), i+1)
msgSameHead (Constr v1 e1) (Constr v2 e2) i =
  if v1==v2 && e1==e2 then (Constr v1 e1, i)
                      else (Var (i+1), i+1) --identical or generalize!
msgSameHead (Choice e1) (Choice e2) i =
  if e1==e2 then (Choice e1, i)
            else (Var (i+1), i+1) --identical or generalize!
msgSameHead (GuardedExpr v1 c1 e1) (GuardedExpr v2 c2 e2) i =
  if v1==v2 && c1==c2 && e1==e2 then (GuardedExpr v1 c1 e1, i) 
                                else (Var (i+1), i+1) --ident or gen!
msgSameHead (Or e1 e2) (Or f1 f2) i =
  if e1==f1 && e2==f2 then (Or e1 e2, i)
                      else (Var (i+1), i+1) --identical or generalize!
msgSameHead (Apply e1 e2) (Apply f1 f2) i =
  if e1==f1 && e2==f2 then (Apply e1 e2, i)
                      else (Var (i+1), i+1) --identical or generalize!

msgArgs :: CombType -> String -> [Expr] -> [Expr] -> [Expr] -> Int -> (Expr,Int)
msgArgs ctype f [] [] es i = (Comb ctype f es, i)
msgArgs ctype f (a:as) (b:bs) es i = msgArgs ctype f as bs (es++[ab]) j
  where (ab, j) = msg a b i

-- end msg stuff

--given a term and a set, returns (Var -1) if there is no comparable (ie, with
--the same root symbol) term or the first comparable term and the remaining
--elements of the set:
comparable :: Expr -> [Expr] -> (Expr,[Expr])
comparable e es = comp e [] es

comp :: Expr -> [Expr] -> [Expr] -> (Expr,[Expr])
comp _ es [] = (Var (-1),es)
comp e es (h:t) =
  if headExpr e == headExpr h
     then (h, es++t)
     else comp e (es++[h]) t

------------------------------------------------------------------------------
-- Renaming stuff
------------------------------------------------------------------------------

-- auxiliar renaming function: ren_rho
-- rename of the RHS according to the independent renaming rho
-- (it should mimic the behaviour of the current abstraction operator..)

ren_rho  :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> Expr
ren_rho _ _ (Var x) = Var x
ren_rho _ _ (Lit x)  = Lit x
ren_rho funs rho (Comb FuncCall c e)
  | pevalAbs==0  = ren_rhoFun funs rho (Comb FuncCall c e)
  | pevalAbs==1  = if definedFunc2 funs c
                      then ren_rhoFunRec funs rho (Comb FuncCall c e)
                      else Comb FuncCall c (map (ren_rho funs rho) e)
  | pevalAbs==2  = if definedFunc2 funs c
                      then ren_rhoFunRec funs rho (Comb FuncCall c e)
                      else Comb FuncCall c (map (ren_rho funs rho) e)
ren_rho funs rho (Comb ConsCall c e) = Comb ConsCall c (map (ren_rho funs rho) e)
ren_rho funs rho (Comb PartCall c e) = 
  if definedFunc2 funs c
     then ren_rhoPartCall funs rho c e
     else Comb PartCall c (map (ren_rho funs rho) e)
ren_rho funs rho (Apply e1 e2) = Apply (ren_rho funs rho e1) (ren_rho funs rho e2)
ren_rho funs rho (Constr vars e) = Constr vars (ren_rho funs rho e) 
ren_rho funs rho (Case ctype e cases) = 
  Case ctype (ren_rho funs rho e) (map (ren_case funs rho) cases)
ren_rho funs rho (Or e1 e2) = Or (ren_rho funs rho e1) (ren_rho funs rho e2)
ren_rho funs rho (GuardedExpr vars gc e) = 
  GuardedExpr vars (ren_rho funs rho gc) (ren_rho funs rho e)
ren_rho funs rho (Choice e) = Choice (ren_rho funs rho e) 
ren_rho funs rho (SQ e)
  | pevalAbs==0  = ren_rhoSQ funs rho e
  | pevalAbs==1  = ren_rhoFunRec funs rho e
  | pevalAbs==2  = ren_rhoFunRec funs rho e

ren_rhoPartCall :: [FuncDecl] -> [(Expr,Expr)] -> String -> [Expr] -> Expr
ren_rhoPartCall funs rho f e = Comb PartCall fr (take (length e) es)
  where args = complete e (getArityF funs f) (maxVarIndex (Comb PartCall f e))
        Comb FuncCall fr es = ren_rho funs rho (Comb FuncCall f args)

ren_case :: [FuncDecl] -> [(Expr,Expr)] -> BranchExpr -> BranchExpr
ren_case funs rho (Branch p e) = Branch p (ren_rho funs rho e) 

ren_rhoSQ :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> Expr
ren_rhoSQ funs rho e = substitute vars exps er
  where ((Sub vars exps),er) = searchConstrInstance funs e rho

ren_rhoFun :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> Expr
ren_rhoFun funs rho (Comb FuncCall f es) =
  if constructorInstance funs (Comb FuncCall f es) (map fst rho)
     then ren_rhoSQ funs rho (Comb FuncCall f es)
     else if (definedFunc2 funs f) -- || (legalBuiltin f)
             then ren_rhoSQ funs rho (Comb FuncCall f es)
             else Comb FuncCall f (map (ren_rho funs rho) es)

ren_rhoFunRec :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> Expr
ren_rhoFunRec funs rho e = 
  if sub==FSub
     then ren_rhoRec funs rho e
     else substitute (subVars sub) (subTerms sub) exp
  where (sub, exp) = searchConstrInstance funs e rho

ren_rhoRec :: [FuncDecl] -> [(Expr,Expr)] -> Expr -> Expr
ren_rhoRec funs rho e = 
  if subs==FSub
     then ren_rho funs rho e
     else substitute (subVars subs) (map (ren_rho funs rho) (subTerms subs)) e2
  where (subs, e2) = searchInstance e rho
--ren_rhoRec funs rho e = ren_rho funs rho e

--extract vars and terms of a given (nonfailed) substitution
subVars :: Subst -> [Int]
subVars (Sub v _) = v
subVars FSub = trace "\n\n ERROR!!!!\n\n" [-2]
subTerms :: Subst -> [Expr]
subTerms (Sub _ e) = e
subTerms FSub = trace "\n\n ERROR!!!\n\n" [Var (-2)]

--check whether an expression is a variable:
isVar :: Expr -> Bool
isVar e = case e of
            (Var _) -> True
            _       -> False

--check whether an expression is a literal:
isLit :: Expr -> Bool
isLit e = case e of
            (Lit _) -> True
            _       -> False

--check whether an expression is rooted by Case:
isCase :: Expr -> Bool
isCase e = case e of
             (Case _ _ _) -> True
             _            -> False

isConstr :: Expr -> Bool
isConstr e = case e of
              (Var _) -> True
              (Lit _) -> True
              (Comb ConsCall _ es) -> andL (map isConstr es)
              _       -> False

-- independent renaming of a set of terms e
-- return tuples (e,e') where e' is the independent renaming of e

build_ren :: [Expr] -> Int -> [(Expr,Expr)]
build_ren [] _ = []
build_ren ((Comb ftype fname args):es) n
  | legalBuiltin fname  = ((Comb ftype fname args),
                           (Comb FuncCall ((nameConversion fname) ++ "_pe" ++ (show n)) (index2vars vars)))
                          : (build_ren es (n+1))
  | otherwise           = ((Comb ftype fname args),
                           (Comb FuncCall (fname ++ "_pe" ++ (show n)) (index2vars vars)))
                          : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (Comb ftype fname args)) 
build_ren ((Case ctype e cases):es) n = 
   ((Case ctype e cases),
    (Comb FuncCall ("case_pe" ++ (show n)) (index2vars vars)))
   : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (Case ctype e cases) )

build_ren ((Apply e1 e2) : es) n =
   ((Apply e1 e2),
    (Comb FuncCall ("apply_pe" ++ (show n)) (index2vars vars)))
   : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (Apply e1 e2)) 

build_ren ((Constr v c) : es) n =
   ((Constr v c),
    (Comb FuncCall ("constr_pe" ++ (show n)) (index2vars vars)))
   : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (Constr v c)) 

build_ren ((Or e1 e2) : es) n =
   ((Or e1 e2),
    (Comb FuncCall ("or_pe" ++ (show n)) (index2vars vars)))
   : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (Or e1 e2)) 

build_ren ((GuardedExpr v c e) : es) n =
   ((GuardedExpr v c e),
    (Comb FuncCall ("guarded_pe" ++ (show n)) (index2vars vars)))
   : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (GuardedExpr v c e)) 

build_ren ((Choice e) : es) n =
   ((Choice e),
    (Comb FuncCall ("choice_pe" ++ (show n)) (index2vars vars)))
   : (build_ren es (n+1))
 where vars = nub (nonLocalVarsInExp (Choice e)) 

-- return the non-local variables of a case expression
--varsInExpCase (Case _ e cases) = varsInExp e ++ 
--                 concat (map varsNonLocal cases)


-- return the non-local variables of an expression

nonLocalVarsInExp :: Expr -> [Int]
nonLocalVarsInExp (Var v) = [v]
nonLocalVarsInExp (Lit _) = []
nonLocalVarsInExp (Comb _ _ exps) = concat (map nonLocalVarsInExp exps)
nonLocalVarsInExp (Apply e1 e2) = nonLocalVarsInExp e1 ++ nonLocalVarsInExp e2
nonLocalVarsInExp (Constr vars e) = delArgs vars (nonLocalVarsInExp e)
nonLocalVarsInExp (Or e1 e2) = nonLocalVarsInExp e1 ++ nonLocalVarsInExp e2
nonLocalVarsInExp (Case _ e cases) = nonLocalVarsInExp e ++ concat (map varsNonLocal cases)
nonLocalVarsInExp (GuardedExpr vars c e) = delArgs vars (nonLocalVarsInExp c ++ nonLocalVarsInExp e)
nonLocalVarsInExp (Choice e) = nonLocalVarsInExp e
nonLocalVarsInExp (SQ e) = nonLocalVarsInExp e

-- return the non-local variables of the different scrutinees of a case expression
varsNonLocal :: BranchExpr -> [Int]
varsNonLocal (Branch (Pattern _ as) e) = delArgs as (nonLocalVarsInExp e) 
varsNonLocal (Branch (LPattern _) e)   = nonLocalVarsInExp e

--varsInCase (Branch (Pattern _ as) e) = as ++ varsInExp e


-- remove es1 es2: remove all the elements which appear in es1 from es2
--removeElements [] y = y
--removeElements (_:_) [] = []
--removeElements (x:xs) ys = if x `elem` ys then removeElements xs (remE x ys)
--                                          else removeElements xs ys
                                            
--remE _ [] = []
--remE x (y:ys) = if x == y then remE x ys
--                          else y:(remE x ys)


-- transform variable indices into variable expressions and vice versa:
index2vars :: [VarIndex] -> [Expr]
index2vars = map (\i->Var i)

vars2index :: [Expr] -> [VarIndex]
vars2index = map (\(Var i)->i)

----------------------------------------------------------------------------
-- END Renaming stuff
----------------------------------------------------------------------------

   
----------------------------------------------------------------------------
-- Local Control (unfolding, i.e., a non-standard metainterpreter for Curry)
----------------------------------------------------------------------------

-- local criteria to proceed unfolding a call at a given state:

--proceedUnfold pst e = getDepth pst < 10 && not (existsRen (getExps pst) e)
--proceedUnfold pst e = (getDepth pst < 10) && (not (e `elem` (getExps pst)))
--
-- the above unfolding rules do NOT work (since the current expressions
-- are also in the set of partially evaluated expressions... I guess..)

proceedUnfold :: PEvalState -> _ -> Bool
proceedUnfold pst _ = getDepth pst < 1

--peval implements our non-standard metainterpreter..

peval :: PEvalState -> Int -> Expr -> Expr

----------------------------------------------------------------------------
--RLNT calculus:
--
-- HNF, Case of Case, Case Function, Case Select, Case Guess, Function Eval
----------------------------------------------------------------------------

---------------------------------------------------------------------------
--Terms within square brackets:

--peval pst n (SQ e) = peval pst n e  --is this really needed??

----------------------------------------------------------------------------
-- HNF:

peval _ _ (Var v) = (Var v)
peval _ _ (Lit l) = (Lit l)
peval pst n (Comb ConsCall c es) = (Comb ConsCall c (map (peval pst n) es))

----------------------------------------------------------------------------
-- Case of Case:
-- (this rule only applies when the argument e is a variable, otherwise,
--  it proceeds as in the original Case Eval rule of the LNT calculus)
--
peval pst n (Case ctypej (Case ctypek e cesk) cesj) =
  if (isVar e) 
    then peval pst n (Case ctypek e (map gen_subcase cesk))
    else try_eval pst n (Case ctypej (Case ctypek e cesk) cesj) [Case ctypek e cesk] [1]
 where
   gen_subcase (Branch pat ek) = Branch pat (Case ctypej ek cesj)

------------------------------------------------------------------------------
-- Case function (see below)
peval pst n (Case ctype (Comb FuncCall f es) ces) =
  if (definedFunc pst f)
     then peval_casefun pst n ctype f es ces
     else peval_casefun_builtin pst n ctype f es ces

------------------------------------------------------------------------------
-- Case Select:

peval pst n (Case _ (Comb ConsCall f es) ces) = peval pst n (matchBranch ces)
 where
  -- match a branch (for Case Select):
  matchBranch [] = (Comb FuncCall "failed" [])
  matchBranch (Branch (Pattern p vars) e : bs) =
    if p==f then substitute vars es e else matchBranch bs

--this is for Case's with a constant argument:
peval _ _ (Case _ (Lit l) ces) = matchBranchLit ces
 where
  -- match a branch with Literals:
  matchBranchLit [] = (Comb FuncCall "failed" [])
  matchBranchLit (Branch (LPattern p) e : bs) =
    if p==l then e else matchBranchLit bs

------------------------------------------------------------------------------
-- Case Guess:

peval pst n (Case ctype (Var v) ces) =
     Case ctype (Var v) (map peval_branch (propBinding v ces))
 where
  -- proceed partial evaluation of case branches:
  peval_branch (Branch p e) = Branch p (peval pst n e)

---------------------------------------------------------------------------
-- Function Eval:

peval pst n (Comb FuncCall f es) = 
  if definedFunc pst f
        then peval_fun pst n f es
        else peval_builtin pst n f es

----------------------------------------------------------------------------
--Partcall:

peval pst n (Comb PartCall c es) = Comb PartCall c (map (peval pst n) es)

----------------------------------------------------------------------------
--Or expression:

peval pst n (Or e1 e2) = Or (peval pst n e1) (peval pst n e2)

----------------------------------------------------------------------------
--Choice:

peval _ _ (Choice e) = Choice e

----------------------------------------------------------------------------
--These rules complements Case-of-case, Case Select, Case Fun, and Case Guess:
--
peval pst n (Case ctype (Apply e1 e2) ces) = pevalCaseArg pst n (Apply e1 e2) ctype ces
peval pst n (Case ctype (Constr vars es) ces) = pevalCaseArg pst n (Constr vars es) ctype ces
peval pst n (Case ctype (Or e1 e2) ces) = pevalCaseArg pst n (Or e1 e2) ctype ces
peval pst n (Case ctype (GuardedExpr vars gc e) ces) = 
  pevalCaseArg pst n (GuardedExpr vars gc e) ctype ces
peval _ _ (Case ctype (Choice e) ces) = Case ctype (Choice e) ces
peval pst n (Case ctype (Comb PartCall f es) ces) = 
  pevalCaseArg pst n (Comb PartCall f es) ctype ces
peval pst n (Case ctype (SQ e) ces) = peval pst n (Case ctype e ces)

----------------------------------------------------------------------------
--Apply:

peval pst n (Apply e1 e2) = 
  if (isPartCall e1) then peval_apply pst n e1 e2
                     else try_eval pst n (Apply e1 e2) [e1,e2] [1]

-----------------------------------------------------------------------------
--Guarded expressions:

peval pst n (GuardedExpr vars gc e) =
  if gc2==(Comb FuncCall "success" []) || gc2==(Comb ConsCall "True" [])
     then peval pst n e
     else try_eval pst n (GuardedExpr vars gc e) [gc,e] [1]
  where gc2 = delSQ gc

-----------------------------------------------------------------------------
--Constraints:

peval pst n (Constr vars e)
  | e2==(Comb FuncCall "success" [])  = Comb FuncCall "success" []
  | e2==(Comb ConsCall "True" [])     = Comb FuncCall "success" []  --is it correct??
  | otherwise                         = try_eval pst n (Constr vars e) [e] [1]
  where e2 = delSQ e

----------------------------------------------------------------------------
-- For Case Function
 
peval_casefun :: PEvalState -> Int -> CaseType -> String -> [Expr] -> [BranchExpr] -> Expr
peval_casefun pst n ctype f es ces = 
  if proceedUnfold pst (Comb FuncCall f es)
     then peval_casefun_aux (incrDepth pst) n ctype f es ces  -- Case Fun
     else SQ (Case ctype (Comb FuncCall f es) ces)            -- stop unfolding

peval_casefun_aux :: PEvalState -> Int -> CaseType -> String -> [Expr] -> [BranchExpr] -> Expr
peval_casefun_aux pst n ctype f es ces = 
    peval newpst n (Case ctype newexp ces)
  where (newpst,newexp) = matchRHS pst n f es  --call unfolding

peval_casefun_builtin :: PEvalState -> Int -> CaseType -> String -> [Expr] -> [BranchExpr] -> Expr
peval_casefun_builtin pst n ctype f es ces = 
  if (Comb FuncCall f es)==pe 
     then Case ctype (Comb FuncCall f es) ces
     else if isCaseVar pe2 || isCaseCons pe2
             then peval pst n (Case ctype pe2 ces)
             else SQ (Case ctype pe2 ces) --changed! (before peval)
  where pe = peval_builtin pst n f es
        pe2 = delSQ pe

------------------------------------------------------------------------------
-- For Case Guess:
--propagate matchings to the corresponding branches:
propBinding :: Int -> [BranchExpr] -> [BranchExpr]

propBinding _ [] = []
propBinding v (Branch (Pattern c vs) e : es) = 
   (Branch (Pattern c vs) 
           (substitute [v] [(Comb ConsCall c (index2vars vs))] e)) : propBinding v es
propBinding v (Branch (LPattern (Intc p)) e : es) = 
   (Branch (LPattern (Intc p))
           (substitute [v] [(Lit (Intc p))] e)) : propBinding v es
propBinding v (Branch (LPattern (Floatc p)) e : es) = 
   (Branch (LPattern (Floatc p))
           (substitute [v] [(Lit (Floatc p))] e)) : propBinding v es
propBinding v (Branch (LPattern (Charc p)) e : es) = 
   (Branch (LPattern (Charc p))
           (substitute [v] [(Lit (Charc p))] e)) : propBinding v es

---------------------------------------------------------------------------
-- For Function Eval:

peval_fun :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_fun pst n c es = 
  if proceedUnfold pst (Comb FuncCall c es)
        then peval_fun_aux (incrDepth pst) n c es
        else SQ (Comb FuncCall c es)

peval_fun_aux :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_fun_aux pst n f es = peval newpst n newexp
  where (newpst,newexp) = matchRHS pst n f es

---------------------------------------------------------------------------
-- CALL UNFOLDING:

-- match a right-hand side of a given function:
matchRHS :: PEvalState -> Int -> String -> [Expr] -> (PEvalState,Expr)
matchRHS pst n f es = getMatchedRHS pst n (getFuncDecls pst) f es

getMatchedRHS :: PEvalState -> Int -> [FuncDecl] -> String -> [Expr] -> (PEvalState,Expr)
getMatchedRHS pst n (Func fname _ _ funrule : fds) name es =
   if fname==name then getMatchedRHS_aux pst n funrule es
                  else getMatchedRHS pst n fds name es

getMatchedRHS_aux :: PEvalState -> Int -> Rule -> [Expr] -> (PEvalState,Expr)
getMatchedRHS_aux pst n (Rule vars rhs) es =
  let curr_ri = getRenamingIndex pst
      maxindex = max (maxList vars) (maxVarIndex rhs)
  in
   (updateRenamingIndex pst (curr_ri+maxindex+1),
    substituteAll vars es (curr_ri+maxExpVars*n) rhs)
   -- here we assume that no more than maxExpVars variables occur in each
   -- unfolded expression to produce a safe renaming.

-- (substitute vars exps expr) = expr[vars/exps]
-- i.e., replace all occurrences of vars by corresponding exps in the
-- expression expr
substitute :: [Int] -> [Expr] -> Expr -> Expr
substitute vars exps expr = substituteAll vars exps 0 expr

-- (substituteAll vars exps base expr):
-- substitute all occurrences of variables by corresponding expressions:
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
substituteAll vs es b (Apply e1 e2) =
                 Apply (substituteAll vs es b e1) (substituteAll vs es b e2)
substituteAll vs es b (Constr vars e) =
                 Constr (map (+b) vars) (substituteAll vs es b e)
substituteAll vs es b (Or e1 e2) =
                 Or (substituteAll vs es b e1) (substituteAll vs es b e2)
substituteAll vs es b (Case ctype e cases) =
   Case ctype (substituteAll vs es b e) (map (substituteAllCase vs es b) cases)
substituteAll vs es b (GuardedExpr vars c e) =
   GuardedExpr (map (+b) vars) (substituteAll vs es b c)
                               (substituteAll vs es b e)
substituteAll vs es b (Choice e) = Choice (substituteAll vs es b e)
substituteAll vs es b (SQ e) = SQ (substituteAll vs es b e)

substituteAllCase :: [Int] -> [Expr] -> Int -> BranchExpr -> BranchExpr
substituteAllCase vs es b (Branch (Pattern l pvs) e) =
                 Branch (Pattern l (map (+b) pvs)) (substituteAll vs es b e)
substituteAllCase vs es b (Branch (LPattern l) e) =
                 Branch (LPattern l) (substituteAll vs es b e)

----------------------------------------------------------------------------
--Extension of the RLNT calculus:
--
-- partcall, apply, constr, or, guardedexpr, choice
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--try_eval:
-- (see comments in try_eval_aux)

try_eval :: PEvalState -> Int -> Expr -> [Expr] -> [Int] -> Expr
try_eval pst n exp es i = 
  if exp==new 
     then exp 
     else if sharedVars new || groundVars new || condSQ new
             then SQ (delSQ new) --we move SQ's to the outermost position if there are shared vars
             else new
 where
  new = try_eval2 pst n exp es i 

try_eval2 :: PEvalState -> Int -> Expr -> [Expr] -> [Int] -> Expr
try_eval2 pst n exp es i =
   if (caseVarArg es i 1) then float_case exp [] es i 1
                          else try_eval_aux pst n exp [] es --i 1

caseVarArg :: [Expr] -> [Int] -> Int -> Bool
caseVarArg [] _ _ = False
caseVarArg (e:es) i j = if (j `elem` i) && (caseVar e) then True else caseVarArg es i (j+1)

caseVar :: Expr -> Bool
caseVar e = case e of
             --ATTENTION: only Flex case's are floated out!!
             (Case Flex (Var _) _) -> True
             _                     -> False

float_case :: Expr -> [Expr] -> [Expr] -> [Int] -> Int -> Expr
float_case exp les (e:es) i j = 
  if (j `elem` i) && (caseVar e) 
      then float_caseAux exp les e es 
      else float_case exp (les++[e]) es i (j+1)

float_caseAux :: Expr -> [Expr] -> Expr -> [Expr] -> Expr
float_caseAux exp les (Case ctype (Var x) ces) es =
   Case ctype (Var x) (map (gen_case exp les es) ces)

gen_case :: Expr -> [Expr] -> [Expr] -> BranchExpr -> BranchExpr
gen_case (Comb ftype f _) les es (Branch pat ek) = Branch pat (Comb ftype f (les++(ek:es)))
gen_case (Apply _ _) les es (Branch pat ek) = Branch pat (Apply e1 e2)
    where [e1,e2] = les++(ek:es)
gen_case (Case ctype _ ces) _ _ (Branch pat ek) = Branch pat (Case ctype ek ces)
gen_case (GuardedExpr var _ e) _ _ (Branch pat ek) = Branch pat (GuardedExpr var ek e)
gen_case (Constr var _) _ _ (Branch pat ek) = Branch pat (Constr var ek)

--try_eval_aux :: PEvalState -> Int -> Expr -> [Expr] -> [Expr] -> [Int] -> Int -> Expr
try_eval_aux :: PEvalState -> Int -> Expr -> [Expr] -> [Expr] -> Expr
try_eval_aux _ _ exp les [] = try_eval_aux2 exp les
try_eval_aux pst n exp les (e:es) =
  if e==new
     then try_eval_aux pst n exp (les++[e]) es --i (j+1)
     else try_eval_aux2 exp (les++[new]++es)
  where new = peval pst n e
--  if (j `elem` i) --> everything can be unfolded!
--    then unfoldableArg pst n exp les (e:es) i j
--    else try_eval_aux pst n exp (les++[e]) es i (j+1)

--we use the above definition for try_eval since it is equivalent (but a bit more
--efficient) than the one below within a depth-1 strategy..
--in general, this sequential (left-to-right) strategy gives better results
--(exceptions: conjunctions, disjunctions,..)

--    then try_eval_aux pst n exp (les++[delSQ (peval (decrDepth pst) n e)]) es i (j+1)
--    else try_eval_aux pst n exp (les++[delSQ (peval pst n e)]) es i (j+1)
--note that above the depth is *only* decremented when the argument is strict.. 
-- *this doesn't guarantee termination..!
-- *but it terminates very often (on the other hand, by decrementing it for all 
-- *the arguments, we enter an infinite loop in many examples..)

--unfoldableArg :: PEvalState -> Int -> Expr -> [Expr] -> [Expr] -> [Int] -> Int -> Expr
--unfoldableArg pst n exp les (e:es) i j =
--  if (delSQ e)==new
--     then try_eval_aux pst n exp (les++[e]) es i (j+1)
--     else try_eval_aux2 exp (les++[delSQ (peval pst n e)]++es)
--  where new = delSQ (peval pst n e)

try_eval_aux2 :: Expr -> [Expr] -> Expr
try_eval_aux2 (Comb ftype f _) es = Comb ftype f es
try_eval_aux2 (Apply _ _) [e1,e2] = Apply e1 e2
try_eval_aux2 (Case ctype _ ces) [e] = Case ctype e ces
try_eval_aux2 (GuardedExpr var _ _) [gc,e] = GuardedExpr var gc e
try_eval_aux2 (Constr vars _) [e] = Constr vars e

sharedVars :: Expr -> Bool
sharedVars (Comb _ f es) = 
  if f=="if_then_else" 
     then False
     else sharedVarsAux (map varsInExp es) 
sharedVars (Apply _ _) = True
sharedVars (Case _ _ _) = True
sharedVars (GuardedExpr _ _ _) = True
sharedVars (Constr _ _) = True

sharedVarsAux :: [[Int]] -> Bool
sharedVarsAux [] = False
sharedVarsAux (e:es) =
  if shVar e (concat es)
     then True
     else sharedVarsAux es

shVar :: [Int] -> [Int] -> Bool
shVar [] _ = False
shVar (e:es) g =
  if e `elem` g
     then True
     else shVar es g

groundVars :: Expr -> Bool
groundVars (Var _) = False
groundVars (Lit _) = True
groundVars (Comb _ f es) = (concat (map varsInExp es))==[]
                           || (f=="&" && reducible es)
                           || (f=="&&" && reducible es)
--  | f=="&"    = orL (map groundVars es)   --in a conjunction we preserve SQ
--  | f=="&&"   = orL (map groundVars es)   --with one ground argument..
--  | otherwise = (concat (map varsInExp es))==[] 
groundVars (Apply _ _) = False
groundVars (Case _ _ _) = False
groundVars (GuardedExpr _ _ _) = False
groundVars (Constr _ _) = False
groundVars (Choice _) = False
groundVars (Or _ _) = False

reducible :: [Expr] -> Bool
reducible [e1,e2] = 
  e1==(Comb FuncCall "success" [])
  || e1==(Comb FuncCall "failed" [])
  || e2==(Comb FuncCall "success" [])
  || e2==(Comb FuncCall "failed" [])

condSQ :: Expr -> Bool
condSQ (Var _) = False
condSQ (Lit _) = False
condSQ (Comb _ f es) =
  (f=="if_then_else" && sqRooted (head es))
  || (f=="map" && sqRooted (head (tail es)))
  || (arithBuiltin f && sqRooted (head es))
  || (arithBuiltin f && sqRooted (head (tail es)))
  -- TO DO: complete the remaining cases..
condSQ (Apply e _) = sqRooted e
condSQ (Case _ e _) = sqRooted e
condSQ (GuardedExpr _ e _) = sqRooted e
condSQ (Constr _ e) = sqRooted e
condSQ (Choice _) = False
condSQ (Or _ _) = False

sqRooted :: Expr -> Bool
sqRooted e = case e of
               (SQ _) -> True
               _      -> False

----------------------------------------------------------------------------
-- For rules complementing Case-of-case, Case Select, Case Fun, and Case Guess:
--
pevalCaseArg :: PEvalState -> Int -> Expr -> CaseType -> [BranchExpr] -> Expr
pevalCaseArg pst n e ctype ces =
  if e==arg
     then Case ctype arg ces
     else SQ (Case ctype (delSQ arg) ces)
  where arg = peval pst n e
        --arg2 = delSQ arg

----------------------------------------------------------------------------
-- For Apply:

isPartCall :: Expr -> Bool
isPartCall e = case e of
                 (Comb PartCall _ _) -> True
                 _                   -> False

peval_apply :: PEvalState -> Int -> Expr -> Expr -> Expr
peval_apply pst n (Comb PartCall f es) e2 =
  if (getArity pst f)==(length es + 1)
  then peval pst n (Comb FuncCall f (es++[e2]))
  else peval pst n (Comb PartCall f (es++[e2]))

-----------------------------------------------------------------------------
--Built-in's:

peval_builtin :: PEvalState -> Int -> String -> [Expr] -> Expr

peval_builtin pst n f es 
  | f=="=="   = peval_builtinEQ pst n f es 
  | f=="=:="  = peval_builtinEQ pst n f es
  | f=="if_then_else" = peval_builtinITE pst n es
  | f=="&&"   = peval_builtinAND pst n es
  | f=="&"    = peval_builtinCAND pst n es
  | f=="*"    = peval_builtinARITH pst n f es
  | f=="+"    = peval_builtinARITH pst n f es
  | f=="-"    = peval_builtinARITH pst n f es
  | f=="<"    = peval_builtinARITH pst n f es
  | f==">"    = peval_builtinARITH pst n f es
  | f=="<="   = peval_builtinARITH pst n f es
  | f==">="   = peval_builtinARITH pst n f es
  | f=="map"  = peval_builtinMAP pst n f es
  | f=="foldl"   = peval_builtinFOLDL pst n f es
  | f=="foldr"   = peval_builtinFOLDR pst n f es
  | f=="filter"  = peval_builtinFILTER pst n f es
  | f=="iterate" = peval_builtinITERATE pst n f es
  | True        = Comb FuncCall f es
     
---------------------------------
-- STRICT EQUALITY:

peval_builtinEQ :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinEQ pst n f [e1,e2]
  | (isConsCall e1s) && (isConsCall e2s)  = peval_builtinEQaux pst n f [e1s,e2s]
  | (isLitInt e1s) && (isLitInt e2s)      = peval_builtinEQlit f e1s e2s
  | isVar e1s                             = peval_builtinEQvar1 pst n f [e1s,e2]
  | isVar e2s                             = peval_builtinEQvar2 pst n f [e2s,e1]
  | otherwise                             = try_eval pst n (Comb FuncCall f [e1,e2]) [e1,e2] [1,2]
  where e1s = delSQ e1
        e2s = delSQ e2

peval_builtinEQaux :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinEQaux pst n f [Comb _ f1 e1, Comb _ f2 e2]
  | f1==f2                                      = peval pst n (makeSeqCon f e1 e2)
  | (f1=="False") && (f2=="failed") && f=="=="  = Comb ConsCall "True" []
  | (f1=="False") && (f2=="failed") && f=="=:=" = Comb FuncCall "success" []
  | (f1=="failed") && (f2=="False") && f=="=="  = Comb ConsCall "True" []
  | (f1=="failed") && (f2=="False") && f=="=:=" = Comb FuncCall "success" []
  | f=="=="                                     = Comb ConsCall "False" []
  | otherwise                                   = Comb FuncCall "failed" []
--the strange cases above are needed due to the current definition of Case Select
--(in particular, the case when the argument doesn't match any branch..)

peval_builtinEQlit :: String -> Expr -> Expr -> Expr
peval_builtinEQlit f (Lit (Intc x)) (Lit (Intc y))
  | x==y && f=="=="   = Comb ConsCall "True" []
  | x==y && f=="=:="  = Comb FuncCall "success" []
  | x/=y && f=="=="   = Comb ConsCall "False" []
  | otherwise         = Comb FuncCall "failed" []

peval_builtinEQvar1 :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinEQvar1 pst n f [Var x, e] =
  if dataExp (delSQ e) && f=="=:="
     then peval_builtinEQvarAux pst n f x (delSQ e)
     else try_eval pst n (Comb FuncCall f [Var x, e]) [Var x, e] [1,2]

peval_builtinEQvar2 :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinEQvar2 pst n f [Var x, e] =
  if dataExp (delSQ e) && f=="=:="
     then peval_builtinEQvarAux pst n f x (delSQ e)
     else try_eval pst n (Comb FuncCall f [e, Var x]) [e, Var x] [1,2]

peval_builtinEQvarAux :: PEvalState -> Int -> String -> Int -> Expr -> Expr
peval_builtinEQvarAux pst n f x e =
  peval pst n (Case Flex (Var x) (subs2branches e f))
--  if f=="=="
--     then peval pst n (Case Rigid (Var x) (subs2branches  e f))
--     else peval pst n (Case Flex (Var x) (subs2branches e f))

subs2branches :: Expr -> String -> [BranchExpr]
subs2branches (Lit c) f = 
  if f=="==" 
     then [Branch (LPattern c) (Comb ConsCall "True" [])]
     else [Branch (LPattern c) (Comb FuncCall "success" [])]
subs2branches (Comb ConsCall c []) f =
  if f=="==" 
     then [Branch (Pattern c []) (Comb ConsCall "True" [])]
     else [Branch (Pattern c []) (Comb FuncCall "success" [])]
--PENDING: extend function above to cover arbitrary data terms...

--currently we only consider literals and constants.. TO DO..
dataExp :: Expr -> Bool
dataExp (Var _) = False
dataExp (Lit _) = True
dataExp (Comb ftype f args) = 
  args==[] && 
          (ftype==ConsCall || f=="success" || f=="failed")
-- if ftype==ConsCall then andL (map dataExp args) else False
dataExp (Apply _ _) = False
dataExp (Constr _ _) = False
dataExp (Or _ _) = False
dataExp (Case _ _ _) = False
dataExp (GuardedExpr _ _ _) = False
dataExp (Choice _) = False
--dataExp (SQ e) = dataExp e


isConsCall :: Expr -> Bool
isConsCall e = case e of
                 (Comb ConsCall _ _)  -> True
                 (Comb _ "success" _) -> True
                 (Comb _ "failed" _)  -> True
                 _                    -> False

makeSeqCon :: String -> [Expr] -> [Expr] -> Expr
makeSeqCon f [] [] = 
  if f=="==" then Comb ConsCall "True" []
             else Comb FuncCall "success" []
makeSeqCon f [e1] [e2] = Comb FuncCall f [e1,e2]
makeSeqCon f (e1:(e1b:es1)) (e2:(e2b:es2)) =
  if f=="==" 
     then (Comb FuncCall "&&" [(Comb FuncCall f [e1,e2]), (makeSeqCon f (e1b:es1) (e2b:es2))])
     else (Comb FuncCall "&" [(Comb FuncCall f [e1,e2]), (makeSeqCon f (e1b:es1) (e2b:es2))])

-------------------------------------------------------------------
-- IF THEN ELSE:

peval_builtinITE :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinITE pst n [c,e1,e2]
  | cond==(Comb ConsCall "True" [])   = e1
  | cond==(Comb ConsCall "False" [])  = e2
  | otherwise                         = try_eval pst n (Comb FuncCall "if_then_else" [c,e1,e2]) 
                                        [cond,e1,e2] [1]
  where cond = (delSQ c)

-------------------------------------------------------------------
-- SEQUENTIAL CONJUNCTION:

peval_builtinAND :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinAND pst n [e1,e2] 
  | e1Aux==(Comb ConsCall "True" [])    =  peval pst n e2
  | e1Aux==(Comb FuncCall "success" []) =  peval pst n e2
  | e2Aux==(Comb ConsCall "True" [])    =  peval pst n e1
  | e2Aux==(Comb FuncCall "success" []) =  peval pst n e1
  | e1Aux==(Comb ConsCall "False" [])   =  Comb ConsCall "False" []
  | e1Aux==(Comb FuncCall "failed" [])  =  Comb FuncCall "failed" []
  | e2Aux==(Comb ConsCall "False" [])   =  Comb ConsCall "False" []
  | e2Aux==(Comb FuncCall "failed" [])  =  Comb FuncCall "failed" []
  | otherwise                           =  try_eval pst n (Comb FuncCall "&&" [e1,e2]) [e1,e2] [1,2]
  where e1Aux = delSQ e1
        e2Aux = delSQ e2
  
--------------------------------------------------------------------
-- CONCURRENT CONJUNCTION:

peval_builtinCAND :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinCAND pst n [e1,e2] 
  | e1s==(Comb FuncCall "success" [])  = peval pst n e2
  | e2s==(Comb FuncCall "success" [])  = peval pst n e1
  | e1s==(Comb FuncCall "failed" [])   = Comb FuncCall "failed" []
  | e2s==(Comb FuncCall "failed" [])   = Comb FuncCall "failed" []
  | otherwise                          = try_eval pst n (Comb FuncCall "&" [e1,e2]) [e1,e2] [1,2]
  where e1s = delSQ e1
        e2s = delSQ e2

-----------------------------------------------------------------------
-- ARITHMETIC AND RELATIONAL OPERATORS:

peval_builtinARITH :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinARITH pst n f [e1,e2] =
  if (isLitInt e1) && (isLitInt e2)
     then peval_builtinARITHaux f [e1,e2]
     else try_eval pst n (Comb FuncCall f [e1,e2]) [e1,e2] [1,2]

--PENDING: extend to floats and to more operators..

peval_builtinARITHaux :: String -> [Expr] -> Expr
peval_builtinARITHaux f [(Lit (Intc e1)),(Lit (Intc e2))] 
  | (f=="*")   = Lit (Intc (e1*e2))
  | (f=="+")   = Lit (Intc (e1+e2))
  | (f=="-")   = Lit (Intc (e1-e2))
  | (f=="<")   = if e1<e2 then (Comb ConsCall "True" []) else (Comb ConsCall "False" [])
  | (f==">")   = if e1>e2 then (Comb ConsCall "True" []) else (Comb ConsCall "False" [])
  | (f=="<=")  = if e1<=e2 then (Comb ConsCall "True" []) else (Comb ConsCall "False" [])
  | (f==">=")  = if e1>=e2 then (Comb ConsCall "True" []) else (Comb ConsCall "False" [])

isLitInt :: Expr -> Bool
isLitInt e = case e of
               (Lit (Intc _)) -> True
               _              -> False

------------------------------------------------------------------------------
--HIGHER-ORDER FUNCTIONS (defined in the prelude): we just perform a function unfolding

-- function map:

peval_builtinMAP :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinMAP pst n f es =
  if proceedUnfold pst (Comb FuncCall f es)
        then peval_builtinMAP_aux (incrDepth pst) n es
        else SQ (Comb FuncCall f es)

peval_builtinMAP_aux :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinMAP_aux pst n [func,xs] = 
  let curr_ri = getRenamingIndex pst
      var1    = curr_ri+maxExpVars*n+1
      var2    = curr_ri+maxExpVars*n+2
      newpst  = updateRenamingIndex pst (curr_ri+3)
  in
    peval newpst n (Case Rigid xs [Branch (Pattern "[]" []) (Comb ConsCall "[]" []),
                                   Branch (Pattern ":" [var1,var2]) 
                                     (Comb ConsCall ":" 
                                       [Apply func (Var var1),
                                        Comb FuncCall "map" [func, Var var2]  
                                       ]
                                     )
                                   ]
                    )

-- function foldl:

peval_builtinFOLDL :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinFOLDL pst n f es =
  if proceedUnfold pst (Comb FuncCall f es)
        then peval_builtinFOLDL_aux (incrDepth pst) n es
        else SQ (Comb FuncCall f es)

peval_builtinFOLDL_aux :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinFOLDL_aux pst n [func,z,xs] = 
  let curr_ri = getRenamingIndex pst
      var1    = curr_ri+maxExpVars*n+1
      var2    = curr_ri+maxExpVars*n+2
      newpst  = updateRenamingIndex pst (curr_ri+3)
  in
    peval newpst n (Case Rigid xs [Branch (Pattern "[]" []) z,
                                   Branch (Pattern ":" [var1,var2]) 
                                     (Comb FuncCall "foldl" 
                                       [func,
                                        Apply (Apply func z) (Var var1),
                                        Var var2  
                                       ]
                                     )
                                   ]
                    )

-- function foldr:

peval_builtinFOLDR :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinFOLDR pst n f es =
  if proceedUnfold pst (Comb FuncCall f es)
        then peval_builtinFOLDR_aux (incrDepth pst) n es
        else SQ (Comb FuncCall f es)

peval_builtinFOLDR_aux :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinFOLDR_aux pst n [func,z,xs] = 
  let curr_ri = getRenamingIndex pst
      var1    = curr_ri+maxExpVars*n+1
      var2    = curr_ri+maxExpVars*n+2
      newpst  = updateRenamingIndex pst (curr_ri+3)
  in
    peval newpst n (Case Rigid xs [Branch (Pattern "[]" []) z,
                                   Branch (Pattern ":" [var1,var2]) 
                                     (Apply
                                       (Apply func (Var var1)) 
                                       (Comb FuncCall "foldr"
                                        [func, z,  Var var2]
                                       )
                                     )
                                   ]
                    )

-- function filter:

peval_builtinFILTER :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinFILTER pst n f es =
  if proceedUnfold pst (Comb FuncCall f es)
        then peval_builtinFILTER_aux (incrDepth pst) n es
        else SQ (Comb FuncCall f es)

peval_builtinFILTER_aux :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinFILTER_aux pst n [p,xs] = 
  let curr_ri = getRenamingIndex pst
      var1    = curr_ri+maxExpVars*n+1
      var2    = curr_ri+maxExpVars*n+2
      newpst  = updateRenamingIndex pst (curr_ri+3)
  in
    peval newpst n (Case Rigid xs [Branch (Pattern "[]" []) (Comb ConsCall "[]" []),
                                   Branch (Pattern ":" [var1,var2]) 
                                     (Comb FuncCall "if_then_else" 
                                       [Apply p (Var var1),
                                        Comb ConsCall ":" 
                                             [Var var1,
                                              Comb FuncCall "filter" [p, Var var2]
                                             ],
                                        Comb FuncCall "filter" [p, Var var2]  
                                       ]
                                     )
                                   ]
                    )

-- function iterate:

peval_builtinITERATE :: PEvalState -> Int -> String -> [Expr] -> Expr
peval_builtinITERATE pst n f es =
  if proceedUnfold pst (Comb FuncCall f es)
        then peval_builtinITERATE_aux (incrDepth pst) n es
        else SQ (Comb FuncCall f es)

peval_builtinITERATE_aux :: PEvalState -> Int -> [Expr] -> Expr
peval_builtinITERATE_aux pst n [f,x] = 
    peval pst n (Comb ConsCall ":"
                  [x,
                   Comb FuncCall "iterate"
                        [x,
                         Apply f x
                        ]
                  ]
                )

------------------------------------------------------------------------------
-- Auxiliaries:

isCaseVar :: Expr -> Bool
isCaseVar e = case e of
               (Case _ (Var _) _) -> True
               _                  -> False

isCaseCons :: Expr -> Bool
isCaseCons e = case e of
               (Case _ (Comb ConsCall _ _) _) -> True
               _                             -> False

-- is f a defined function?
definedFunc :: PEvalState -> String -> Bool
definedFunc pst f = any (hasName f) (getFuncDecls pst)
definedFunc2 :: [FuncDecl] -> String -> Bool
definedFunc2 funs f = any (hasName f) funs

hasName :: String -> FuncDecl -> Bool
hasName name (Func fname _ _ _) = fname==name


------------------------------------------------------------------------------
-- handling variables in expressions:

-- get the maximum index of all variables in an expression:
-- (or -1 if there is no variable)
maxVarIndex :: Expr -> Int
maxVarIndex (Var v)         = v
maxVarIndex (Lit _)         = -1
maxVarIndex (Comb _ _ exps) = maxList (map maxVarIndex exps)
maxVarIndex (Apply e1 e2)   = max (maxVarIndex e1) (maxVarIndex e2)
maxVarIndex (Constr vars e) = max (maxList vars) (maxVarIndex e)
maxVarIndex (Or e1 e2)      = max (maxVarIndex e1) (maxVarIndex e2)
maxVarIndex (Case _ ce cs)   = max (maxVarIndex ce) (maxList (map maxCase cs))
  where maxCase (Branch (Pattern _ pvs) e) = max (maxList pvs) (maxVarIndex e)
        maxCase (Branch (LPattern _)   e) = maxVarIndex e
maxVarIndex (GuardedExpr vars c e) =
                   max (max (maxList vars) (maxVarIndex c)) (maxVarIndex e)
maxVarIndex (Choice e)      = maxVarIndex e
maxVarIndex (SQ e)      = maxVarIndex e

-- maximum of two numbers:
max :: Int -> Int -> Int
max i j = if i<=j then j else i

-- maximum of a list of naturals (-1 if list is empty):
maxList :: [Int] -> Int
maxList xs = foldr max (-1) xs


--get the list of LOCAL variables in an expression:
localVarsInExp :: Expr -> [Int]
localVarsInExp e = diffList all_vars nonlvars
  where all_vars = varsInExp e
        nonlvars = nonLocalVarsInExp e

diffList :: [Int] -> [Int] -> [Int]
diffList [] [] = []
diffList (v:vs) vars =
  if v `elem` vars then diffList vs vars
                   else v : (diffList vs vars)


-- get list of all variables in an expression:
varsInExp :: Expr -> [Int]
varsInExp (Var v) = [v]
varsInExp (Lit _) = []
varsInExp (Comb _ _ exps) = concat (map varsInExp exps)
varsInExp (Apply e1 e2) = varsInExp e1 ++ varsInExp e2
varsInExp (Constr vars e) = vars ++ varsInExp e
varsInExp (Or e1 e2) = varsInExp e1 ++ varsInExp e2
varsInExp (Case _ e cases) = varsInExp e ++ concat (map varsInCase cases)
varsInExp (GuardedExpr vars c e) = vars ++ varsInExp c ++ varsInExp e
varsInExp (Choice e) = varsInExp e

varsInExp (SQ e) = varsInExp e

varsInCase :: BranchExpr -> [Int]
varsInCase (Branch (Pattern _ as) e) = as ++ varsInExp e
varsInCase (Branch (LPattern _)   e) = varsInExp e


-- delete argument variables in a list of variables
delArgs :: [Int] -> [Int] -> [Int]
delArgs [] vs = vs
delArgs (x:xs) vs = delArgs xs (deleteAll x vs)

------------------------------------------------------------------------------
-- END local control 
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Pretty print 
------------------------------------------------------------------------------

-- pretty printer for expressions (first argument: indentation):
ppExpr :: Int -> Expr -> String
ppExpr _ (Var n) = ppVar n
ppExpr _ (Lit l) = ppLit l
ppExpr b (Comb _ cf es) =
  "(" ++ cf ++ " " ++ ppList (ppExpr b) es ++ ")"
ppExpr b (Apply e1 e2) =
  "(" ++ ppExpr b e1 ++ " " ++ ppExpr b e2 ++ ")"
ppExpr b (Constr xs e) =
  "(Constr " ++ ppFormatList ppVar xs ++ (ppExpr b) e ++ ")"
ppExpr b (Or e1 e2) =
  "(Or " ++ ppExpr b e1 ++ " " ++ ppExpr b e2 ++ ")"
ppExpr b (Case Rigid e cs) =
  "(Case " ++ ppExpr b e ++ " of\n " ++ ppList (ppCase (b+2)) cs
  ++ blanks b ++ ")"
ppExpr b (Case Flex e cs) =
  "(FCase " ++ ppExpr b e ++ " of\n " ++ ppList (ppCase (b+2)) cs
  ++ blanks b ++ ")"
ppExpr b (GuardedExpr xs e1 e2) =
  "(GuardedExpr " ++ ppFormatList ppVar xs
                  ++ ppExpr b e1 ++ " " ++ ppExpr b e2 ++ ")"
ppExpr b (Choice e) = "(Choice " ++ ppExpr b e ++ ")"

ppExpr b (SQ e) = "(SQ " ++ ppExpr b e ++ ")"

ppVar :: Int -> String
ppVar i = "v" ++ show i

ppLit :: Literal -> String
ppLit (Intc   i) = show i
ppLit (Floatc f) = show f
ppLit (Charc  c) = "'" ++ show (ord c) ++ "'"

ppCase :: Int -> BranchExpr -> String
ppCase b (Branch (Pattern l xs) e) = blanks b ++ "(" ++ l ++ " "
                                     ++ ppList ppVar xs
                                     ++ " -> " ++ ppExpr b e ++ ")\n"
ppCase b (Branch (LPattern l) e) = blanks b ++ "(" ++ ppLit l ++ " "
                                   ++ " -> " ++ ppExpr b e ++ ")\n"

blanks :: Int -> String
blanks b = take b (repeat ' ')

-- format a finite list of elements (with a format function for list elems):
ppFormatList :: (a->String) -> [a] -> String
ppFormatList format elems = " [" ++ ppElems elems ++ "] "
 where
   ppElems [] = ""
   ppElems [x] = format x
   ppElems (x1:x2:xs) = format x1 ++ "," ++ ppElems (x2:xs)

ppList :: (a -> String) -> [a] -> String
ppList format elems = ppListElems elems
  where
    ppListElems [] = ""
    ppListElems [x] = format x
    ppListElems (x1:x2:xs) = format x1 ++ " " ++ ppListElems (x2:xs)

buildResultants :: Prog -> [Expr] -> [(Expr,Expr)] -> [(Expr,Expr)]
buildResultants _ [] _ = []
buildResultants (Prog x1 x2 x3 funs x4 x5) (e:es) rho = 
  if e == exp || e == (delSQ exp)
    then  ( pprenLHS e rho , exp ) :
           (buildResultants (Prog x1 x2 x3 funs x4 x5) es rho)
    else  ( pprenLHS e rho, ren_rho funs rho exp) :
           buildResultants (Prog x1 x2 x3 funs x4 x5) es rho
  where exp = peval (PEState (e:es) funs (maxVarIndex e) 0) 0 e

ppResultants :: [(Expr,Expr)] -> IO ()
ppResultants [] = done
ppResultants ((lhs,rhs):es) = 
  do putStrLn (ppExpr 0 lhs ++ " = " ++ ppExpr 0 rhs)
     ppResultants es

ppRen:: [(Expr,Expr)] -> IO ()
ppRen [] = putStr "\n"
ppRen ((e1,e2):es) = 
  do putStrLn (ppExpr 0 e1 ++ "->" ++ ppExpr 0 e2)
     ppRen es


pprenLHS :: Expr -> [(Expr,Expr)] -> Expr
pprenLHS e ((s,ns):rest) = 
  if e == s then ns else pprenLHS e rest

-- pretty print for substitutions..
pprintSub :: Subst -> IO ()
pprintSub FSub = putStrLn "FSub"
pprintSub (Sub vars exps) =
   putStrLn (concatMap show vars) >>
   putStrLn (concat (map (ppExpr 0) exps))


-- pprint: prints a program in FlatCurry form

pprint :: String -> IO ()
pprint p = do (Prog _ _ _ fds _ _) <- readFlatCurry p
              ppFuncDecls fds

ppFuncDecls :: [FuncDecl] -> IO ()
ppFuncDecls fds = foldr (>>) done (map ppFunRule fds)
  where
    ppFunRule (Func fname _ _ rule) = ppRule fname rule

-- pretty printer for rules:
ppRule :: String -> Rule -> IO ()
ppRule fname (Rule lhsVars rhs) =
   putStrLn (fname ++ " " ++ ppList ppVar lhsVars ++ " = " ++ ppExpr 0 rhs)
ppRule fname (External ename) =
   putStrLn (fname ++ " external \"" ++ ename ++ "\"")


------------------------------------------------------------------------------
-- END pretty print 
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Utilities 
------------------------------------------------------------------------------

-- delete all occurrences of an element in a list:
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ []     = []
deleteAll x (y:ys) = if x==y then deleteAll x ys else y : deleteAll x ys


-- check if two sets of terms are equal modulo renaming:
equalMR :: [Expr] -> [Expr] -> Bool
equalMR s1 s2 =  (existsRenSet s1 s2) && (existsRenSet s2 s1) 
 
--check if all the elements of the first set appear modulo renaming in the second set:
existsRenSet :: [Expr] -> [Expr] -> Bool
existsRenSet [] _ = True
existsRenSet (e:s) es = if (existsRen es e) then existsRenSet s es else False
  
--check if a term (second argument) appears (MR) in a list (first argument)
existsRen :: [Expr] -> Expr -> Bool

existsRen [] _ = False
existsRen (e1:es) e = 
  if  not (instanceOf e1 e == FSub) &&  
      not (instanceOf e e1 == FSub)
                      then True
                      else existsRen es e

-- check whether one expression is an instance of another and compute
-- the corresponding substitution:
instanceOf :: Expr -> Expr -> Subst
instanceOf a b =
  if a==b then Sub [] []
          else if isVar b 
                  then instanceVar a b
                  else if headExpr a == headExpr b
                          then instanceS a b
                          else FSub

instanceVar :: Expr -> Expr -> Subst
instanceVar a (Var x) = Sub [x] [a]

--no need to treat vbles (otherwise we don't call to instanceS..)
instanceS :: Expr -> Expr -> Subst
instanceS (Lit x) (Lit y) = 
  if x==y then Sub [] [] else FSub
instanceS (Comb c1 f1 e1) (Comb c2 f2 e2) =
  if c1==c2 && f1==f2 then instanceL e1 e2 (Sub [] [])
                      else FSub
instanceS (Case c1 e1 ces1) (Case c2 e2 ces2) =
  if c1==c2 then instanceCase ces1 ces2 (instanceOf e1 e2)
            else FSub
instanceS (Constr _ e1) (Constr _ e2) = instanceOf e1 e2
instanceS (Choice e1) (Choice e2) = instanceOf e1 e2
instanceS (GuardedExpr _ c1 e1) (GuardedExpr _ c2 e2) =
  instanceL [c1,e1] [c2,e2] (Sub [] [])
instanceS (Or e1 e2) (Or f1 f2) = instanceL [e1,e2] [f1,f2] (Sub [] [])
instanceS (Apply e1 e2) (Apply f1 f2) = instanceL [e1,e2] [f1,f2] (Sub [] [])

instanceL :: [Expr] -> [Expr] -> Subst -> Subst
instanceL [] [] s = s
instanceL (e1:es1) (e2:es2) subs 
  | newsub == FSub    =  FSub
  | clash newsub subs =  FSub
  | otherwise         =  instanceL es1 es2 (composeSubs subs newsub)
  where newsub = instanceOf e1 e2

--
clash :: Subst -> Subst -> Bool
clash (Sub vs es) sub2 = clash_ vs es sub2

clash_ :: [Int] -> [Expr] -> Subst -> Bool
clash_ [] [] _ = False
clash_ (v:vs) (e:es) (Sub vs2 es2) =
  if ve==varBottom then clash_ vs es (Sub vs2 es2) 
                   else not (e==ve)
  where ve = findVal v vs2 es2
  
findVal :: Int -> [Int] -> [Expr] -> Expr
findVal _ [] [] = varBottom
findVal w (v:vs) (e:es) = if w==v then e else findVal w vs es
--

instanceCase :: [BranchExpr] -> [BranchExpr] -> Subst -> Subst
instanceCase _ _ FSub = FSub
instanceCase ces1 ces2 (Sub sx vx) =
  instanceCaseL ces1 ces2 (Sub sx vx)

instanceCaseL :: [BranchExpr] -> [BranchExpr] -> Subst -> Subst
instanceCaseL [] [] s = s
instanceCaseL (e1:es1) (e2:es2) subs 
  | newsub == FSub    =  FSub
  | clash newsub subs =  FSub
  | otherwise         =  instanceCaseL es1 es2 (composeSubs subs newsub)
  where newsub = instanceCaseBranch e1 e2

instanceCaseBranch :: BranchExpr -> BranchExpr -> Subst
instanceCaseBranch (Branch (Pattern c1 _) e1) (Branch (Pattern c2 _) e2) = 
  if c1 == c2 then instanceOf e1 e2
              else FSub
instanceCaseBranch (Branch (LPattern c1) e1) (Branch (LPattern c2) e2) = 
  if c1 == c2 then instanceOf e1 e2
              else FSub

-- composition of substitutions:

composeSubs :: Subst -> Subst -> Subst
composeSubs (Sub vars1 exps1) (Sub vars2 exps2) = 
  (Sub (vars1 ++ vars2) (newExps ++ exps2))
  where newExps = (map (substitute vars2 exps2) exps1)
composeSubs FSub (Sub _ _) = FSub
composeSubs (Sub _ _) FSub = FSub
composeSubs FSub FSub = FSub

--conjunction of a list of boolean terms

andL:: [Bool] -> Bool
andL es = foldr (&&) True es

--disjunction of a list of boolean terms

orL:: [Bool] -> Bool
orL es = foldr (||) False es

--remove duplicated expressions (modulo variable renaming) 
removeRenamings :: [Expr] -> [Expr]
removeRenamings [] = []
removeRenamings (e:es) = if (existsRen es e) then removeRenamings es
                                            else e:(removeRenamings es)

--remove variables iand literals from a list:
removeVarsLits :: [Expr] -> [Expr]
removeVarsLits [] = []
removeVarsLits (e:es) = 
  if (isVar e) || (isLit e) || (isConstr e)  then removeVarsLits es
                                             else e : (removeVarsLits es)

--returns the matching substitution between an expression and a set of expressions
--(FSbub otherwise), as well as the matched term..
search_instance :: Expr -> [Expr] -> (Subst,Expr)
search_instance _ [] = (FSub, Var (-1))
search_instance e (e1:es) = 
  if sub==FSub then search_instance e es
               else (sub,e)
  where sub = instanceOf e e1

--printSubs FSub = "FSub"
--printSubs (Sub vars exps) =
--  "Subs "++(concatMap ppVar vars)++(concatMap (ppExpr 0) exps)

--checks whether a term is an instance of the renaming set.. 
--returns both the substitution and the selected term.. 
--(nondeterministic! it's necessary for the renaming phase..)  -> NOT NOW!
searchInstance :: Expr -> [(Expr,Expr)] -> (Subst,Expr)
searchInstance _ [] = (FSub, Var (-1))
searchInstance e ((e1,e2):es) =
  if sub /= FSub then (sub, e2)
                 else searchInstance e es
  where sub = instanceOf e e1
--searchInstance e (_:es) = searchInstance e es
--deterministic version follows:
--  if sub==FSub 
--     then searchInstance funs e es 
--     else (sub,e2)
--  where sub = instanceOf e e1


--checks whether a term is a constructor instance of the renaming set.. 
--returns the selected term of the renaming set only if the substitution is 'constructor' 
--(i.e., the mathcing substitution does not contain defined function symbols..)
searchConstrInstance :: [FuncDecl] -> Expr -> [(Expr,Expr)] -> (Subst,Expr)
searchConstrInstance _ e [] = (FSub,e)
searchConstrInstance funs e ((e1,e2):es) = 
  if (sub /= FSub) && (isConstructor funs sub)
                         then (sub,e2)
                         else searchConstrInstance funs e es
  where sub = instanceOf e e1

--used to pretty print a list of expressions:
concatBlank :: [String] -> String
concatBlank [] = []
concatBlank (e:es) = e++"\n"++(concatBlank es)

-- delete all SQ symbols in an expression:
delSQ :: Expr -> Expr
delSQ (Var x) = Var x
delSQ (Lit l) = Lit l
delSQ (Comb ftype f args) = Comb ftype f (map delSQ args)
delSQ (Apply e1 e2) = Apply (delSQ e1) (delSQ e2)
delSQ (Constr vs e) = Constr vs (delSQ e)
delSQ (Or e1 e2) = Or (delSQ e1) (delSQ e2)
delSQ (Case ctype e ces) = Case ctype (delSQ e) (map delSQcase ces)
delSQ (GuardedExpr vs c e) = GuardedExpr vs (delSQ c) (delSQ e)
delSQ (Choice e) = Choice (delSQ e)
delSQ (SQ e) = delSQ e

delSQcase :: BranchExpr -> BranchExpr
delSQcase (Branch p e) = Branch p (delSQ e)

------------------------------------------------------------------------------
-- END utilities 
------------------------------------------------------------------------------

-- end of program
