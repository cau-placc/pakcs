------------------------------------------------------------------------
--- Linearity optimizer for function patterns and non-strict unification
---
--- Replace occurrences of (fp =:<= x) by (fp =:<<= x) if the function
--- pattern fp always evaluates to a linear term.
---
--- @author Michael Hanus
--- @version January 2006
------------------------------------------------------------------------

import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Read
import Linearity
import Dependency
import FileGoodies
import System
import List(intersperse)
import ReadShowTerm(showTerm)
import Directory(doesFileExist,renameFile)
import IO
import Maybe(catMaybes)

-- Example: optimizeNonstrictEqualityInModule "last"

-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
    [prog] -> optimizeNonstrictEqualityInModuleIfNecessary (stripSuffix prog)
    _ -> putStrLn $ "ERROR: Illegal arguments: " ++
                    concat (intersperse " " args) ++ "\n" ++
                    "Usage: OptNonStrict.state <module_name>"

optimizeNonstrictEqualityInModuleIfNecessary prog = do
  let progfcy = flatCurryFileName prog
  optimized <- hasCommentOption progfcy "-fpopt"
  if optimized
   then putStrLn ("Program '"++progfcy++"' already optimized")
   else optimizeNonstrictEqualityInModule prog

optimizeNonstrictEqualityInModule prog = do
  progs <- readFlatCurryWithImports prog
  storeOptimizedModule prog
    (optimizeNonstrictEquality (concatMap funcsOfProg progs) (head progs))

storeOptimizedModule prog ([],nsu,lnsu,optmod) = do
  let progfcy = flatCurryFileName prog
      optfcy  = prog++"_lo.fcy"
  writeFile optfcy ("{- -fpopt -}\n" ++ showTerm optmod)
  renameFile optfcy progfcy
  putStrLn (show nsu ++ " non-strict unifications found, " ++ show lnsu ++
            " optimized into linear unifications.")
  putStrLn ("Optimized program written to '"++progfcy++"'")
storeOptimizedModule prog ((cyc:cycs),_,_,_) = do
  putStrLn $ "ERROR: illegal use of function patterns in program \""++prog++"\"!"
  putStrLn $ "The following functions are defined by self-dependent function patterns:"
  putStrLn (concatMap (\ (m,f)->m++"."++f++" ") (cyc:cycs))
  exitWith 1

------------------------------------------------------------------------------
-- Checks whether a file exists and the file starts with a comment of the form
--  "{- ...-}" and contains the second argument as a word in the comment:
hasCommentOption :: String -> String -> IO Bool
hasCommentOption filename option = do
  existsfile <- doesFileExist filename
  if existsfile
   then do cwords <- readWordsInFirstCommentLine filename
           return (option `elem` cwords)
   else return False

-- Read the words in the first comment line and return them, if the file
-- start with a comment, otherwise return the empty list:
readWordsInFirstCommentLine :: String -> IO [String]
readWordsInFirstCommentLine filename = do
  fh <- openFile filename ReadMode
  c1 <- hGetChar fh
  c2 <- hGetChar fh
  if c1=='{' && c2=='-'
   then do cs <- getCommentString fh
           hClose fh
           return (words cs)
   else do hClose fh
           return []

getCommentString fh = do
  c <- hGetChar fh
  if c=='-' then getCommentString' fh
            else do cs <- getCommentString fh
                    return (c:cs)

getCommentString' fh = do
  c <- hGetChar fh
  if c=='}' then return []
            else if c=='-'
                 then do cs <- getCommentString' fh
                         return ('-':cs)
                 else do cs <- getCommentString fh
                         return ('-':c:cs)


------------------------------------------------------------------------------
-- Replace "=:<=" by "=:<<=" if the left argument is a linear term with
-- linear functions.
-- The first argument is the list of all functions occuring in the program,
-- the second argument is the current module to be optimized.
-- The result is a tuple containing the number of "=:<=" occurring in the
-- module, the number of the optimized occurrences, and the optimized module.
optimizeNonstrictEquality :: [FuncDecl] -> Prog -> ([QName],Int,Int,Prog)
optimizeNonstrictEquality allfuns (Prog mod imps ts funs ops) =
  let (cycfuns,nsus,lnsus,optfs) =
          unzip4 (map (optimizeFun (indirectlyDependent allfuns)
                                   (analyseRightLinearity allfuns)) funs)
  in (catMaybes cycfuns, sum nsus, sum lnsus, Prog mod imps ts optfs ops)

-- Optimize a single function definition.
-- The first argument is the list of all functions together with a flag
-- whether they are defined by right-linear rules and functions.
-- The result is (n,l,fd) where n is the number of non-strict equalities
-- in the function definition, l is the number of optimized linear
-- non-strict equalities, and fd is the optimized function definition.
optimizeFun :: [(QName,[QName])] -> [(QName,Bool)] -> FuncDecl
               -> (Maybe QName,Int,Int,FuncDecl)
optimizeFun depinfo lininfo (Func qn ar vis ty (Rule vs e)) =
  let (cyc,nsu,lnsu,opte) = optimizeExp (isDependent depinfo qn,lininfo) e
  in (if cyc then Just qn else Nothing, nsu, lnsu,
      Func qn ar vis ty (Rule vs opte))
optimizeFun _ _ (Func qn ar vis ty (External e)) =
  (Nothing,0,0,Func qn ar vis ty (External e))

-- does a function (argument 3) depend on another function (argument 2)
-- (w.r.t. dependencies given in argument 1)?
isDependent :: [(QName,[QName])] -> QName -> QName -> Bool
isDependent deps dependonfun fname =
   dependonfun `elem` (maybe [] id (lookup fname deps))

optimizeExp :: (QName->Bool, [(QName,Bool)]) -> Expr -> (Bool,Int,Int,Expr)
optimizeExp _ (Var i) = (False,0,0,Var i)
optimizeExp _ (Lit l) = (False,0,0,Lit l)
optimizeExp funinfo@(depinfo,lininfo) (Comb ct f es)
 | f==("Prelude","=:<=") && length es == 2
  = let e1 = head es
        e2 = head (tail es)
        (cyc1,nsu1,lnsu1,opte1) = optimizeExp funinfo e1
        (cyc2,nsu2,lnsu2,opte2) = optimizeExp funinfo e2
        cyclicFP = cyc1 || cyc2 || any depinfo (funcsInExpr e1)
    in if linearExpr e1 && onlyLinearFunctions lininfo e1
       then (cyclicFP,nsu1+nsu2+1,lnsu1+lnsu2+1,
             Comb ct ("Prelude","=:<<=") [opte1,opte2])
       else (cyclicFP,nsu1+nsu2+1,lnsu1+lnsu2,
             Comb ct ("Prelude","=:<=") [opte1,opte2])
 | otherwise
  = let (cycs,nsus,lnsus,optes) = unzip4 (map (optimizeExp funinfo) es)
    in (or cycs, sum nsus, sum lnsus, Comb ct f optes)
optimizeExp funinfo (Free vs e) = 
  let (cyc,nsu,lnsu,opte) = optimizeExp funinfo e
  in  (cyc,nsu,lnsu,Free vs opte)
optimizeExp funinfo (Let bs exp) =
  let (cyc,nsu,lnsu,optexp) = optimizeExp funinfo exp
      (bvs,bes) = unzip bs
      (cycs,nsus,lnsus,optbes) = unzip4 (map (optimizeExp funinfo) bes)
  in (or (cyc:cycs), nsu + sum nsus, lnsu + sum lnsus,
      Let (zip bvs optbes) optexp)
optimizeExp funinfo (Or e1 e2) =
  let (cyc1,nsu1,lnsu1,opte1) = optimizeExp funinfo e1
      (cyc2,nsu2,lnsu2,opte2) = optimizeExp funinfo e2
  in (cyc1||cyc2, nsu1+nsu2, lnsu1+lnsu2, Or opte1 opte2)
optimizeExp funinfo (Case ct exp bs) =
  let (cyc,nsu,lnsu,optexp) = optimizeExp funinfo exp
      (cycs,nsus,lnsus,optbs) = unzip4 (map optimizeBranch bs)
  in (or (cyc:cycs), nsu + sum nsus, lnsu + sum lnsus, Case ct optexp optbs)
 where
  optimizeBranch (Branch patt be) =
    let (ncyc,nsub,lnsub,optbe) = optimizeExp funinfo be
    in (ncyc,nsub,lnsub,Branch patt optbe)

-- does an expression contains only functions defined by right-linear rules
-- and functions?
onlyLinearFunctions :: [(QName,Bool)] -> Expr -> Bool
onlyLinearFunctions li e = all isRightLinearDefined (funcsInExpr e)
 where
   isRightLinearDefined fun = maybe False id (lookup fun li)

-- goodies:

funcsOfProg (Prog _ _ _ funcs _) = funcs

sum = foldl (+) 0

unzip4              :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 []           = ([],[],[],[])
unzip4 ((x,y,z,v):ts) = (x:xs,y:ys,z:zs,v:vs) where (xs,ys,zs,vs) = unzip4 ts

