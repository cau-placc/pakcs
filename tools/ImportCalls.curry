-----------------------------------------------------------------------------
-- Show all calls to imported functions in a module
--
-- Michael Hanus, November 2003
-----------------------------------------------------------------------------

module ImportCalls(main,showImportCalls) where

import FlatCurry
import Char
import List
import Sort
import System
import Directory
import FileGoodies
import Distribution

m1 = showAllImportedCalls "ImportCalls"

-- Check arguments and call main function:
main = do
  args <- getArgs
  if length args /= 1
   then putStrLn $ "ERROR: Illegal arguments: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: importcalls <module_name>"
   else showAllImportedCalls (stripSuffix (head args))

showAllImportedCalls modname = do
  prog <- readCurrentFlatCurry modname
  putStrLn ("Calls to imported functions/constructors in module \""++modname++"\":\n")
  putStrLn (showImportCalls prog)

showImportCalls :: Prog -> String
showImportCalls = formatImpCalls . getAllImpCalls

-- format import calls as import declarations:
formatImpCalls :: [(String,[String])] -> String
formatImpCalls impcalls =
  concatMap (\(mod,imps)->"import "++mod++"("++
                          concat (intersperse "," (map showName imps))++")\n")
            impcalls
 where
   showName name = if isAlpha (head name) then name else '(':name++")"

getAllImpCalls :: Prog -> [(String,[String])]
getAllImpCalls (Prog mod imps _ funs _) =
  calls2impCalls imps
                (mergeSort (\(_,n1) (_,n2)->leqString n1 n2)
                           (allFunCalls mod funs))

calls2impCalls :: [String] -> [QName] -> [(String,[String])]
calls2impCalls [] _ = []
calls2impCalls (mod:mods) funs = 
 (mod, map snd (filter (\(m,_)->m==mod) funs)) : calls2impCalls mods funs

-- Computes the list of called functions in a list of function declarations
allFunCalls :: String -> [FuncDecl] -> [QName]
allFunCalls _ [] = []
allFunCalls mod (Func _ _ _ _ (Rule _ e) : funs) =
     union (globalFunsInExpr mod e) (allFunCalls mod funs)
allFunCalls mod (Func _ _ _ _ (External _) : funs) = allFunCalls mod funs

-- Gets a list of all functions called in an expression that are not defined
-- in a module (first argument):
globalFunsInExpr :: String -> Expr -> [QName]
globalFunsInExpr mod exp = funsInExpr exp
 where
  funsInExpr (Var _) = []
  funsInExpr (Lit _) = []
  funsInExpr (Comb _ (m,f) es) =
    if m==mod || (m=="Prelude" && f `elem` ["commit","apply","cond"])
    then nub (concatMap funsInExpr es)
    else nub ((m,f) : concatMap funsInExpr es)
  funsInExpr (Free _ e) = funsInExpr e
  funsInExpr (Let bs e) = union (nub (concatMap (funsInExpr . snd) bs)) (funsInExpr e)
  funsInExpr (Or e1 e2) = union (funsInExpr e1) (funsInExpr e2)
  funsInExpr (Case _ e bs) = union (funsInExpr e)
                                   (nub (concatMap funsInBranch bs))
                       where funsInBranch (Branch _ be) = funsInExpr be


----------------- Auxiliaries:

-- Read a FlatCurry program (parse only if necessary):
readCurrentFlatCurry :: String -> IO Prog
readCurrentFlatCurry modname = do
  progname <- findSourceFileInLoadPath modname
  let fcyprogname = flatCurryFileName progname
  fcyexists <- doesFileExist fcyprogname
  if not fcyexists
    then readFlatCurry progname
    else do ctime <- getSourceModificationTime progname
            ftime <- getModificationTime fcyprogname
            if ctime>ftime
             then readFlatCurry progname
             else readFlatCurryFile fcyprogname

getSourceModificationTime progname = do
  lexists <- doesFileExist (progname++".lcurry")
  if lexists then getModificationTime (progname++".lcurry")
             else getModificationTime (progname++".curry")

-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath modname = do
  loadpath <- getLoadPathForFile modname
  mbfname <- lookupFileInPath (baseName modname) [".lcurry",".curry"] loadpath
  maybe (error ("Curry file for module \""++modname++"\" not found!"))
        (return . stripSuffix)
        mbfname
