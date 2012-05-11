------------------------------------------------------------------------------
-- Check some properties of all functions defined in a Curry program:
-- (1) Which functions are defined by overlapping left-hand sides?
-- (2) Which functions are indeterministic, i.e., contains an
--     indirect/implicit call to a committed choice?
--
-- Michael Hanus, September 22, 1999
------------------------------------------------------------------------------

import FlatCurry
import Char
import List
import AnaOverlapping
import AnaIndeterminism
import System
import FileGoodies(stripSuffix)

-- Check arguments and call main function:
main = do
  args <- getArgs
  if length args /= 1
   then putStrLn $ "ERROR: Illegal arguments: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: check <module_name>"
   else check (stripSuffix (head args))

check :: String -> IO ()
check progname =
 do prog <- readFlatCurryFile (flatCurryFileName progname)
    putStrLn ("Checking program \""++progname++"\"...\n")
    checkProg prog

checkProg :: Prog -> IO ()
checkProg (Prog mod _ _ funs _) = let tf = showQNameInModule mod in
  checkseq tf funs >> checkindet tf funs

checkseq tf funs =
  if ovfuns == []
  then putStrLn "All functions have non-overlapping definitions."
  else putStr "Functions with overlapping left-hand sides: " >>
       foldr (>>) done (map (printFuncs tf) ovfuns) >>
       putStr "\n"

  where
    ovfuns = filter isOverlapping funs

    isOverlapping (Func _ _ _ _ (Rule _ e))   = orInExpr e
    isOverlapping (Func _ _ _ _ (External _)) = False


checkindet tf funs =
  if indfuns == []
  then putStrLn "There are no indeterministic functions."
  else putStr "Possibly indeterministic functions: " >>
       putStrLn (showFuns (map tf indfuns))

  where indfuns = indetFunctions funs


showFuns fs = concat (map ((++" ") . showFun) fs)

showFun f = if isAlpha (head f) then f else "("++f++")"

printFuncs tf (Func f _ _ _ _) = putStr (showFun (tf f) ++ " ")


-- examples:

-- check "../examples/merge"
-- check "../examples/chords"
