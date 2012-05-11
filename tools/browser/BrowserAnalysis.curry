-----------------------------------------------------------------------------
-- Definition of the various analyses contained in the browser.
--
-- To modify or extend the analysis functionality of the browser,
-- add access to a new analysis here and recompile the browser.
-----------------------------------------------------------------------------

module BrowserAnalysis(moduleAnalyses,allFunctionAnalyses,functionAnalyses) where

import AnalysisTypes
import FlatCurry
import FlatCurryGoodies
import FlatCurryShow(showFlatFunc)
import Overlapping
import PatternComplete
import SolutionComplete
import Nondeterminism
import Dependency
import Indeterminism
import CalledByAnalysis
import Linearity
import AddTypes
import ShowFlatCurry
import List(intersperse)
import Imports
import FileGoodies(stripSuffix)
import ShowGraph

infix 1 `showWith`,`showWithMsg`

-----------------------------------------------------------------------------------
-- The list of all available analyses for individual modules.
-- Each analysis must return a string representation of its analysis result
-- or an IO action to show the result.
moduleAnalyses :: [(String, ModuleAnalysis ModuleAnalysisResult)]
moduleAnalyses =
 [("Interface",
   InterfaceAnalysis (\int -> ContentsResult CurryProg (showInterface False int))),
  --("Write Interface",
  -- InterfaceAnalysis (\int -> ModuleAction (putStrLn (showInterface False int)))),
  --("Read source file",
  -- SourceCodeAnalysis (\fname -> readFile fname >>= \prog ->
  --                               return (ContentsResult CurryProg prog))),
  ("Curry code (generated from FlatCurry)",
   FlatCurryAnalysis (\prog -> ContentsResult CurryProg (showCurryMod False prog))),
  ("Source program with type signatures added", SourceCodeAnalysis addTypes),
  ("FlatCurry code",
   FlatCurryAnalysis (\prog -> ContentsResult CurryProg (showCurryMod True prog))),
  ("FlatCurry expression",
   FlatCurryAnalysis (\prog -> ContentsResult FlatCurryExp (showFlatProg prog)))]

addTypes :: String -> IO ModuleAnalysisResult
addTypes fname
 | take 7 (reverse fname) == "yrrucl."
  = return (ContentsResult OtherText "Can't add types to literate programs")
 | otherwise
  = do prog <- addTypeSignatures (stripSuffix fname)
       return (ContentsResult CurryProg prog)

-----------------------------------------------------------------------------------
-- The list of all available analyses for individual functions.
-- Each analysis must return a string or an IO action representation of its
-- analysis result.
functionAnalyses :: [(String, FunctionAnalysis AnalysisResult)]
functionAnalyses =
 [("Curry code",        LocalAnalysis     (MsgResult . showFuncDeclAsCurry)),
  --("Print Curry code",  withAction (LocalAnalysis (putStr . showFuncDeclAsCurry))),
  --("Print func name",   withAction (GlobalAnalysis printFuncName)),
  ("FlatCurry code",    LocalAnalysis     (MsgResult . showFuncDeclAsFlatCurry)),
  ("FlatCurry expression",LocalAnalysis   (MsgResult . showFlatFunc)),
  ("Calls directly",    LocalAnalysis     callsDirectly `showWithMsg`
       showQDep "Calls the following functions directly in the right-hand sides:"),
  ("Depends on",        GlobalAnalysis    indirectlyDependent `showWithMsg`
       showQDep "Depends on the following functions:"),
  ("Depends on externals", GlobalAnalysis  externalDependent `showWithMsg`
       showQDep "Depends on the following external functions:"),
  ("Dependency graph (DOT)", withAction (GlobalAnalysis viewFuncDepGraphs)),
  ("Local dependency graph (DOT)", withAction (GlobalAnalysis viewFuncLocalDepGraphs)),
  ("Called by",         GlobalAnalysis    calledBy `showWithMsg`
       showDep "Is called by the following functions of the current module:"),
  ("Overlapping rules",
   LocalAnalysis     isOverlappingFunction   `showWithMsg` showOverlap),
  ("Right-linear rules",
   LocalAnalysis     hasRightLinearRules     `showWithMsg` showLinear),
  ("Right-linearity",
   GlobalAnalysis    analyseRightLinearity   `showWithMsg` showLinearity),
  ("Pattern completeness",
   LocalDataAnalysis analyseCompleteness     `showWithMsg` showComplete),
  ("Totally defined",
   GlobalDataAnalysis analyseTotallyDefined  `showWithMsg` showComplete),
  ("Solution complete",
   GlobalAnalysis    analyseSolutionComplete `showWithMsg` showOpComplete),
  ("Nondeterministic",
   GlobalAnalysis    analyseNondeterminism   `showWithMsg` showNondet),
  ("Set-valued",        GlobalAnalysis    analyseSetValued `showWithMsg` showSetValued),
  ("Purity",            GlobalAnalysis    analyseIndeterminism `showWithMsg` showIndet)]


-- The list of all available analyses for sets of functions.
-- Each analysis must return a short(!) string representation (no more than a few chars)
-- of its analysis result that is prefixed to the function name in the list
-- of function. The second (String) component of each analysis entry is a short
-- explanation of the used prefixes.
allFunctionAnalyses :: [(String, String, FunctionAnalysis String)]
allFunctionAnalyses =
 [("Overlapping rules",
   "Meaning of function markings:\n\n"++
   "OVL>>>  : defining rules overlap\n\n"++
   "unmarked: no overlapping rules",
   LocalAnalysis     isOverlappingFunction `showWith` showBool "OVL>>>" ""),
  ("Pattern completeness",
   "Meaning of function markings:\n\n"++
   "INCMP>>> : possibly incompletely defined operation\n\n"++
   "unmarked : completely defined operation",
   LocalDataAnalysis analyseCompleteness   `showWith` showCompleteS),
  ("Totally defined",
   "Meaning of function markings:\n\n"++
   "PARTIAL>>> : possibly partially defined operation\n\n"++
   "unmarked : totally defined operation",
   GlobalDataAnalysis analyseTotallyDefined `showWith` showTotally),
  ("Solution complete",
   "Meaning of function markings:\n\n"++
   "SUSP>>> : operation may suspend\n\n"++
   "unmarked: operation does not suspend",
   GlobalAnalysis    analyseSolutionComplete `showWith` showBool "" "SUSP>>>"),
  ("Nondeterministic",
   "Meaning of function markings:\n\n"++
   "ND>>>   : nondeterministic operation\n\n"++
   "unmarked: deterministic operation",
   GlobalAnalysis    analyseNondeterminism `showWith` showBool "ND>>>" ""),
  ("Right-linearity",
   "Meaning of function markings:\n\n"++
   "RL>>>   : defined by right-linear rules and depend only on\n"++
   "          right-linear functions\n\n"++
   "unmarked: possibly non-right-linear",
   GlobalAnalysis    analyseRightLinearity `showWith` showBool "RL>>>" ""),
  ("Set-valued",
   "Meaning of function markings:\n\n"++
   "SET>>>  : set-valued operation\n\n"++
   "unmarked: single-valued operation",
   GlobalAnalysis    analyseSetValued      `showWith` showBool "SET>>>" ""),
  ("Purity",
   "Meaning of function markings:\n\n"++
   "IMP>>>  : impure (indeterministic) operation\n\n"++
   "unmarked: referentially transparent operation",
   GlobalAnalysis    analyseIndeterminism  `showWith` showBool "IMP>>>" "")]

-- This function is useful to integrate an existing program analysis
-- into the browser by providing a transformation of the analysis results
-- into strings:
showWith :: FunctionAnalysis a -> (a->String) -> FunctionAnalysis String
showWith (LocalAnalysis ana) showresult =
   LocalAnalysis (\f -> showresult (ana f))
showWith (LocalDataAnalysis ana) showresult =
   LocalDataAnalysis (\types f -> showresult (ana types f))
showWith (GlobalAnalysis ana) showresult =
   GlobalAnalysis (\funs -> map (\(name,res)->(name,showresult res)) (ana funs))
showWith (GlobalDataAnalysis ana) showresult =
   GlobalDataAnalysis (\types funs -> map (\(name,res)->(name,showresult res))
                                          (ana types funs))

showWithMsg :: FunctionAnalysis a -> (a->String) -> FunctionAnalysis AnalysisResult
showWithMsg (LocalAnalysis ana) showresult =
   LocalAnalysis (\f -> MsgResult (showresult (ana f)))
showWithMsg (LocalDataAnalysis ana) showresult =
   LocalDataAnalysis (\types f -> MsgResult (showresult (ana types f)))
showWithMsg (GlobalAnalysis ana) showresult =
   GlobalAnalysis (\funs -> map (\(name,res)->(name,MsgResult (showresult res)))
                                (ana funs))
showWithMsg (GlobalDataAnalysis ana) showresult =
   GlobalDataAnalysis (\types funs -> map (\(name,res)->(name,MsgResult (showresult res)))
                                          (ana types funs))

-- Shows a Boolean result:
showBool :: String -> String -> Bool -> String
showBool t _ True  = t
showBool _ f False = f

-- Shows the result of the overlapping analysis.
showOverlap :: Bool -> String
showOverlap True  = "Overlapping"
showOverlap False = "Not Overlapping"

-- Shows the result of the right-linear rules analysis.
showLinear :: Bool -> String
showLinear True  = "Defined by right-linear rules"
showLinear False = "Definition contains non-right-linear rules"

-- Shows the result of the right-linearity analysis.
showLinearity :: Bool -> String
showLinearity True  = "Defined by functions with right-linear rules"
showLinearity False = "Defined by functions containing non-right-linear rules"

-- Shows the result of the completeness analysis.
showComplete :: CompletenessType -> String
showComplete Complete     = "completely defined (i.e., reducible on all constructors)"
showComplete InComplete   = "incompletely defined"
showComplete InCompleteOr =
                    "incompletely defined in each disjunction (but might be complete)"

showCompleteS :: CompletenessType -> String
showCompleteS Complete     = ""
showCompleteS InComplete   = "INCMP>>>"
showCompleteS InCompleteOr = "INCMP>>>"

-- Shows the result of the totally-defined analysis.
showTotallyDefined :: CompletenessType -> String
showTotallyDefined Complete     = "totally defined (i.e., reducible to a value)"
showTotallyDefined InComplete   = "partially defined"
showTotallyDefined InCompleteOr = "partially defined"

showTotally :: CompletenessType -> String
showTotally Complete     = ""
showTotally InComplete   = "PARTIAL>>>"
showTotally InCompleteOr = "PARTIAL>>>"

-- Shows the result of the operational completeness analysis.
showOpComplete :: Bool -> String
showOpComplete True  = "All solutions can be computed"
showOpComplete False = "Evaluation might suspend"

-- Shows the result of the indeterminism analysis.
showIndet :: Bool -> String
showIndet True  = "Impure (indeterministic) operation"
showIndet False = "Referentially transparent"

-- Shows the result of the non-determinism analysis.
showNondet :: Bool -> String
showNondet True  = "Operation might be nondeterministic"
showNondet False = "Deterministic operation"

-- Shows the result of the set-valued analysis.
showSetValued :: Bool -> String
showSetValued True  = "Operation might be set-valued"
showSetValued False = "Single-valued operation"

-- Shows the result of a dependency analysis with title.
showQDep :: String -> [QName] -> String
showQDep title fnames = title ++ "\n" ++ unlines (map (\(m,n)->m++"."++n) fnames)

-- Shows the result of a dependency analysis with title without qualifiers.
showDep :: String -> [QName] -> String
showDep title fnames = title ++ "\n" ++ unlines (map snd fnames)

-- Visualize the result of the dependency graph analysis.
viewFuncDepGraphs :: [FuncDecl] -> [(QName,IO ())]
viewFuncDepGraphs fdecls =
  map (\(f,fgraph)->(f,showDGraph f (isExternal fdecls) fgraph))
      (dependencyGraphs fdecls)

isExternal [] _ = True -- this case should not occur
isExternal (Func g _ _ _ rule : gs) f = if f==g then isExternalRule rule
                                                else isExternal gs f
 where
   isExternalRule (Rule _ _) = False
   isExternalRule (External _) = True

-- Visualize the result of the local dependency graph analysis.
viewFuncLocalDepGraphs :: [FuncDecl] -> [(QName,IO ())]
viewFuncLocalDepGraphs fdecls =
  map (\(f,fgraph)->(f,showDGraph f (\(m,_)->m/=fst f) fgraph))
      (localDependencyGraphs fdecls)

showDGraph :: QName -> (QName->Bool) -> [(QName,[QName])] -> IO ()
showDGraph (mod,_) isExt fnames =
   viewDependencyGraph
       (map (\(f,gs)->(showLocalName f,
                       if isExt f then extAttrs else [],
                       map showLocalName gs))
            fnames)
 where
  showLocalName (m,g) = if m==mod then g else m++'.':g

  -- dot attributes for visualization of external function nodes:
  extAttrs = [("style","filled"),("color",".7 .3 1.0")]

--------------------------------------------------------------------------------
-- This function is useful to integrate an existing program analysis
-- with result type (IO a) into the browser by providing a transformation
-- of the analysis results.
withAction :: FunctionAnalysis (IO _) -> FunctionAnalysis AnalysisResult
withAction (LocalAnalysis ana) =
   LocalAnalysis (\f -> ActionResult (ana f >> done))
withAction (LocalDataAnalysis ana) =
   LocalDataAnalysis (\types f -> ActionResult (ana types f >> done))
withAction (GlobalAnalysis ana) =
   GlobalAnalysis (\funs -> map (\(name,res)->(name,ActionResult (res >> done)))
                                (ana funs))
withAction (GlobalDataAnalysis ana) =
   GlobalDataAnalysis (\types funs -> map (\(name,res)->(name,ActionResult (res>>done)))
                                          (ana types funs))

-- A simple example for a global function analysis of type IO:
printFuncName :: [FuncDecl] -> [(QName,IO ())]
printFuncName = map (\fdecl -> (funcName fdecl, putStrLn (unqualifiedName fdecl)))
