---------------------------------------------------------------------
--- GUI for CurryBrowser, a generic analysis environment for declarative
--- programs.
---
--- @author Michael Hanus
---------------------------------------------------------------------

module BrowserGUI where

import GUI
import Read
import IOExts
import Imports
import FlatCurry
import FlatCurryGoodies
import FlatCurryShow
import ShowFlatCurry(leqFunc)
import System
import FileGoodies
import List
import Maybe
import AnalysisTypes
import BrowserAnalysis
import Sort
import Dependency(callsDirectly,indirectlyDependent)
import ShowGraph
import ImportCalls
import BrowserPropertyFile
import Directory
import Time(toCalendarTime,calendarTimeToString)
import Distribution(installDir)

---------------------------------------------------------------------
-- Set this constant to True if the execution times of the main operations
-- should be shown in the status line:
showExecTime = False

---------------------------------------------------------------------
-- Title and version
title = "CurryBrowser"

version = "Version of 22/03/2012"

patchReadmeVersion = do
  readmetxt <- readCompleteFile "README"
  writeFile "README" (unlines . map replaceVersion . lines $ readmetxt)
 where
  replaceVersion l = if isPrefixOf "Version" l then version else l

---------------------------------------------------------------------
-- Main program: check arguments, read interfaces, and run GUI:
main = do
  args <- getArgs
  case args of
   [a] -> start (baseName (stripSuffix a))
   _ -> putStrLn $ "ERROR: Illegal arguments for CurryBrowser: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: currybrowser <module_name>"

start mod = do
  putStrLn "Please be patient, reading all interfaces..."
  helptxt <- readFile (browserDir++"/README")
  mods <- getImportedInterfaces mod
  let mainmod = moduleName (progOfIFFP (snd (head mods)))
      trees = [Leaf (if mainmod==mod then mod else mod++"(module: "++mainmod++")")
                    (mainmod,map (moduleImports . progOfIFFP . snd) mods)]
  stateref <- newIORef (trees,
                        (mainmod,(mod,snd (head mods)))
                         : map (\ (m,p)->(m,(m,p))) (tail mods),
                        [],("",OtherText,""),False,Nothing)
  runInitGUI (title++" ("++version++")")
             (browserGUI stateref rmod rtxt (trees2strings trees))
             (\gp -> setValue rtxt helptxt gp >> setValue rmod "0" gp)
 where rmod,rtxt free

m1 = start "BrowserGUI"
m2 = start "Imports"
m3 = start "demo"
m4 = start "rev"

-----------------------------------------------------------------------------
-- Structure of import tree to be shown:
data Tree a = Leaf String a          -- leaves have a name and a value
            | Node String a [Tree a] -- nodes have a name, value and a list of subtrees

trees2strings trees = concatMap (tree2strings 0) trees

tree2strings n (Leaf t _)       = [blanks n ++ "+ "++t]
tree2strings n (Node t _ trees) = (blanks n ++"- "++t)
                                  : concatMap (tree2strings (n+2)) trees

blanks n = take n (repeat ' ')


-- get name of selected node in the tree list:
getTreesNodeName :: Int -> [Tree _] -> String
getTreesNodeName _ [] = error "getTreesNodeName: nothing selected" -- should not occur
getTreesNodeName n (Leaf name _ : trees) =
  if n==0 then name
          else getTreesNodeName (n-1) trees
getTreesNodeName n (Node t v subtrees : trees) =
  if n==0 then t
          else let l = length (tree2strings 0 (Node t v subtrees)) in
               if n < l
               then getTreesNodeName (n-1) subtrees
               else getTreesNodeName (n-l) trees

-- get selected value in the tree list:
getTreesValue :: Int -> [Tree a] -> a
getTreesValue _ [] = error "getTreesValue: nothing selected" -- should not occur
getTreesValue n (Leaf _ v : trees) =
  if n==0 then v 
          else getTreesValue (n-1) trees
getTreesValue n (Node t v subtrees : trees) =
  if n==0 then v
          else let l = length (tree2strings 0 (Node t v subtrees)) in
               if n < l
               then getTreesValue (n-1) subtrees
               else getTreesValue (n-l) trees

-- change tree when click on some line (i.e., open or close a node):
changeTrees :: Int -> ImportTree -> IO ImportTree
changeTrees _ [] = return [] -- should not occur
changeTrees n (Leaf t v : trees) =
  if n==0 then openNode v >>= \newst -> return (Node t v newst : trees)
          else changeTrees (n-1) trees >>= \nts -> return (Leaf t v : nts)
changeTrees n (Node t v subtrees : trees) =
  if n==0 then return (Leaf t v : trees)
          else let l = length (tree2strings 0 (Node t v subtrees)) in
               if n < l
               then (changeTrees (n-1) subtrees) >>= \nsts ->
                    return (Node t v nsts : trees)
               else changeTrees (n-l) trees >>= \nts -> return (Node t v subtrees : nts)

openNode (mod,modimps) = let mbimps = lookup mod modimps in
  return $  maybe [] (map (\m->Leaf m (m,modimps))) mbimps

-----------------------------------------------------------------------------
-- Operations on the state of the GUI:
-- The GUI state consists of
-- * the import tree
-- * the list of all modules (module name, file name, FlatCurry interface or program)
-- * the list of all currently shown functions of the module
-- * the name and contents of the module currently shown in the contents window
-- * flag: True if function list shows functions of currently selected module
-- * the currently selected function analysis

type ImportTree = [Tree (String,[(String,[String])])]


type GuiState = (ImportTree,[(String,(String,InterfaceOrFlatProg))],
                 [FuncDecl],(String,ContentsKind,String),Bool,
                 Maybe (FunctionAnalysis AnalysisResult))

getModuleFileName gs mod = do
  (_,mods,_,_,_,_) <- readIORef gs
  return (fst (fromJust (lookup mod mods)))


getTrees gs = readIORef gs >>= \(trees,_,_,_,_,_) -> return trees

storeTrees gs trees = do
  (_,ms,_,ct,_,fana) <- readIORef gs
  writeIORef gs (trees,ms,[],ct,False,fana)

-- extract all reflexive-transitive imports for a module from GUI state:
getAllImportsOfModule gs mod = do
  (trees,_,_,_,_,_) <- readIORef gs
  return (collectImports (importsOfRoot trees) [mod] [])
 where
   importsOfRoot ((Leaf _ (_,imps)) :_)   = imps
   importsOfRoot ((Node _ (_,imps) _) :_) = imps

   collectImports _ [] imps = imps
   collectImports modimps (m:ms) doneimps =
    if m `elem` doneimps
    then collectImports modimps ms doneimps
    else collectImports modimps (ms ++ fromJust (lookup m modimps)) (m:doneimps)

getFuns gs = readIORef gs >>= \(_,_,funs,_,_,_) -> return funs

storeSelectedFunctions gs funs = do
  (t,ms,_,ct,flag,fana) <- readIORef gs
  writeIORef gs (t,ms,mergeSort leqFunc funs,ct,flag,fana)

setMainContentsModule :: IORef GuiState -> String -> ContentsKind -> String -> IO ()
setMainContentsModule gs cntmod cntkind contents = do
  (t,ms,fs,_,flag,fana) <- readIORef gs
  writeIORef gs (t,ms,fs,
                 (if cntkind==OtherText then "" else cntmod,cntkind,contents),
                 flag,fana)

getContentsModule :: IORef GuiState -> IO String
getContentsModule gs = do
  (_,_,_,(cntmod,_,_),_,_) <- readIORef gs
  return cntmod

getMainContents :: IORef GuiState -> IO (ContentsKind,String)
getMainContents gs = do
  (_,_,_,(_,cntkind,contents),_,_) <- readIORef gs
  return (cntkind,contents)

getFunctionListKind :: IORef GuiState -> IO Bool
getFunctionListKind gs = do
  (_,_,_,_,flag,_) <- readIORef gs
  return flag

setFunctionListKind :: IORef GuiState -> Bool -> IO ()
setFunctionListKind gs flag = do
  (t,ms,fs,cnttype,_,fana) <- readIORef gs
  writeIORef gs (t,ms,fs,cnttype,flag,fana)

-- get the interfaces (or FlatCurry programs) of all modules:
getAllModules gs = do
  (_,mods,_,_,_,_) <- readIORef gs
  return (map (progOfIFFP . snd . snd) mods)

-- get the interface (or FlatCurry program) of a specific module:
getIntWithName gs name = do
  (_,mods,_,_,_,_) <- readIORef gs
  return (progOfIFFP . snd . fromJust . lookup name $ mods)

-- get the FlatCurry program of a specific module (read, if necessary):
getProgWithName gs prt name = do
  (_,mods,_,_,_,_) <- readIORef gs
  ifOrProg (\_->readProgAndStore gs prt name) return (snd (fromJust (lookup name mods)))

-- Get all data type declarations (also imported) of a module:
getAllTypes gs _ mod =
  getAllImportsOfModule gs mod >>= \imps ->
  readIORef gs >>= \(_,mods,_,_,_,_) ->
  return (concatMap (typesOfProg . progOfIFFP . snd . snd)
                    (filter ((`elem` imps) . fst) mods))

-- Get all function declarations (also imported) of a module:
getAllFunctions :: IORef GuiState -> (String->IO ()) -> String -> IO [FuncDecl]
getAllFunctions gs prt mod = do
  imps <- getAllImportsOfModule gs mod
  (_,mods,_,_,_,_) <- readIORef gs
  mapIO_ (readProgAndStoreIfNecessary gs prt) (filter ((`elem` imps) . fst) mods)
  (_,newmods,_,_,_,_) <- readIORef gs
  return (concatMap (funcsOfProg . progOfIFFP . snd . snd)
                    (filter ((`elem` imps) . fst) newmods))

-- Get all function names (also imported) of a module:
getAllFunctionNames :: IORef GuiState -> String -> IO [QName]
getAllFunctionNames gs mod =
  getAllImportsOfModule gs mod >>= \imps ->
  readIORef gs >>= \(_,mods,_,_,_,_) ->
  return (map funcName (concatMap (funcsOfProg . progOfIFFP . snd . snd)
                                  (filter ((`elem` imps) . fst) mods)))

-- Get currently selected function analysis:
getCurrentFunctionAnalysis :: IORef GuiState ->
                              IO (Maybe (FunctionAnalysis AnalysisResult))
getCurrentFunctionAnalysis gs = do
  (_,_,_,_,_,fana) <- readIORef gs
  return fana

-- Set current function analysis:
setCurrentFunctionAnalysis :: IORef GuiState ->
                              (Maybe (FunctionAnalysis AnalysisResult)) -> IO ()
setCurrentFunctionAnalysis gs fana = do
  (ts,mods,fs,ct,flag,_) <- readIORef gs
  writeIORef gs (ts,mods,fs,ct,flag,fana)

-- read a FlatCurry program and store if not already done
readProgAndStore gs prt mod = do
  (ts,mods,fs,ct,flag,fana) <- readIORef gs
  prog <- readFlatCurryFileInLoadPath prt (fst (fromJust (lookup mod mods)))
  writeIORef gs (ts,update mod prog mods,fs,ct,flag,fana)
  return prog
 where
  update _ _ [] = []
  update nm pr ((n,(f,p)):ms) = if n==nm then (n,(f,FP pr)):ms
                                         else (n,(f,p)) : update nm pr ms

-- read a FlatCurry program and store if not already done
readProgAndStoreIfNecessary _ _ (_,(_,FP _)) = done
readProgAndStoreIfNecessary gs prt (name,(_,IF _)) =
   readProgAndStore gs prt name >> done

-- find a function declaration in a list of fdecls for a given name:
findDecl4name :: [FuncDecl] -> QName -> FuncDecl
findDecl4name [] _ = error "Internal error in fundDecl4name!"
findDecl4name (fd:fds) qn | funcName fd == qn = fd
                          | otherwise         = findDecl4name fds qn

-----------------------------------------------------------------------------
-- The GUI of the browser:

browserGUI :: IORef GuiState -> WidgetRef -> WidgetRef -> [String] -> Widget
browserGUI gstate rmod rtxt names =
 col [
   row [
     Col [LeftAlign] [
       Label [Text "Select module and imports:"],
       ListBoxScroll [WRef rmod, List names, Width 20, Height 14,
                      Cmd (showBusy selmod), Background "yellow", Fill],
       MenuButton
        [Text "Analyze selected module...",
         Menu (map (\ (aname,acmt,afun) -> MButton (showMBusy (analyzeAllFuns acmt afun))
                                              aname)
                   allFunctionAnalyses)],
       row [MenuButton
             [Text "Select functions...",
              Menu [MButton (showMBusy (executeForModule showExportedFuns))
                            "exported and defined in selected module",
                    MButton (showMBusy (executeForModule showAllModuleFuns))
                            "defined in selected module",
                    MButton (showMBusy (executeForModule showAllExportedFuns))
                            "exported by selected and imported modules",
                    MButton (showMBusy selectDirectCalls)
                            "all direct dependants from selected function",
                    MButton (showMBusy selectInDirectCalls)
                            "all dependants from selected function"]],
            CheckButton [Text "focus in code", WRef focusbutton,
                         Cmd focusFunctionIfSelected]],
       ListBoxScroll [WRef rfun, Width 20, Height 16,
                      Cmd (showBusy selectFunction), Background "white", Fill]],
     Col [LeftAlign] [
       row [Button (showBusy (executeForModule showSource)) [Text "Show source"],
            MenuButton
              [Text "Show selected module as...",
               Menu (map (\ (t,ma) ->
                            MButton (showMBusy (executeForModule
                                                    (analyzeModuleWith ma))) t)
                         moduleAnalyses)],
            MenuButton
              [Text "Tools...",
               Menu [MButton (showMBusy (executeForModule showImpCalls))
                             "List calls to imported functions in selected module",
                     MButton (showMBusy showImportGraph)
                             "Show import graph of all modules (except prelude) (DOT)"]],
            MenuButton
              [Text "File...",
               Menu [MButton (showMBusy (executeForModule showModuleInfo))
                             "...information of selected module",
                     MButton (showMBusy saveMainText) "Save program text as...",
                     MSeparator,
                     MButton (\gp->exitGUI gp >> return []) "Exit"]],
            MenuButton
              [Text "Settings...",
               Menu [MButton setViewDot
                             "Set viewer for dot graph specifications"]],
            Label [FillX],
            MenuButton
              [Text "Help...",
               Menu [MButton (showMBusy (help "README"))
                             "About CurryBrowser",
                     MSeparator,
                     MButton (showMBusy (help "Help.txt"))
                             "How to use CurryBrowser",
                     MButton (showMBusy (help "Extend.txt"))
                             "How to extend CurryBrowser"]]
              ],
       TextEditScroll [WRef rtxt, Height 25, Width 80, Background "white"],
       Row [LeftAlign]
        [Label [Text "Current function analysis:"],
         Entry [Text noAnalysisText, WRef anaentry,
                Background "white", FillX],
         MenuButton
          [Text "Select analysis...",
           Menu (MButton (showMBusy deselectFunAna) noAnalysisText :
                 map (\(name,ana) -> MButton (showMBusy (selectAna name ana))
                                             name)
                     functionAnalyses)]],
       TextEditScroll [WRef resultwidget, Height 5, Width 72,
                       Background "white"]]],
   Label [WRef rstatus, Text "Status: ready", Background "green", FillX]]
 where
  resultwidget,rfun,focusbutton,rstatus,anaentry free

  saveMainText gp = do
    file <- getSaveFile
    if null file
     then done
     else getValue rtxt gp >>= writeFile file

  -- put a message in main contents widget:
  putMainMessage gp msg = do
    setValue rtxt msg gp
    setMainContentsModule gstate "" OtherText msg

  -- set viewer for DOT files:
  setViewDot _ = do
     oldcmd <- getBrowserConfig "viewdot"
     getAnswer "Command to view dot files:" oldcmd
               (\cmd->if oldcmd==cmd then done
                                     else setBrowserConfig "viewdot" cmd)
     return []

  -- show info texts:
  help localhelpfile gp =
    readFile (browserDir++"/"++localhelpfile) >>= putMainMessage gp

  -- show business while executing an event handler:
  showBusy handler gp = do
     setConfig rstatus (Background "red") gp
     setConfig rstatus (Text "Status: running") gp
     time1 <- getCPUTime
     handler gp
     time2 <- getCPUTime
     setConfig rstatus (Text $ if showExecTime
                               then "Status: ready (" ++ show(time2-time1) ++ " msecs)"
                               else "Status: ready") gp
     setConfig rstatus (Background "green") gp

  showMBusy handler gp = showBusy handler gp >> return []

  -- show what we are doing in status line:
  showDoing gp str = setConfig rstatus (Text ("Status: "++str)) gp

  -- Execute an I/O action safely, i.e., catch all errors and failures:
  safeIO gp act =
    catch act (\e -> putMainMessage gp ("Failure occurred: "++showError e))

  -- click on a module name in left module column:
  selmod gp =
    getValue rmod gp >>= \sel ->
    if sel==""
      then done
      else putMainMessage gp "" >>
           setConfig rfun (List []) gp >>
           getTrees gstate >>= \trees ->
           changeTrees (readNat sel) trees >>= \newtrees ->
           storeTrees gstate newtrees >>
           setConfig rmod (List (trees2strings newtrees)) gp >>
           setValue resultwidget "" gp >>
           setValue rmod sel gp

  -- get the name of the selected module (or Nothing in case of no selection):
  getSelectedModName gp = do
    sel <- getValue rmod gp
    if sel==""
      then return Nothing
      else getTrees gstate >>= \trees ->
           return (Just (fst (getTreesValue (readNat sel) trees)))

  -- execute event handler on the selected module
  -- (or show "nothing selected" message):
  executeForModule modhandler gp =
    getSelectedModName gp >>= \mod ->
    if mod==Nothing
      then putMainMessage gp "No module selected!"
      else modhandler (fromJust mod) gp

  -- analyze a selected module:
  analyzeModuleWith modanalysis mod gp = safeIO gp $
    performModuleAnalysis modanalysis (showDoing gp) mod >>= \res ->
    showModAnalysisResult mod res gp
    
  showModAnalysisResult mod (ContentsResult cntkind contents) gp = do
    setValue rtxt contents gp
    setMainContentsModule gstate mod cntkind contents
  showModAnalysisResult _ (ModuleAction act) _ = act

  -- show module source code:
  showSource mod gp = do
    fmod <- getModuleFileName gstate mod
    mbprogname <- Imports.findFileInLoadPath fmod [".lcurry",".curry"]
    maybe (putMainMessage gp "Source file does not exist!")
          (\filename -> readFile filename >>= \source ->
                        setValue rtxt source gp >>
                        setMainContentsModule gstate mod
                            (if take 7 (reverse filename) == "yrrucl."
                             then LCurryProg else CurryProg) source)
          mbprogname

  -- show information about a module:
  showModuleInfo mod gp = do
    fmod <- getModuleFileName gstate mod
    mbsrcfile <- Imports.findFileInLoadPath fmod [".lcurry",".curry"]
    mbfcyfile <- Imports.findFileInLoadPath fmod [".fcy"]
    srcinfo   <- getFileInfo 2 mbsrcfile
    fcyinfo   <- getFileInfo 4 mbfcyfile
    let msg = "Source file:    " ++ srcinfo ++
            "\nFlatCurry file: " ++ fcyinfo
    putMainMessage gp msg

  -- returns information about a possible file:
  getFileInfo _ Nothing = return "does not exist"
  getFileInfo bls (Just fname) = do
    fsize <- fileSize fname
    ftime <- getModificationTime fname
    ctime <- toCalendarTime ftime
    return $ fname ++ take bls (repeat ' ')
                   ++ "(" ++ calendarTimeToString ctime
                   ++ ", size: " ++ show fsize ++ " bytes)"

  -- show module dependency graph:
  showImportGraph gp =
    getAllModules gstate >>= \mods ->
    safeIO gp $
       viewDependencyGraph
         (concatMap (\(Prog mod imps _ _ _) ->
                          if mod=="Prelude" then []
                                            else [(mod,[],delete "Prelude" imps)]) mods)

  -- show import calls of selected module:
  showImpCalls mod gp =
    getProgWithName gstate (showDoing gp) mod >>= \prog ->
    putMainMessage gp (showImportCalls prog)

  -- show module's functions:
  showAllModuleFuns mod gp = do
    prog <- getProgWithName gstate (showDoing gp) mod
    storeSelectedFunctions gstate (funcsOfProg prog)
    setFunctionListKind gstate True
    funs <- getFuns gstate
    setConfig rfun (List (map unqualifiedName funs)) gp

  -- show module's exported functions:
  showExportedFuns mod gp = do
    prog <- getProgWithName gstate (showDoing gp) mod
    storeSelectedFunctions gstate (filter isPublic (funcsOfProg prog))
    setFunctionListKind gstate True
    funs <- getFuns gstate
    setConfig rfun (List (map unqualifiedName funs)) gp

  -- show exported functions of module and selected modules:
  showAllExportedFuns mod gp = do
    allfuns <- getAllFunctions gstate (showDoing gp) mod
    storeSelectedFunctions gstate (filter isPublic allfuns)
    setFunctionListKind gstate False
    funs <- getFuns gstate
    setConfig rfun (List (map showQNameWithMod (map funcName funs))) gp

  -- select all functions that directly depend on selected function:
  selectDirectCalls gp = do
    mod <- getSelectedModName gp
    self <- getValue rfun gp
    if mod==Nothing || null self then done else
      getFuns gstate >>= \funs ->
      let mainfun = funs!!(readNat self)
          qfnames = mergeSort leqQName (union [funcName mainfun] (callsDirectly mainfun))
       in getAllFunctions gstate (showDoing gp) (fromJust mod) >>= \allfuns ->
          storeSelectedFunctions gstate (map (findDecl4name allfuns) qfnames) >>
          setFunctionListKind gstate False >>
          setConfig rfun (List (map showQNameWithMod qfnames)) gp

  -- select all functions that indirectly depend on selected function:
  selectInDirectCalls gp = do
    mod <- getSelectedModName gp
    self <- getValue rfun gp
    if mod==Nothing || null self then done else
      getFuns gstate >>= \funs ->
      let mainfun = funcName (funs!!(readNat self)) in
      getAllFunctions gstate (showDoing gp) (fromJust mod) >>= \allfuns ->
      let qfnames = mergeSort leqQName
             (union [mainfun] (fromJust (lookup mainfun (indirectlyDependent allfuns))))
       in storeSelectedFunctions gstate (map (findDecl4name allfuns) qfnames) >>
          setFunctionListKind gstate False >>
          setConfig rfun (List (map showQNameWithMod qfnames)) gp

  -- click on a name in function column:
  selectFunction gp = safeIO gp $ do
    focusFunctionIfSelected gp
    analyzeFunctionIfSelected gp

  -- select a function analysis from the menu:
  selectAna ananame funana gp = safeIO gp $ do
    setCurrentFunctionAnalysis gstate (Just funana)
    setValue anaentry ananame gp
    analyzeFunctionIfSelected gp

  -- deselect function analysis from the menu:
  deselectFunAna gp = do
    setCurrentFunctionAnalysis gstate Nothing
    setValue anaentry noAnalysisText gp
    setValue resultwidget "" gp

  -- perform a function analysis if function is selected:
  analyzeFunctionIfSelected gp = do
    mod  <- getSelectedModName gp
    self <- getValue rfun gp
    fana <- getCurrentFunctionAnalysis gstate
    funs <- getFuns gstate
    if mod==Nothing || null self || fana==Nothing then done else do
      result <- performAnalysis (fromJust fana) (showDoing gp)
                                (funs!!readNat self)
      showAnalysisResult result gp

  showAnalysisResult (MsgResult str) gp = setValue resultwidget str gp
  showAnalysisResult (ActionResult act) _ = act


  -- focus on a function if selected:
  focusFunctionIfSelected gp = do
    self <- getValue rfun gp
    focusvalue <- getValue focusbutton gp
    funs <- getFuns gstate
    if null self || focusvalue=="0" then done
     else showModuleAndFocusFunction gp (funcName (funs!!readNat self))

  -- focus on a function and load the source code, if necessary:
  showModuleAndFocusFunction gp (fmod,fname) =
    getContentsModule gstate >>= \cntmod ->
    if fmod == cntmod
    then getMainContents gstate >>= \(ct,cnt) ->
         let row = findFunDeclInProgText ct cnt (fmod,fname)
          in if row==0 then done else seeText rtxt (row,1) gp
    else showSource fmod gp >>
         getMainContents gstate >>= \(ct,cnt) ->
         let row = findFunDeclInProgText ct cnt (fmod,fname)
          in if row==0 then done else seeText rtxt (row,1) gp

  -- analyze all functions in the function column:
  analyzeAllFuns explanation analysis gp = safeIO gp $ do
    mod <- getSelectedModName gp
    if mod==Nothing then done else
      getFunctionListKind gstate >>= \modfuns ->
      (if modfuns then done else showExportedFuns (fromJust mod) gp) >>
      getFuns gstate >>= \funs ->
      setValue resultwidget explanation gp >>
      performAllAnalysis analysis (showDoing gp) (fromJust mod) funs >>= \anaresults ->
      setConfig rfun (List (map (\(prefix,func)-> prefix++" "++unqualifiedName func)
                                (zip anaresults funs))) gp

  -- Perform an analysis on a module:
  performModuleAnalysis (InterfaceAnalysis ana) _ mod = do
    int <- getIntWithName gstate mod
    return (ana int)
  performModuleAnalysis (FlatCurryAnalysis ana) prt mod = do
    prog <- getProgWithName gstate prt mod
    return (ana prog)
  performModuleAnalysis (SourceCodeAnalysis ana) _ mod = do
    mbfilename <- Imports.findFileInLoadPath mod [".lcurry",".curry"]
    maybe (return (ContentsResult
                   OtherText ("Curry source file for module \""++mod++"\" not found!")))
          (\filename -> ana filename)
          mbfilename

  -- Perform an analysis to a single function declaration:
  performAnalysis (LocalAnalysis ana) prt fdecl = do
    prt "Analyzing..."
    return (ana fdecl)
  performAnalysis (LocalDataAnalysis ana) prt fdecl = do
    types <- getAllTypes gstate prt (funcModule fdecl)
    prt "Analyzing..."
    return (ana types fdecl)
  performAnalysis (GlobalAnalysis ana) prt fdecl = do
    funcs <- getAllFunctions gstate prt (funcModule fdecl)
    prt "Analyzing..."
    return (fromJust (lookup (funcName fdecl) (ana funcs)))
  performAnalysis (GlobalDataAnalysis ana) prt fdecl = do
    let mod = funcModule fdecl
    types <- getAllTypes gstate prt mod
    funcs <- getAllFunctions gstate prt mod
    prt "Analyzing..."
    return (fromJust (lookup (funcName fdecl) (ana types funcs)))

  -- Perform an analysis to a list of function declarations:
  performAllAnalysis (LocalAnalysis ana) prt _ fdecls = do
    prt "Analyzing..."
    return (map ana fdecls)
  performAllAnalysis (LocalDataAnalysis ana) prt mod fdecls = do
    types <- getAllTypes gstate prt mod
    prt "Analyzing..."
    return (map (ana types) fdecls)
  performAllAnalysis (GlobalAnalysis ana) prt mod fdecls =
    getAllFunctions gstate prt mod >>= \funcs ->
    prt "Analyzing..." >>
    let anaresults = ana funcs in
    return (map (\fd->fromJust (lookup (funcName fd) anaresults)) fdecls)
  performAllAnalysis (GlobalDataAnalysis ana) prt mod fdecls =
    getAllTypes gstate prt mod >>= \types ->
    getAllFunctions gstate prt mod >>= \funcs ->
    prt "Analyzing..." >>
    let anaresults = ana types funcs in
    return (map (\fd->fromJust (lookup (funcName fd) anaresults)) fdecls)


---------------------------------------------------------------------
-- find a function declaration in a program text:
findFunDeclInProgText :: ContentsKind -> String -> QName -> Int
findFunDeclInProgText CurryProg progtext fname =
  findFirstDeclLine (showCurryId (snd fname)) (lines progtext) 1
findFunDeclInProgText LCurryProg progtext fname =
  findFirstDeclLine ("> "++showCurryId (snd fname)) (lines progtext) 1
findFunDeclInProgText FlatCurryExp progtext fname =
  findFirstDeclLine ("  (Func (\""++fst fname++"\",\""++snd fname++"\")")
                    (lines progtext) 1
findFunDeclInProgText OtherText _ _ = 0

-- finds first declaration line:
findFirstDeclLine _ [] _ = 0 -- not found
findFirstDeclLine f (l:ls) n =
     if isPrefixOf f l then n else findFirstDeclLine f ls (n+1)

---------------------------------------------------------------------
-- directory with browser sources:
browserDir = installDir++"/tools/browser"

-- order qualified names by basename first:
leqQName (mod1,n1) (mod2,n2) = leqString n1 n2 || n1==n2 && leqString mod1 mod2

-- show qualified name with module:
showQNameWithMod (m,n) = n++" ("++m++")"

noAnalysisText = "*** no analysis ***"

---------------------------------------------------------------------
-- Get a string in a GUI box and process this input:
getAnswer :: String -> String -> (String -> IO ()) -> IO ()
getAnswer question initial processinput = do
   runInitGUI "" (Col []
                      [Label [Text question],
                       Entry [Text initial, WRef entry, Cmd getinput, FillX,
                              Background "white", Width 50],
                       Button getinput [Text "Ok"]])
                 (focusInput entry)
  where
   entry free

   getinput wp = do inp <- getValue entry wp
                    processinput inp
                    exitGUI wp

---------------------------------------------------------------------
