---------------------------------------------------------------------
-- A protocol GUI for the CurryTest tool:
--
-- The GUI process messages sent by running tests with the test tool
-- and summarizes the results in a GUI.
--
-- If the currytest tool is executed in batch mode, the return code is
-- positive if some error occurred.
--
-- @author Michael Hanus
-- @version June 2012
---------------------------------------------------------------------

import Socket
import ReadShowTerm
import GUI
import Read
import Assertion
import System
import List
import FlatCurry
import IO
import IOExts -- use IORefs
import FileGoodies(stripSuffix)
import Distribution
import Directory

---------------------------------------------------------------------
-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
   "--window":modnames -> startGUI modnames
   "-w":modnames -> startGUI modnames
   "-window":modnames -> startGUI modnames
   ["-f",portnums] -> forwardMessages (readNat portnums)
   a:as -> do rcs <- mapIO (testModule putStr 0) (map stripSuffix (a:as))
              let ecode = foldr (+) 0 rcs
              if ecode==0 then done
               else putStrLn "FAILURE IN SOME TEST OCCURRED!!!"
              exitWith ecode
   _ -> do putStrLn $ "ERROR: Illegal arguments for currytest: " ++
                      concat (intersperse " " args) ++ "\n" ++
                      "Usage: currytest [--window|-w] <module_names>"
           exitWith 1

-- This operation creates a new socket to receive messages that are forwarded
-- to a continues connection to a socket with the argument port number:
forwardMessages guiportnum = do
  (portnum,socket) <- listenOnFresh
  putStrLn $ "Forwarding messages from port "++show portnum++
             " to port "++show guiportnum++"..."
  guihandle <- connectToSocket "localhost" guiportnum
  hPutStrLn guihandle (show portnum) -- first message is my port number
  hFlush guihandle
  acceptAndForwardMessage guihandle socket
 where
   acceptAndForwardMessage guihandle socket = do
     (_,h) <- socketAccept socket
     str <- hGetLine h
     hClose h
     --putStrLn ("MESSAGE: "++str)
     if str=="TERMINATE" then sClose socket else do
       hPutStrLn guihandle str
       hFlush guihandle
       acceptAndForwardMessage guihandle socket

terminateForwardMessages portnum = do
  h <- connectToSocket "localhost" portnum
  hPutStrLn h "TERMINATE"
  hClose h
   
startGUI modnames = do
  (guiportnum,socket) <- listenOnFresh
  system (installDir++"/bin/currytest -f "++show guiportnum++" &")
  (_,inhandle) <- socketAccept socket
  portnums <- hGetLine inhandle
  let portnum = readNat portnums
  --putStrLn ("Using port number: "++show portnum)
  stateref <- newIORef (0,[])
  mapIO_ (addModule stateref) (map stripSuffix modnames)
  mods <- getModules stateref
  runHandlesControlledGUI "CurryTest Tool"
                          (protocolGUI portnum mods stateref) [inhandle]
  terminateForwardMessages portnum

-- A text edit widget with vertical scrollbar.
TextEditVScroll confs =
   Row []
     [TextEdit ([WRef txtref, Fill]++confs),
      ScrollV txtref [FillY]]     where txtref free

---------------------------------------------------------------------
-- Functions to manipulate the GUI state (pair of module name list and
-- module index):

-- add a module name:
addModule ref modname = do
  (_,mods) <- readIORef ref
  writeIORef ref (length mods+1,mods++[modname])

-- delete all modules:
deleteModules ref = writeIORef ref (0,[])

-- get list of modules as string representation:
getModules ref = do
  (i,mods) <- readIORef ref
  return (concatMap (\ (c,n)->c++n++"\n")
                    (zip (replace "==>" i (repeat "   ")) mods))

-- intialize module index for testing:
initModuleIndex ref = do
  (_,mods) <- readIORef ref
  writeIORef ref (0,mods)

-- get next module to be tested (or Nothing):
getNextTestModule ref = do
  (i,mods) <- readIORef ref
  if i >= length mods
   then return Nothing
   else writeIORef ref (i+1,mods) >> return (Just (mods!!i))

-- get name of current test module:
getCurrentTestModule ref = do
  (i,mods) <- readIORef ref
  return (mods!!(i-1))


---------------------------------------------------------------------
-- The definition of the protocol GUI together with a handler
-- "ext_handler" that is responsible to handle the external messages
-- sent during running the test on the program:
protocolGUI portnum initmods stateref =
 (Row []
   [Col [LeftAlign]
     [Row [] [Label [Text "Test cases:"],
                Entry [WRef rtestnum, Text "0", Width 5],
                Label [Text "Failures:"],
                Entry [WRef rfailnum, Text "0", Width 5,
                       Background "green"],
                Label [Text "Status:"],
                Entry [WRef rstatus, Text "ready", Width 10,
                       Background "green"]],
      Label [Text "Failed test cases:"],
      TextEditScroll [WRef rfail, Height 10, Width 60,
                      Background "green"],
      Label [Text "Test protocol:"],
      TextEditScroll [WRef rprot, Height 15, Width 60,
                        Background "white"]],
    Col [LeftAlign]
     [Row [LeftAlign] [Button starttest [Text "Run test"],
                       Button openfile  [Text "Add test module"],
                       Button delete    [Text "Clear test modules"],
                       Button exitGUI   [Text "Exit"]],
      Label [Text "Modules to be tested:"],
      TextEditVScroll [WRef rmods, Height 10, Width 60,
                         Text initmods, Background "white"],
      Label [Text "Compilation messages:"],
      TextEditScroll [WRef rcmsgs, Height 15, Width 60,
                        Background "white"]]
   ], [ext_handler])
 where
   rtestnum,rfailnum,rstatus,rfail,rprot,rcmsgs,rmods free

   openfile gp =
      getOpenFileWithTypes curryFileTypes >>= \filename ->
      if null filename then done else
      addModule stateref (stripSuffix filename) >>
      showModules gp

   delete gp = do
      deleteModules stateref
      showModules gp

   starttest gp = do
     setConfig rfailnum (Background "green") gp
     setConfig rfail (Background "green") gp
     setValue rtestnum "0" gp
     setValue rfailnum "0" gp
     setValue rfail "" gp
     setValue rprot "" gp
     setValue rcmsgs "" gp
     setConfig rstatus (Background "yellow") gp
     setValue rstatus "testing" gp
     initModuleIndex stateref
     startTestModule gp

   -- test a module, if present:
   startTestModule gp =
     showModules gp >>
     getNextTestModule stateref >>=  \nextmod ->
     maybe (setValue rstatus "ready" gp >>
            setConfig rstatus (Background "green") gp)
           (\m -> testModule (printCompMsg gp) portnum m >> done)
           nextmod

   -- print a compilation message in corresponding widget:
   printCompMsg gp msg = appendValue rcmsgs msg gp

   -- update list of modules in widget:
   showModules gp = do
     mods <- getModules stateref
     setValue rmods mods gp
     (row,_) <- readIORef stateref
     seeText rmods (row,1) gp

   ext_handler h gp = do
     msgstring <- hGetLine h
     processTestMsg (readUnqualifiedTerm ["Assertion","Prelude"] msgstring) gp

   processTestMsg (TestModule m) gp =
      appendValue rprot
                  (take 60 (repeat '=')++"\nTesting module: "++m++"\n") gp

   processTestMsg TestFinished gp = startTestModule gp

   processTestMsg TestCompileError gp = do
     setConfig rfailnum (Background "red") gp
     setConfig rfail (Background "red") gp
     updateValue incrText rfailnum gp
     mod <- getCurrentTestModule stateref
     appendValue rfail ("Compilation error in module: "++mod++"\n") gp
     startTestModule gp

   processTestMsg (TestCase s b) gp = do
      updateValue incrText rtestnum gp
      if b then done
           else do setConfig rfailnum (Background "red") gp
                   setConfig rfail (Background "red") gp
                   updateValue incrText rfailnum gp
                   mod <- getCurrentTestModule stateref
                   appendValue rfail ("Module: "++mod++"\n") gp
                   appendValue rfail (s++"\n") gp
      appendValue rprot s gp

-- Curry file types:
curryFileTypes = [("Curry Files",".curry"),
                  ("Literate Curry files",".lcurry")]

-- increment number text string:
incrText s = show (readInt s + 1)


---------------------------------------------------------------------------
-- Main function to test a module:
-- Arg 1: function for printing compilation messages
-- Arg 2: port number of socket where the test messages are sent
--        (or 0 if no gui defined)
-- Arg 3: module name
-- The return code is positive if some tests failed and the GUI is not used
testModule :: (String -> IO _) -> Int -> String -> IO Int
testModule prtmsg portnum modname = do
  prtmsg ("Loading module \""++modname++"\"...\n")
  prog_or_error <- tryReadFlatCurry modname
  testModuleIfPossible prtmsg portnum modname prog_or_error

testModuleIfPossible prtmsg portnum _ (Right errmsg) = do
  prtmsg ("ERROR: compilation not successful:\n\n"++errmsg++"\n")
  if portnum==0 then done else showTestCompileError portnum
  return 1
testModuleIfPossible prtmsg portnum modname (Left prog) =
  execTestFunctions prtmsg portnum modname (getTestFunctionNames prog)

execTestFunctions prtmsg portnum _ [] = do
  prtmsg "No test functions found.\n\n"
  if portnum==0 then done else showTestEnd portnum
  return 0
execTestFunctions prtmsg portnum modname fs@(_:_) = do
  prtmsg ("Exported top-level test functions:\n"
            ++ concatMap (++" ") fs ++ "\n\n")
  if portnum/=0 then showTestMod portnum modname else done
  let testgoal =
         "putStrLn (take 60 (repeat (chr 61))) >> " ++
         "putStrLn (\"Testing module \\\""++modname++"\\\"...\") >> " ++
         concat
           (intersperse " `seqStrActions` "
             (map (\f -> "checkAssertion \"" ++ f ++ "\" " ++
                         (if portnum/=0 then "(showTestCase "++show portnum++") "
                                        else "return ") ++ f)
                  fs)) ++
         " >>= writeAssertResult " ++
         (if portnum/=0 then " >> showTestEnd "++show portnum
                        else " >>= exitWith")
      execCall = "echo ':l "++modname++"\n"++testgoal++" ' | \"" ++
                 installDir++"/bin/pakcs\" -quiet -Dshowfcyload=no 2>&1"
  --putStrLn testgoal
  if portnum==0 then system execCall
                else system (execCall ++ " &") >> return 0


-- Extract all test functions from a module:
getTestFunctionNames (Prog _ _ _ funs _) =
   map funcname . filter isExportedFunc . filter hasAssertType $ funs
 where
   isExportedFunc (Func _ _ vis _ _) = vis==Public

   funcname (Func (_,fname) _ _ _ _) = fname


hasAssertType (Func _ _ _ texp _) =
 case texp of
   TCons tc _ -> tc==("Assertion","Assertion")
   _          -> False


-- Tries to read a FlatCurry program.
-- Returns either (Left prog) (if reading was successful)
-- or (Right msg) where msg is the string of error messages from the parser
tryReadFlatCurry :: String -> IO (Either Prog String)
tryReadFlatCurry mname = do
  pofile <- getPOFilename
  callFrontendWithParams FCY (setLogfile pofile defaultParams) mname
  exfcy <- doesFileExist (flatCurryFileName mname)
  -- check whether parsing was ok and return appropriate value:
  if exfcy
   then
     do prog <- readFlatCurryFile (flatCurryFileName mname)
        system ("rm "++pofile)
        return (Left prog)
   else
     do msgs <- readFile pofile
        system ("rm "++pofile)
        return (Right msgs)
 where
   -- compute name for auxiliary file for parser outputs:
   getPOFilename =
     do pid <- getPID
        return ("/tmp/pakcsoutput_"++show pid)
