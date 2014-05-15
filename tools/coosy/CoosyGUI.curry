------------------------------------------------------------------------------
-- GUI for showing the result of observing objects in Curry programs.
------------------------------------------------------------------------------

import Observe(clearLogFile)
import CoosyShowObserve(readAndPrintEvents,ViewConf(..))
import CoosyTrace(logDir,logFileClear)
import CoosyDerive(deriveFile)
import GUI
import System
import Directory

------------------------------------------------------------------------------

main = do
  logexist <- doesDirectoryExist logDir
  if logexist
    then done
    else do
      putStrLn $ ">>> Creating new directory '"++logDir++"' for Coosy log files"
      createDirectory logDir
      writeFile logFileClear ""
  writeFile (logDir++"/READY") "" -- for synchronization with PAKCS
  args <- getArgs
  runGUI "COOSy" (addlineGUI (head args))

-- The COOSy GUI (parameter: home dir of COOSy to find the help files):
addlineGUI coosyhome =
 Col [] [
   Label [Text "Curry Object Observation System",
          Background "blue", Foreground "white", FillX],
   Row [CenterAlign]
    [Label [FillX],
     Button clearTrace [Text "Clear"],
     Button (showBusy showObserve) [Text "Show"],
     CheckButton [Text "show bound logical variables",
                  CheckInit "1",
                  WRef logVarCheck,
                  Cmd showObserve],
     Label [FillX],
     Button (showBusy addObservers) [Text "Add observers"],
     Button exitGUI [Text "Exit"],
     MenuButton
       [Text "Infos...",
        Menu [MButton help  "How to use COOSy",
              MButton about "About COOSy"]]],
   TextEditScroll [WRef rtxt, Height 40, Text initMsg, Background "white"],
   Label [WRef status, Text "Status: ready", Background "green", FillX]
   ]
 where
   rtxt,status,logVarCheck free

   clearTrace wp = do
     clearLogFile
     writeFile logFileClear "1"
     setValue rtxt "Logfiles cleared." wp

   showObserve wp = do
     setValue rtxt "" wp
     logVarSel <- getValue logVarCheck wp
     catch (readAndPrintEvents (\s -> appendValues rtxt wp s)
                               (toViewConf logVarSel))
           (\e -> putStrLn (showError e) >> appendValue rtxt failMsg wp)

   addObservers wp = do
     filename <- getOpenFileWithTypes curryFileTypes
     if null filename
       then done
       else do
         msg <- catch (deriveFile filename)
                      (\e -> return ("Error occurred: " ++ showError e))
         setValue rtxt msg wp

   showBusy handler wp = do
     setValue status "Status: running" wp
     setConfig status (Background "red") wp
     handler wp
     setValue status "Status: waiting" wp
     setConfig status (Background "green") wp

   help wp = do
     helptext <- readFile (coosyhome++"/Help.txt")
     setValue rtxt helptext wp
     return []

   about wp = do
     helptext <- readFile (coosyhome++"/README.txt")
     setValue rtxt helptext wp
     return []

appendValues _ _ [] = done
appendValues rtxt wp (s:ss) 
  = if elem (chr 7) (s:ss) then appendGray rtxt wp (s:ss)
      else appendStyledValue rtxt (s:ss) [Fg Black] wp 

appendGray _ _ [] = done
appendGray rtxt wp (s:ss) 
  = appendStyledValue rtxt gray [Fg Gray] wp >> appendBlack rtxt wp rest
  where
    (gray,rest) = span (/= (chr 7)) (s:ss)

appendBlack _ _ [] = done
appendBlack rtxt wp (_:ss) = appendValue rtxt black wp 
                             >> appendGray rtxt wp rest
  where
    (black,_:rest) = span (/= (chr 7)) ss

toViewConf :: String -> ViewConf
toViewConf "1" = ShowLogVarBinds
toViewConf "0" = HideLogVarBinds

-- Curry file types:
curryFileTypes = [("Curry Files",".curry"),
                  ("Literate Curry files",".lcurry")]


initMsg =
   "IMPORTANT NOTE:\n\n" ++
   "Don't forget to press 'clear' before you observe a new program execution!"

failMsg =
   "Failure occurred during reading of trace file!\n\n"++
   "Press 'clear' button and run again your program."

------------------------------------------------------------------------------
