------------------------------------------------------------------------
--- A simple command-based manager for CGI servers.
--- 
--- @author Michael Hanus
--- @version March 2012
------------------------------------------------------------------------

import ReadShowTerm
import System
import IOExts
import HtmlCgi
import Directory(doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["show"]     -> showAllActiveServers
    ["showload"] -> cmdForAllServers "Show load of "     GetLoad
    ["sketch" ]  -> cmdForAllServers "Sketch status of " SketchHandlers
    ["showall"]  -> cmdForAllServers "Status of "        ShowStatus
    ["clean"]    -> cmdForAllServers "Clean status of "  CleanServer >>
                    getAndCleanRegistry >> done
    ["stop"]     -> cmdForAllServers "Stopping cgi server " StopCgiServer >>
                    getAndCleanRegistry >> done
    ["kill"]     -> doForAllServers "Killing process of cgi server "
                              (\(pid,_,_) -> system ("kill -9 "++show pid)) >>
                    getAndCleanRegistry >> done
    ["stopscript",scriptprog] -> stopActiveScriptServers scriptprog
    _            -> putStrLn $ "ERROR: Illegal arguments!"

showAllActiveServers :: IO ()
showAllActiveServers = do
  let header = "Currently active cgi script servers:"
  putStrLn header
  putStrLn (take (length header) (repeat '='))
  doForAllServers "" (const done)

--- Stops the active servers for a particular cgi script by sending them
--- a stop message. This operation is used by the installation script
--- "makecurrycgi" to terminate old versions of a server.
stopActiveScriptServers :: String -> IO ()
stopActiveScriptServers scriptprog = do
  regs <- getAndCleanRegistry
  putStrLn $ "Stop active servers for cgi script: " ++ scriptprog
  mapIO_ stopServer regs
 where
  stopServer (_,progname,port) =
    if progname==scriptprog
    then do putStrLn $ "...on port: " ++ port
            runCgiServerCmd port StopCgiServer
    else done

doForAllServers :: String -> ((Int,String,String) -> IO _) -> IO ()
doForAllServers cmt action = do
  regs <- getAndCleanRegistry
  mapIO_ doForServer regs
 where
  doForServer (pid,progname,port) = do
    putStrLn $ cmt ++ progname++":\n(pid: "++show pid++", port: "++port++")"
    catch (action (pid,progname,port) >> done) (const done)

cmdForAllServers :: String -> CgiServerMsg -> IO ()
cmdForAllServers cmt servercmd =
  doForAllServers
    cmt
    (\ (_,_,port) -> catch (runCgiServerCmd port servercmd) (const done))

-- Get the registry with active processes and clean up the registry file.
getAndCleanRegistry :: IO [(Int,String,String)]
getAndCleanRegistry = exclusiveIO (cgiServerRegistry++".lock") $ do
  regexists <- doesFileExist cgiServerRegistry
  regs <- if regexists then readQTermListFile cgiServerRegistry
                       else return []
  aregs <- mapIO (\ (pid,pname,port) -> isActivePID pid >>= \pidactive ->
                   return (if pidactive then [(pid,pname,port)] else [])) regs
  let cregs = concat aregs
  if cregs==regs
   then done
   else writeFile cgiServerRegistry (concatMap (\reg->show reg++"\n") cregs)
  return cregs

-- Is an integer the pid of an existing process?
isActivePID :: Int -> IO Bool
isActivePID pid = do
  mypid <- getPID
  let tmp = "/tmp/tmp_pakcs_registry_"++show mypid
  system ("ps -p "++show pid++" | fgrep "++show pid++" > "++tmp)
  pr <- readCompleteFile tmp
  system ("rm "++tmp)
  return (not (null pr))
