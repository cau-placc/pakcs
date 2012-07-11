------------------------------------------------------------------------------
--- Implementation of a Curry Port Name Server based on raw sockets.
--- It is used to implement the library Ports for distributed programming
--- with ports.
---
--- @author Michael Hanus
--- @version January 2012
------------------------------------------------------------------------------

module CPNS(registerPort,getPortInfo,unregisterPort,
            cpnsStart,cpnsStop,cpnsShow,cpnsAlive,main) where

import Socket
import IO
import ReadShowTerm
import Char
import List(delete,intersperse)
import Time
import System
import Distribution(installDir)
import Profile

-- If we connect to a port with symbolic name pn, we first connect
-- to the CPNS of the host named by pn to get the physical socket
-- number of this port. In order to connect to CPNS from any
-- machine in the world, the CPNS demon always listens at the following
-- port:
-- (Note that this must be identical for all machines running
-- Distributed Curry! If this port is occupied by another process
-- on a host, you cannot run Distributed Curry on it.)

cpnsSocket = 8767  -- standard port number of CPNS demon


-- The time out before considering the server as unreachable:
cpnsTimeOut = 3000

--- Type of messages to be processed by the Curry Port Name Server.
---
--- @cons Register name pid sn pn ack
---       -  assign the values pid, sn, and pn to name
---          (pid is the process number of the registered process
---           (should be 0 if it is unknown); the server returns True
---           if registration had no problems, otherwise False)
--- @cons GetRegister name - request for a registered port name;
---         the server returns the values (sn,pn) that are assigned to the
---         port name
--- @cons Unregister name - request to remove a registered port name
--- @cons ShowRegistry - show the current port registrations
--- @cons Ping         - ping the CPNS demon for liveness check
--- @cons Terminate    - terminate the CPNS demon
data CPNSMessage = Terminate
                 | Ping
                 | Register String Int Int Int
                 | GetRegister String
                 | Unregister String
                 | ShowRegistry

-- The lock file to coordinate the startup of the CPNS demon:
cpnsStartupLockfile = "/tmp/CurryPNSD.lock"

--- Starts the "Curry Port Name Server" (CPNS) running on the local machine.
--- The CPNS is responsible to resolve symbolic names for ports
--- into physical socket numbers so that a port can be reached
--- under its symbolic name from any machine in the world.

cpnsStart :: IO ()
cpnsStart = catch startup 
                  (\_ -> putStrLn "FAILURE occurred during startup!" >> 
                         deleteStartupLockfile >> 
                         return Nothing) >>= 
             maybe done (cpnsServer []) 
 where
   deleteStartupLockfile = do
     putStrLn $ "Removing startup lock file \""++cpnsStartupLockfile++"\"..."
     system $ "rm -f "++cpnsStartupLockfile
     done

   startup = do
     putStrLn $ "Starting Curry Port Name Server on port " ++
                show cpnsSocket ++ "..."
     socket <- listenOn cpnsSocket
     deleteStartupLockfile
     pid <- getPID
     putStrLn $ "Curry Port Name Server is ready (PID: "++show pid++")."
     return (Just socket) 

--- The main loop of the CPNS demon
cpnsServer :: [(String,Int,Int,Int)] -> Socket -> IO ()
cpnsServer regs socket = do
  (chost,stream) <- socketAccept socket
  --putStrLn $ "Connection from "++chost
  serveRequest chost stream
 where
   doIfLocalHost rhost action = do
     hostname <- getHostname
     if rhost `elem` ["localhost","localhost.localdomain",hostname]
        || take 8 rhost == "127.0.0."
      then action
      else do putStrLn $ "Illegal request received from host: " ++ rhost
              cpnsServer regs socket

   serveRequest rhost h = do
     msg <- readMsgLine h
     either
      (\line -> do putStrLn $ "ERROR: Illegal message:\n" ++ line
                   cpnsServer regs socket )
      (\m -> case m of
        Terminate -> doIfLocalHost rhost $ do
          hClose h
          putStrLn "CPNS demon terminated."
        Ping -> do
          hPutStrLn h (showQTerm ())
          hClose h
          cpnsServer regs socket
        Register pname pid sn pn -> doIfLocalHost rhost $ do
          (ack, newregs) <- tryRegisterPortName regs pname pid sn pn
          hPutStrLn h (showQTerm ack)
          hClose h
          cpnsServer newregs socket
        GetRegister pname -> do
          --putStrLn $ "Request for port name: " ++ pname
          (newregs,pinfo) <- getRegisteredPortName regs pname
          hPutStrLn h (showQTerm pinfo)
          hClose h
          cpnsServer newregs socket
        Unregister pname -> doIfLocalHost rhost $ do
          newregs <- unregisterPortName regs pname
          hClose h
          cpnsServer newregs socket
        ShowRegistry -> doIfLocalHost rhost $ do
          putStrLn "Currently registered port names:"
          newregs <- showAndCleanRegs regs
          hFlush stdout
          hClose h
          cpnsServer newregs socket )
      msg

tryRegisterPortName regs name pid sn pn = do
  let nameregs = filter (\(n,_,_,_)->name==n) regs
  ack <- if null nameregs
         then return True
         else let (_,pid',_,_) = head nameregs in
              if pid'>0 && pid'/=pid
              -- we allow registration from the same process
              then doesProcessExists pid' >>= \pex -> return (not pex)
              else return True
  ctime <- getLocalTime
  putStrLn $ "Register port \""++name++"\": pid "++show pid++
             " / socket "++show sn++
             " / number "++show pn ++ " at " ++ calendarTimeToString ctime
  let newregs = (name,pid,sn,pn) : filter (\ (n,_,_,_)->name/=n) regs
  printMemInfo newregs
  hFlush stdout
  return (ack, newregs)

-- Delete all registrations for a given port name:
unregisterPortName regs name = do
  ctime <- getLocalTime
  putStrLn $ "Unregister port \""++name++"\" at "++calendarTimeToString ctime
  let newregs = filter (\ (n,_,_,_)->name/=n) regs
  printMemInfo newregs
  hFlush stdout
  return newregs

-- Get the socket number for a registered port name
-- (or (0,0) if not registered).
-- In addition, a new registration list is returned where a registration
-- is deleted if the corresponding server process does not exist.
getRegisteredPortName :: [(String,Int,Int,Int)] -> String
                      -> IO ([(String,Int,Int,Int)],(Int,Int))
getRegisteredPortName regs pname =
  let nameregs = filter (\(n,_,_,_)->pname==n) regs in
  if null nameregs
  then return (regs,(0,0))
  else let (_,pid,sn,pn) = head nameregs in
       if pid>0
       then doesProcessExists pid >>= \pex ->
            if pex
            then return (regs,(sn,pn))
            else --putStrLn ("WARNING: Process "++show pid++" not running!") >>
                 return (delete (head nameregs) regs, (0,0))
       else return (regs,(sn,pn))

-- Show all registered ports and return a new registration list
-- where a registration is deleted if the corresponding server process
-- does not exist.
showAndCleanRegs :: [(String,Int,Int,Int)] -> IO [(String,Int,Int,Int)]
showAndCleanRegs regs = do
  newreglist <- mapIO checkAndShow regs
  return (concat newreglist)
 where
  checkAndShow reg@(n,pid,sn,pn) = do
    pidexist <- doesProcessExists pid
    if pidexist
     then do putStrLn $ n++": pid "++show pid++
                        " / socket "++show sn++" / number "++show pn
             return [reg]
     else return []

-- Print status information of current CPNS demon process:
printMemInfo :: [(String,Int,Int,Int)] -> IO ()
printMemInfo regs = do
  pinfos <- getProcessInfos
  putStrLn ("NumRegs: " ++ show (length regs) ++ ", " ++ showMemInfo pinfos)

-- test whether a process with a given pid is running:
doesProcessExists :: Int -> IO Bool
doesProcessExists pid = do
  status <- system("test -z \"`ps -p "++show pid++" | fgrep "++show pid++"`\"")
  return (status>0)

-- Read a line from a stream and check syntactical correctness of message:
readMsgLine :: Handle -> IO (Either String a)
readMsgLine handle = do
  line <- hGetLine handle
  case readsQTerm line of
     [(msg,rem)] -> return (if all isSpace rem then Right msg else Left line)
     _ -> return (Left line)

-- Perform an action if the CPNS demon at a given host is alive:
doIfAlive :: String -> IO a -> IO a
doIfAlive host action = do
  alive <- cpnsAlive cpnsTimeOut host
  if not alive
   then error $ "Curry port name server at host \""++host++
                "\" is not reachable (timeout after "++show cpnsTimeOut++
                " ms)!"
   else action

sendToLocalCPNS :: CPNSMessage -> IO ()
sendToLocalCPNS msg = doIfAlive "localhost" $ do
  h <- connectToSocket "localhost" cpnsSocket
  hPutStrLn h (showQTerm msg)
  hClose h

--- Shows all registered ports at the local CPNS demon (in its logfile).
cpnsShow = sendToLocalCPNS ShowRegistry

--- Terminates the local CPNS demon
cpnsStop = sendToLocalCPNS Terminate

--- Gets an answer from a Curry port name server on a host,
--- or reports an error.
cpnsTryGetAnswer :: String -> CPNSMessage -> IO _
cpnsTryGetAnswer host msg = catch tryGetAnswer connectError
 where
  tryGetAnswer = do
    h <- connectToSocket host cpnsSocket
    hPutStrLn h (showQTerm msg)
    hFlush h
    ready <- hWaitForInput h cpnsTimeOut
    if ready
     then do
       answer <- readMsgLine h
       hClose h
       either (\line -> error $ "cpnsTryGetAnswer: Illegal answer: " ++ line)
              return
              answer
     else failed

  connectError _ = error $ "Curry port name server at host \""++host++
                           "\" is not reachable!"

--- Registers a symbolic port at the local host.
registerPort :: String -> Int -> Int -> IO ()
registerPort pname sn pn = do
  startCPNSDIfNecessary
  pid <- getPID
  ack <- cpnsTryGetAnswer "localhost" (Register pname pid sn pn)
  if ack then done
         else putStrLn ("WARNING: Port name '"++pname++"' already registered!")

--- Gets the information about a symbolic port at some host.
getPortInfo :: String -> String -> IO (Int,Int)
getPortInfo pname host = cpnsTryGetAnswer host (GetRegister pname)

--- Unregisters a symbolic port at the local host.
unregisterPort :: String -> IO ()
unregisterPort pname = sendToLocalCPNS (Unregister pname)

--- Tests whether the CPNS demon at a host is alive.
cpnsAlive :: Int -> String -> IO Bool
cpnsAlive timeout host = catch tryPingCPNS (\_ -> return False)
 where
  tryPingCPNS = do
    h <- connectToSocket host cpnsSocket
    hPutStrLn h (showQTerm Ping)
    hFlush h
    answer <- hWaitForInput h timeout
    hClose h
    return answer

--- Starts the CPNS demon at localhost if it is not already running:
startCPNSDIfNecessary :: IO ()
startCPNSDIfNecessary = do
  system $ "\""++installDir++"/cpns/start\""
  done

--- Main function for CPNS demon. Check arguments and execute command.
main = do
  args <- getArgs
  case args of
   ["start"] -> cpnsStart
   ["stop"]  -> cpnsStop
   ["show"]  -> cpnsShow
   _ -> putStrLn $ "ERROR: Illegal arguments: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Allowed arguments: start|stop|show"

{-
Test with PAKCS:

:fork cpnsStart
registerPort "xxx" 42 2
getPortInfo "xxx" "localhost"
cpnsStop

-}
