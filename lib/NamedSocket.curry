------------------------------------------------------------------------------
--- Library to support network programming with sockets that are addressed
--- by symbolic names. In contrast to raw sockets (see library
--- <code>Socket</code>), this library uses the Curry Port Name Server
--- to provide sockets that are addressed by symbolic names
--- rather than numbers.
---
--- In standard applications, the server side uses the operations
--- <code>listenOn</code> and <code>socketAccept</code> to provide some service
--- on a named socket, and the client side uses the operation
--- <code>connectToSocket</code> to request a service.
---
--- @author Michael Hanus
--- @version February 2008
------------------------------------------------------------------------------

module NamedSocket(Socket,
                   listenOn, socketAccept, waitForSocketAccept,
                   connectToSocketRepeat, connectToSocketWait,
                   sClose, socketName, connectToSocket)
 where

import System
import IO(Handle)
import qualified Socket
import CPNS

---------------------------------------------------------------------
-- Server side operations:

--- Abstract type for named sockets.
data Socket = NamedSocket String Socket.Socket

--- Creates a server side socket with a symbolic name.
listenOn :: String -> IO Socket
listenOn socketname = do
  (port,socket) <- Socket.listenOnFresh
  registerPort socketname port 0
  return (NamedSocket socketname socket)

--- Returns a connection of a client to a socket.
--- The connection is returned as a pair consisting of a string identifying
--- the client (the format of this string is implementation-dependent)
--- and a handle to a stream communication with the client.
--- The handle is both readable and writable.
socketAccept :: Socket -> IO (String,Handle)
socketAccept (NamedSocket _ socket) = Socket.socketAccept socket

--- Waits until a connection of a client to a socket is available.
--- If no connection is available within the time limit, it returns Nothing,
--- otherwise the connection is returned as a pair consisting
--- of a string identifying the client
--- (the format of this string is implementation-dependent)
--- and a handle to a stream communication with the client.
--- @param socket - a socket
--- @param timeout - milliseconds to wait for input (< 0 : no time out)
waitForSocketAccept :: Socket -> Int -> IO (Maybe (String,Handle))
waitForSocketAccept (NamedSocket _ socket) = Socket.waitForSocketAccept socket

--- Closes a server socket.
sClose :: Socket -> IO ()
sClose (NamedSocket socketname socket) = do
  Socket.sClose socket
  unregisterPort socketname

--- Returns a the symbolic name of a named socket.
socketName :: Socket -> String
socketName (NamedSocket socketname _) = socketname

---------------------------------------------------------------------
-- Client side operations:

--- Waits for connection to a Unix socket with a symbolic name.
--- In contrast to <code>connectToSocket</code>, this action waits until
--- the socket has been registered with its symbolic name.
--- @param waittime - the time to wait before retrying (in milliseconds)
--- @param action   - I/O action to be executed before each wait cycle
--- @param retries  - number of retries before giving up (-1 = retry forever)
--- @param nameAtHost - the symbolic name of the socket
---                     (must be either of the form "name@host" or "name"
---                      where the latter is a shorthand for "name@localhost")
--- @return Nothing (if connection is not possible within the given limits)
---         or (Just h) where h is the handle of the connection
connectToSocketRepeat :: Int -> IO _ -> Int -> String -> IO (Maybe Handle)
connectToSocketRepeat waittime action retries nameAtHost = do
  let (name,atHost) = break (=='@') nameAtHost
      host = if atHost=="" then "localhost" else tail atHost
  -- check whether remote CPNS demon is alive:
  alive <- cpnsAlive waittime host
  if not alive
    then tryAgain 
    else do -- get remote socket/port numbers:
      (snr,_) <- getPortInfo name host
      if snr==0
       then tryAgain
       else Socket.connectToSocket host snr >>= return . Just
 where
  tryAgain = if retries==0 then return Nothing else do
    action
    sleep (ms2s waittime)
    connectToSocketRepeat waittime action (decr retries) nameAtHost

  ms2s n = let mn = n `div` 1000 in if mn==0 then 1 else mn

  decr n = if n<0 then n else n-1


--- Waits for connection to a Unix socket with a symbolic name and
--- return the handle of the connection.
--- This action waits (possibly forever) until the socket with the symbolic
--- name is registered.
--- @param nameAtHost - the symbolic name of the socket
---                     (must be either of the form "name@host" or "name"
---                      where the latter is a shorthand for "name@localhost")
--- @return the handle of the connection (connected to the socket nameAtHost)
---         which is both readable and writable
connectToSocketWait :: String -> IO Handle
connectToSocketWait nameAtHost = do
  Just hdl <- connectToSocketRepeat 1000 done (-1) nameAtHost
  return hdl

--- Creates a new connection to an existing(!) Unix socket with a symbolic
--- name. If the symbolic name is not registered, an error is reported.
--- @param nameAtHost - the symbolic name of the socket
---                     (must be either of the form "name@host" or "name"
---                      where the latter is a shorthand for "name@localhost")
--- @return the handle of the stream (connected to the socket nameAtHost)
---         which is both readable and writable
connectToSocket :: String -> IO Handle
connectToSocket nameAtHost = do
  let (name,atHost) = break (=='@') nameAtHost
      host = if atHost=="" then "localhost" else tail atHost
  -- get remote port number:
  (snr,_) <- getPortInfo name host
  if snr==0
    then error ("connectToSocket: Socket \""++name++"@"++host++
                "\" is not registered!")
    else done
  Socket.connectToSocket host snr


---------------------------------------------------------------------
