------------------------------------------------------------------------------
--- Library for distributed programming with ports.
--- <a href="http://www.informatik.uni-kiel.de/~mh/papers/PPDP99.html">
--- This paper</a> contains a description of the basic ideas
--- behind this library.
---
--- @author Michael Hanus
--- @version April 2007
------------------------------------------------------------------------------

module Ports(Port,openPort,send,doSend,openNamedPort,
             connectPort,connectPortRepeat,connectPortWait,
             ping,timeoutOnStream,
             openProcessPort,SP_Msg(..),choiceSPEP,
             newObject,newNamedObject,runNamedServer ) where

import Time
import System(system,sleep,getPID)
import CPNS

--- The internal constructor for the port datatype is not visible to the user.

data Port a = internalPort String Int Int a


--- Opens an internal port for communication.
--- @param p - a free variable which will be constrained
---            with the port messages
--- @param s - a free variable which will be instantiated
---            to the stream of incoming messages
openPort    :: Port a -> [a] -> Success
openPort p ms = (prim_openPort $! p) $!! ms

prim_openPort :: Port a -> [a] -> Success
prim_openPort external


--- Sends a message to a port.
send        :: a -> Port a -> Success
send msg p = (prim_send $!! ensureNotFree msg) $# p

prim_send :: a -> Port a -> Success
prim_send external

--- I/O action that sends a message to a port.
doSend       :: a -> Port a -> IO ()
doSend msg p = doSolve (send msg p)


--- A constrained which is satisfied after some amount of time
--- (currently only supported in TasteCurry).
--- @param n - the satisfaction time in milliseconds
after       :: Int -> Success
after external

--- Checks whether port p is still reachable.
--- @param n - the time to wait for reachability in milliseconds
--- @param p - a port to be checked for reachability
--- @return Nothing if port p is unreachable within n milliseconds,
---         or (Just m) if port p could be contacted within m milliseconds
ping :: Int -> Port _ -> IO (Maybe Int)
ping n p = (prim_ping $# n) $# p

prim_ping :: Int -> Port _ -> IO (Maybe Int)
prim_ping external

--- Checks for instantiation of a stream within some amount of time.
--- @param n - the time to wait for instantiation in milliseconds
--- @param str - the stream to be checked for instantiation
---              (usually the stream of incoming messages at some port)
--- @return (Just str) if str is instantiated within n milliseconds,
---         or Nothing otherwise
timeoutOnStream :: Int -> [a] -> Maybe [a]
timeoutOnStream n str = (prim_timeoutOnStream $# n) str

prim_timeoutOnStream :: Int -> [a] -> Maybe [a]
prim_timeoutOnStream external


--- A "stream port" is an adaption of the port concept to model the
--- communication with bidirectional streams, i.e., a stream port is
--- a port connection to a bidirectional stream (e.g., opened by
--- openProcessPort) where the communication
--- is performed via the following stream port messages.
---
--- @cons SP_Put s     - write the argument s on the output stream
--- @cons SP_GetLine s - unify the argument s with the next text line of the
---                      input stream
--- @cons SP_GetChar c - unify the argument c with the next character of the
---                      input stream
--- @cons SP_EOF b     - unify the argument b with True if we are at the end
---                      of the input stream, otherwise with False
--- @cons SP_Close     - close the input/output streams

data SP_Msg =
     SP_Put     String -- write the argument on the output stream
   | SP_GetLine String -- unify the argument with the next text line of the
                       -- input stream
   | SP_GetChar Char   -- unify the argument with the next character of the
                       -- input stream
   | SP_EOF     Bool   -- unify the argument with True if we are at the end
                       -- of the input stream, otherwise with False
   | SP_Close          -- close the input/output streams


--- Opens a new connection to a process that executes a shell command.
--- @param cmd - the shell command to be executed
--- @return the output/input stream (represented as a stream port)
---         that is connected to the standard input/output of the process
---         performing the execution of cmd.
openProcessPort :: String -> IO (Port SP_Msg)
openProcessPort cmd = prim_openProcessPort $## cmd

prim_openProcessPort :: String -> IO (Port SP_Msg)
prim_openProcessPort external


--- Opens an external port with a symbolic name.
--- @param portname - the symbolic name under which the port is accessible
---                   (any string without occurrences of '@')
--- @return the stream of incoming messages at this port
openNamedPort :: String -> IO [_]
openNamedPort name = do
  stream <- openPortOnSocket socketnr portnr       -- open new port
  registerPort name socketnr portnr
  return stream
 where socketnr,portnr free

--- Waits for connection to an external port.
--- In contrast to <code>connectPort</code>, this action waits until
--- the external port has been registered with its symbolic name.
--- @param waittime - the time to wait before retrying (in milliseconds)
--- @param action   - I/O action to be executed before each wait cycle
--- @param retries  - number of retries before giving up (-1 = retry forever)
--- @param portname - the symbolic name of the external port
---                   (must be either of the form "name@machine" or "name"
---                    where the latter is a shorthand for "name@localhost")
--- @return Nothing (if connection is not possible within the given limits)
---         or (Just p) where p is a port with the symbolic name portname
connectPortRepeat :: Int -> IO _ -> Int -> String -> IO (Maybe (Port _))
connectPortRepeat waittime action retries nameAtHost = do
  let (name,atHost) = break (=='@') nameAtHost
      host = if atHost=="" then "localhost" else tail atHost
  -- check whether remote CPNS demon is alive:
  alive <- cpnsAlive waittime host
  if not alive
    then tryAgain
    else do -- get remote socket/port numbers:
      (snr,pnr) <- getPortInfo name host
      if snr==0
        then tryAgain
        else connectPortAtSocket snr pnr host >>= return . Just
 where
  tryAgain = if retries==0 then return Nothing else do
    action
    sleep (ms2s waittime)
    connectPortRepeat waittime action (decr retries) nameAtHost

  ms2s n = let mn = n `div` 1000 in if mn==0 then 1 else mn

  decr n = if n<0 then n else n-1


--- Waits for connection to an external port and return the connected port.
--- This action waits (possibly forever) until the external port is
--- registered.
--- @param portname - the symbolic name of the external port
---                   (must be either of the form "name@host" or "name"
---                    where the latter is a shorthand for "name@localhost")
--- @return a port with the symbolic name portname
connectPortWait :: String -> IO (Port _)
connectPortWait nameAtHost = do
  Just port <- connectPortRepeat 1000 done (-1) nameAtHost
  return port


--- Connects to an external port. The external port must be already
--- registered, otherwise an error is reported.
--- @param portname - the symbolic name of the external port
---                   (must be either of the form "name@host" or "name"
---                    where the latter is a shorthand for "name@localhost")
--- @return a port with the symbolic name portname
connectPort   :: String -> IO (Port _)
connectPort nameAtHost = do
  let (name,atHost) = break (=='@') nameAtHost
      host = if atHost=="" then "localhost" else tail atHost
  -- get remote socket/port numbers:
  (snr,pnr) <- getPortInfo name host
  if snr==0
    then error ("connectPort: Port \""++name++"@"++host++
                "\" is not registered!")
    else done
  connectPortAtSocket snr pnr host


--- This function implements a committed choice over the receiving
--- of messages via a stream port and an external port.
---
--- <EM>Note that the implementation of choiceSPEP works only with
--- Sicstus-Prolog 3.8.5 or higher (due to a bug in previous versions
--- of Sicstus-Prolog).</EM>
---
--- @param sp - a stream port sp
--- @param ms - a stream of messages received via an external port
--- @return (Left s) if s is an input line received
---                  at the stream port (via SP_GetLine) or
--- 
---         (Right ms) if the stream ms is instantiated
---                    with at least one new message at the head

choiceSPEP :: Port SP_Msg -> [msg] -> Either String [msg]
choiceSPEP p ms = (prim_choiceSPEP $# p) ms

prim_choiceSPEP :: Port SP_Msg -> [msg] -> Either String [msg]
prim_choiceSPEP external


--- Creates a new object (of type <code>State -> [msg] -> Success</code>)
--- with an initial state and a port to which messages for this object
--- can be sent.
---
--- @param object - an object template
--- @param state - the initial state of the object
--- @param port - a free variable which will be constrained to the port
---               for sending messages to the object
newObject :: (state -> [msg] -> Success) -> state -> Port msg -> Success
newObject object state port = let msgs free in
  openPort port msgs &> object state (map ensureNotFree (ensureSpine msgs))


--- Creates a new object (of type <code>State -> [msg] -> Success</code>)
--- with a symbolic port name to which messages for this object can be sent.
--- @param object - an object template
--- @param state - the initial state of the object
--- @param portname - the symbolic name under which the object's port is
---                   accessible (any string without occurrences of '@')
newNamedObject :: (state -> [_] -> Success) -> state -> String -> IO ()
newNamedObject object state portname = do
  msgs <- openNamedPort portname
  doSolve (object state msgs)

--- Runs a new server (of type <code>[msg] -> IO a</code>) on a named port
--- to which messages can be sent.
--- @param server - a server function that processes incoming messages
--- @param portname - the symbolic name under which the server's port is
---                   accessible (any string without occurrences of '@')
runNamedServer :: ([_] -> IO a) -> String -> IO a
runNamedServer server portname = do
  msgs <- openNamedPort portname
  server msgs


------------------------------------------------------------------------------

-- The following predefined actions are not intended for application programs.
-- They are the basis to implement ports with symbolic names
-- via a name server (see library CPNS).


-- (openPortOnSocket snr pnr) is an action which opens an external port
-- on socket number snr with internal port number pnr and returns
-- the stream of incoming messages.
-- snr and pnr are allowed to be unbound: in this case they will be bound to the
-- numbers associated to a free port

openPortOnSocket :: Int -> Int -> IO [_]
openPortOnSocket snr pnr = (prim_openPortOnSocket $! snr) $! pnr

prim_openPortOnSocket :: Int -> Int -> IO [_]
prim_openPortOnSocket external


-- The internal function that reads a port stream lazily.
basicServerLoop :: Port a -> [a]
basicServerLoop external


-- (connectPortAtSocket snr pnr host) is an action which returns a port that
-- has been opened at <host> with socket number <snr> and port number <pnr>

connectPortAtSocket :: Int -> Int -> String -> IO (Port _)
connectPortAtSocket snr pnr host =
  ((prim_connectPortAtSocket $# snr) $# pnr) $## host

prim_connectPortAtSocket :: Int -> Int -> String -> IO (Port _)
prim_connectPortAtSocket external

--  end of module Ports
