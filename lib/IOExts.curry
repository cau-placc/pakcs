------------------------------------------------------------------------------
--- Library with some useful extensions to the IO monad.
---
--- @author Michael Hanus
--- @version June 2007
------------------------------------------------------------------------------

module IOExts(execCmd,connectToCommand,
              readCompleteFile,updateFile,
              exclusiveIO,setAssoc,getAssoc,
              IORef,newIORef,readIORef,writeIORef) where

import System
import IO(Handle)

--- Executes a command with a new default shell process.
--- The standard I/O streams of the new process (stdin,stdout,stderr)
--- are returned as handles so that they can be explicitly manipulated.
--- They should be closed with <code>IO.hClose</code> since they are not
--- closed automatically when the process terminates.
--- @param cmd - the shell command to be executed
--- @return the handles of the input/output/error streams of the new process
execCmd :: String -> IO (Handle,Handle,Handle)
execCmd cmd = prim_execCmd $## cmd

prim_execCmd :: String -> IO (Handle,Handle,Handle)
prim_execCmd external


--- Executes a command with a new default shell process.
--- The input and output streams of the new process is returned
--- as one handle which is both readable and writable.
--- Thus, writing to the handle produces input to the process and
--- output from the process can be retrieved by reading from this handle.
--- The handle should be closed with <code>IO.hClose</code> since they are not
--- closed automatically when the process terminates.
--- @param cmd - the shell command to be executed
--- @return the handle connected to the input/output streams
---         of the new process
connectToCommand :: String -> IO Handle
connectToCommand cmd = prim_connectToCmd $## cmd

prim_connectToCmd :: String -> IO Handle
prim_connectToCmd external


--- An action that reads the complete contents of a file and returns it.
--- This action can be used instead of the (lazy) <code>readFile</code>
--- action if the contents of the file might be changed.
--- @param file - the name of the file
--- @return the complete contents of the file
readCompleteFile :: String -> IO String
readCompleteFile file = do
  s <- readFile file
  f s (return s)
 where
   f []     r = r
   f (_:cs) r = f cs r


--- An action that updates the contents of a file.
--- @param f - the function to transform the contents
--- @param file - the name of the file
updateFile :: (String -> String) -> String -> IO ()
updateFile f file = do
  s <- readCompleteFile file
  writeFile file (f s)


--- Forces the exclusive execution of an action via a lock file.
--- For instance, (exclusiveIO "myaction.lock" act) ensures that
--- the action "act" is not executed by two processes on the same
--- system at the same time.
--- @param lockfile - the name of a global lock file
--- @param action - the action to be exclusively executed
--- @return the result of the execution of the action
exclusiveIO :: String -> IO a -> IO a
exclusiveIO lockfile action = do
  system ("lockfile -1 "++lockfile)
  catch (do actionResult <- action
            system ("rm -f "++lockfile)
            return actionResult )
        (\e -> system ("rm -f "++lockfile) >> ioError e)


--- Defines a global association between two strings.
--- Both arguments must be evaluable to ground terms before applying
--- this operation.
setAssoc :: String -> String -> IO ()
setAssoc key val = (prim_setAssoc $## key) $## val

prim_setAssoc :: String -> String -> IO ()
prim_setAssoc external


--- Gets the value associated to a string.
--- Nothing is returned if there does not exist an associated value.
getAssoc :: String -> IO (Maybe String)
getAssoc key = prim_getAssoc $## key

prim_getAssoc :: String -> IO (Maybe String)
prim_getAssoc external


--- Mutable variables containing values of some type.
--- The values are not evaluated when they are assigned to an IORef.
data IORef a = IORef a -- precise structure internally defined

--- Creates a new IORef with an initial values.
newIORef :: a -> IO (IORef a)
newIORef external

--- Reads the current value of an IORef.
readIORef :: IORef a -> IO a
readIORef ref = prim_readIORef $# ref

prim_readIORef :: IORef a -> IO a
prim_readIORef external

--- Updates the value of an IORef.
writeIORef :: IORef a -> a -> IO ()
writeIORef ref val = (prim_writeIORef $# ref) val

prim_writeIORef :: IORef a -> a -> IO ()
prim_writeIORef external

