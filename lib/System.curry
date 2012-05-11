------------------------------------------------------------------------------
--- Library to access parts of the system environment.
---
--- @author Michael Hanus, Bernd Brassel
--- @version August 2007
------------------------------------------------------------------------------

module System(getCPUTime,getElapsedTime,
              getArgs,getEnviron,setEnviron,unsetEnviron,
              getHostname,getPID,getProgName,
              system,exitWith,sleep) where

import Global

--- Returns the current cpu time of the process in milliseconds.

getCPUTime :: IO Int
getCPUTime external
 
--- Returns the current elapsed time of the process in milliseconds.

getElapsedTime :: IO Int
getElapsedTime external
 
--- Returns the list of the program's command line arguments.
--- The program name is not included.

getArgs :: IO [String]
getArgs external
 
--- Returns the value of an environment variable.
--- The empty string is returned for undefined environment variables.

getEnviron :: String -> IO String
getEnviron evar = do
  envs <- readGlobal environ
  maybe (prim_getEnviron $## evar) return (lookup evar envs)
  
prim_getEnviron :: String -> IO String
prim_getEnviron external
 
--- internal state of environment variables set via setEnviron
environ :: Global [(String,String)]
environ = global [] Temporary

--- Set an environment variable to a value.
--- The new value will be passed to subsequent shell commands
--- (see <code>system</code>) and visible to subsequent calls to
--- <code>getEnviron</code> (but it is not visible in the environment
--- of the process that started the program execution).

setEnviron :: String -> String -> IO ()
setEnviron evar val = do
  envs <- readGlobal environ
  writeGlobal environ ((evar,val) : filter ((/=evar) . fst) envs)
 
--- Removes an environment variable that has been set by
--- <code>setEnviron</code>.

unsetEnviron :: String -> IO ()
unsetEnviron evar = do
  envs <- readGlobal environ
  writeGlobal environ (filter ((/=evar) . fst) envs)
 
--- Returns the hostname of the machine running this process.

getHostname :: IO String
getHostname external

--- Returns the process identifier of the current Curry process.

getPID :: IO Int
getPID external


--- Returns the name of the current program, i.e., the name of the
--- main module currently executed.

getProgName :: IO String
getProgName external


--- Executes a shell command and return with the exit code of the command.
--- An exit status of zero means successful execution.

system :: String -> IO Int
system cmd = do
   envs <- readGlobal environ
   prim_system $## (concatMap envToExport envs ++ cmd)
 where
   envToExport (var,val) =
     var++"='"++ concatMap encodeShellSpecials val ++"' ; export "++var++" ; "

   encodeShellSpecials c | c=='\''   = map chr [39,34,39,34,39]
                         | otherwise = [c]

prim_system :: String -> IO Int
prim_system external


--- Terminates the execution of the current Curry program
--- and returns the exit code given by the argument.
--- An exit code of zero means successful execution.

exitWith :: Int -> IO _
exitWith exitcode = prim_exitWith $# exitcode

prim_exitWith :: Int -> IO _
prim_exitWith external


--- The evaluation of the action (sleep n) puts the Curry process
--- asleep for n seconds.

sleep :: Int -> IO ()
sleep n = prim_sleep $# n

prim_sleep :: Int -> IO ()
prim_sleep external
