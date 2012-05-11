--------------------------------------------------------------------------------
--- This module contains functions to obtain information concerning the current
--- distribution of the Curry implementation.
--- Most of the information is based on the external constants 
--- <b>curryCompiler...</b>.
---
--- @author Bernd Brassel, Michael Hanus
--- @version April 2010
--------------------------------------------------------------------------------

module Distribution (
  curryCompiler,curryCompilerMajorVersion,curryCompilerMinorVersion,
  curryRuntime,curryRuntimeMajorVersion,curryRuntimeMinorVersion,
  installDir,currySubdir,inCurrySubdir,addCurrySubdir,
  
  rcFileName,rcFileContents,getRcVar,getRcVars,

  findFileInLoadPath,lookupFileInLoadPath,
  readFirstFileInLoadPath,getLoadPath,getLoadPathForFile,

  FrontendTarget(..), 
  
  FrontendParams, defaultParams,
  quiet, fullPath, outfile, logfile,
  setQuiet, setFullPath, setOutfile, setLogfile,

  callFrontend,callFrontendWithParams
  ) where

import List(intersperse)
import Char(toLower)
import System
import IO
import FileGoodies
import PropertyFile

-----------------------------------------------------------------
-- Compiler and run-time environment name and version
-----------------------------------------------------------------

-- if you do not use other functions but 
-- if-then-else, and the _Prelude_ functions
-- (<), (>), (<=), (>=), (==)
-- directly on the following constants, 
-- the compiler might be able to eliminate
-- them at compile time.

--- The name of the Curry compiler (e.g., "pakcs" or "kics").
curryCompiler :: String
curryCompiler external

--- The major version number of the Curry compiler.
curryCompilerMajorVersion :: Int
curryCompilerMajorVersion external

--- The minor version number of the Curry compiler.
curryCompilerMinorVersion :: Int
curryCompilerMinorVersion external

--- The name of the run-time environment (e.g., "sicstus", "swi", or "ghc")
curryRuntime :: String
curryRuntime external

--- The major version number of the Curry run-time environment.
curryRuntimeMajorVersion :: Int
curryRuntimeMajorVersion external

--- The minor version number of the Curry run-time environment.
curryRuntimeMinorVersion :: Int
curryRuntimeMinorVersion external

---------------------------------------------------
-- retrieving user specified options from rc file
---------------------------------------------------

--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: IO String
rcFileName = getEnviron "HOME" >>= return . (++"/."++curryCompiler++"rc")

--- Returns the current configuration parameters of the distribution.
--- This action yields the list of pairs (var,val).
rcFileContents :: IO [(String,String)]
rcFileContents = rcFileName >>= readPropertyFile

--- Look up a specific configuration variable as specified by user in his rc file.
--- Uppercase/lowercase is ignored for the variable names.
getRcVar :: String -> IO (Maybe String)
getRcVar var = getRcVars [var] >>= return . head

--- Look up configuration variables as specified by user in his rc file.
--- Uppercase/lowercase is ignored for the variable names.
getRcVars :: [String] -> IO [Maybe String]
getRcVars vars = do
  rcs <- rcFileContents 
  return (map (flip lookup (map (\ (a,b)->(map toLower a,b)) rcs))
              (map (map toLower) vars))

-----------------------------------------------------------
--- finding files in correspondence to compiler load path
-----------------------------------------------------------

--- Name of the main installation directory of the Curry compiler.
installDir :: String
installDir external

--- Name of the sub directory where auxiliary files (.fint, .fcy, etc)
--- are stored.
currySubdir :: String
currySubdir = ".curry"

--- Transforms a file name by adding the currySubDir to the file name.
inCurrySubdir :: String -> String
inCurrySubdir filename =
  let (base,file) = splitDirectoryBaseName filename
   in base++'/':currySubdir++'/':file

--- Transforms a directory name into the name of the corresponding
--- sub directory containing auxiliary files.
addCurrySubdir :: String -> String
addCurrySubdir dir = dir++'/':currySubdir

--- Returns the current path (list of directory names) of the
--- system libraries.
getSysLibPath :: IO [String]
getSysLibPath = case curryCompiler of
  "pakcs" -> do pakcspath <- getEnviron "PAKCSLIBPATH"
                return
                 (if null pakcspath
                  then [installDir++"/lib",installDir++"/lib/meta"]
                  else splitPath pakcspath)
  "kics"  -> return [installDir++"/src/lib"]
  "kics2" -> return [installDir++"/lib",installDir++"/lib/meta"]
  _ -> error "Distribution.getSysLibPath: unknown curryCompiler"


--- Adds a directory name to a file by looking up the current load path.
--- An error message is delivered if there is no such file.
lookupFileInLoadPath :: String -> IO (Maybe String)
lookupFileInLoadPath fn = 
  getLoadPathForFile fn >>= lookupFileInPath (baseName fn) [""] 

--- Adds a directory name to a file by looking up the current load path.
--- An error message is delivered if there is no such file.
findFileInLoadPath :: String -> IO String
findFileInLoadPath fn = 
  getLoadPathForFile fn >>= getFileInPath (baseName fn) [""] 

--- Returns the contents of the file found first in the current load path.
--- An error message is delivered if there is no such file.
readFirstFileInLoadPath :: String -> IO String
readFirstFileInLoadPath fn = findFileInLoadPath fn >>= readFile 

--- Returns the current path (list of directory names) that is
--- used for loading modules.
getLoadPath :: IO [String]
getLoadPath = getLoadPathForFile "xxx"

--- Returns the current path (list of directory names) that is
--- used for loading modules w.r.t. a given main module file.
--- The directory prefix of the module file (or "." if there is
--- no such prefix) is the first element of the load path and the
--- remaining elements are determined by the environment variables
--- CURRYRPATH and PAKCSLIBPATH when using pakcs and
--- the entry of kicsrc when using kics, respectively.
getLoadPathForFile :: String -> IO [String]
getLoadPathForFile file = do
  syslib <- getSysLibPath
  mblib <- getRcVar "Libraries"
  let fileDir = dirName file 
  if curryCompiler `elem` ["pakcs","kics","kics2"] then
    do currypath <- getEnviron "CURRYPATH"
       let llib = maybe [] splitPath mblib
       return (addCurrySubdirs (fileDir :
                                   (if null currypath
                                    then []
                                    else splitPath currypath) ++
                                    llib ++ syslib))

    else error "Distribution.getLoadPathForFile: unknown curryCompiler"
 where
  addCurrySubdirs = concatMap (\d->[d,addCurrySubdir d])


-------------------------------------------------------------------
-- calling the front end
-------------------------------------------------------------------

--- Data type for representing the different target files that can be produced
--- by the front end of the Curry compiler.
--- @cons FCY - FlatCurry file ending with .fcy
--- @cons FINT - FlatCurry interface file ending with .fint
--- @cons ACY - AbstractCurry file ending with .acy
--- @cons UACY - Untyped (without type checking) AbstractCurry file ending with .uacy
--- @cons HTML - colored HTML representation of source program
--- @cons CY - source representation employed by the frontend
data FrontendTarget = FCY | FINT | ACY | UACY | HTML | CY

--- Abstract data type for representing parameters supported by the front end
--- of the Curry compiler.
-- The parameters are of the form
-- FrontendParams Quiet FullPath OutFile LogFile
-- where
--   Quiet - work silently
--   FullPath dirs - the complete list of directory names for loading modules
--   OutFile file - output file (currently, only relevant for HTML target)
--   LogFile file - store all output (including errors) of the front end in file
data FrontendParams =
  FrontendParams Bool (Maybe [String]) (Maybe String) (Maybe String)

--- The default parameters of the front end.
defaultParams :: FrontendParams
defaultParams = FrontendParams False Nothing Nothing Nothing

--- Set quiet mode of the front end.
setQuiet :: Bool -> FrontendParams -> FrontendParams
setQuiet s (FrontendParams _ x y z) = FrontendParams s x y z

--- Set the full path of the front end.
--- If this parameter is set, the front end searches all modules
--- in this path (instead of using the default path).
setFullPath ::  [String] -> FrontendParams -> FrontendParams 
setFullPath s (FrontendParams x _ y z) = FrontendParams x (Just s) y z

--- Set the outfile parameter of the front end.
--- Relevant for HTML generation.
setOutfile ::  String -> FrontendParams -> FrontendParams 
setOutfile  s (FrontendParams x y _ z) = FrontendParams x y (Just s) z

--- Set the logfile parameter of the front end.
--- If this parameter is set, all messages produced by the front end
--- are stored in this file.
setLogfile ::  String -> FrontendParams -> FrontendParams 
setLogfile  s (FrontendParams x y z _) = FrontendParams x y z (Just s)

--- Returns the value of the "quiet" parameter.
quiet :: FrontendParams -> Bool
quiet (FrontendParams x _ _ _) = x

--- Returns the full path parameter of the front end.
fullPath :: FrontendParams -> Maybe [String]
fullPath (FrontendParams _ x _ _) = x

--- Returns the outfile parameter of the front end.
outfile :: FrontendParams -> Maybe String
outfile  (FrontendParams _ _ x _) = x

--- Returns the logfile parameter of the front end.
logfile :: FrontendParams -> Maybe String
logfile  (FrontendParams _ _ _ x) = x

--- In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--- are up to date, one can call the front end of the Curry compiler with this action.
--- @param target - the kind of target file to be generated
--- @param progname - the name of the main module of the application to be compiled
callFrontend :: FrontendTarget -> String -> IO ()
callFrontend target = callFrontendWithParams target defaultParams

--- In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--- are up to date, one can call the front end of the Curry compiler
--- with this action where various parameters can be set.
--- @param target - the kind of target file to be generated
--- @param params - parameters for the front end
--- @param progname - the name of the main module of the application to be compiled
callFrontendWithParams :: FrontendTarget -> FrontendParams -> String -> IO ()
callFrontendWithParams target params progname = do
  parsecurry <- callParseCurry
  let lf      = maybe "" id (logfile params)
      syscall = "\"" ++ parsecurry ++ "\" " ++ showFrontendTarget target
                     ++ showFrontendParams 
                     ++ " " ++ progname
  if null lf
    then system syscall
    else system (syscall++" > "++lf++" 2>&1")
  return ()
 where
   callParseCurry = case curryCompiler of
     "pakcs" -> return ("\""++installDir++"/bin/parsecurry\"")
     "kics"  -> do path <- maybe getLoadPath return (fullPath params)
                   return ("\""++installDir++"/bin/parsecurry\""++
                           concatMap (" -i"++) path)
     "kics2"  -> do path <- maybe getLoadPath return (fullPath params)
                    return ("\""++installDir++"/bin/cymake\""++
                            concatMap (\d->" -i\""++d++"\"") path)
     _ -> error "Distribution.callFrontend: unknown curryCompiler"

   showFrontendTarget FCY  = "--flat"
   showFrontendTarget FINT = "--flat"
   showFrontendTarget ACY  = "--acy"
   showFrontendTarget UACY = "--uacy"
   showFrontendTarget HTML = "--html"
   showFrontendTarget CY   = "--parse-only"

   showFrontendParams =
     (if quiet params then runQuiet else "")
     ++ (maybe "" (" -o "++) (outfile params))
     ++ (maybe "" (\p -> if curryCompiler=="pakcs"
                         then " --fullpath " ++ concat (intersperse ":" p)
                         else "")
               (fullPath params))

   runQuiet = if curryCompiler=="pakcs"
              then " --quiet "
              else " --no-verb --no-warn --no-overlap-warn " -- kics(2)

rcErr :: String -> a -> IO a
rcErr s x = hPutStrLn stderr (s ++ " undefined in rc file") >> return x

