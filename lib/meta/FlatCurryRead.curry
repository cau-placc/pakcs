------------------------------------------------------------------------------
--- This library defines operations to read a FlatCurry programs or interfaces
--- together with all its imported modules in the current load path.
---
--- @author Michael Hanus
--- @version June 2009
------------------------------------------------------------------------------

module FlatCurryRead(readFlatCurryWithImports,readFlatCurryWithImportsInPath,
                     readFlatCurryIntWithImports,
                     readFlatCurryIntWithImportsInPath)
  where

import FlatCurry
import Directory
import FileGoodies
import System
import Distribution
import List(intersperse)

--------------------------------------------------------------------------
--- Reads a FlatCurry program together with all its imported modules.
--- The argument is the name of the main module (possibly with a directory prefix).
readFlatCurryWithImports :: String -> IO [Prog]
readFlatCurryWithImports modname = do
  loadpath <- getLoadPathForFile (flatCurryFileName modname)
  readFlatCurryFileWithImports loadpath (baseName modname) [".fcy"]

--- Reads a FlatCurry program together with all its imported modules in a given
--- load path.
--- The arguments are a load path and the name of the main module.
readFlatCurryWithImportsInPath :: [String] -> String -> IO [Prog]
readFlatCurryWithImportsInPath loadpath modname =
  readFlatCurryFileWithImports loadpath modname [".fcy"]

--- Reads a FlatCurry interface together with all its imported module interfaces.
--- The argument is the name of the main module (possibly with a directory prefix).
--- If there is no interface file but a FlatCurry file (suffix ".fcy"),
--- the FlatCurry file is read instead of the interface.
readFlatCurryIntWithImports :: String -> IO [Prog]
readFlatCurryIntWithImports modname = do
  loadpath <- getLoadPathForFile (flatCurryIntName modname)
  readFlatCurryFileWithImports loadpath (baseName modname) [".fint",".fcy"]

--- Reads a FlatCurry interface together with all its imported module interfaces
--- in a given load path.
--- The arguments are a load path and the name of the main module.
--- If there is no interface file but a FlatCurry file (suffix ".fcy"),
--- the FlatCurry file is read instead of the interface.
readFlatCurryIntWithImportsInPath :: [String] -> String -> IO [Prog]
readFlatCurryIntWithImportsInPath loadpath modname =
  readFlatCurryFileWithImports loadpath modname [".fint",".fcy"]

-- Read a FlatCurry file together with its imported modules.
-- The argument is the loadpath, the name of the main module, and the
-- possible suffixes of the FlatCurry file (e.g., [".fint",".fcy"]).
readFlatCurryFileWithImports :: [String] -> String -> [String] -> IO [Prog]
readFlatCurryFileWithImports loadpath mod suffixes = do
  putStr "Reading FlatCurry files "
  -- try to read the interface files directly:
  mbaimps <- tryReadFlatCurryFileWithImports loadpath mod suffixes
  maybe (parseFlatCurryFileWithImports loadpath mod suffixes) return mbaimps


parseFlatCurryFileWithImports loadpath modname suffixes = do
  putStrLn $ ">>>>> FlatCurry files not up-to-date, parsing module \""++modname++"\"..."
  callFrontendWithParams FCY
     (setQuiet True (setFullPath loadpath defaultParams)) modname
  putStr "Reading FlatCurry files "
  collectMods [modname] []
 where
  collectMods [] _ = putStrLn "done" >> return []
  collectMods (mod:mods) implist =
    if mod `elem` implist
    then collectMods mods implist
    else lookupFileInPath mod suffixes loadpath >>= \mbfname ->
         maybe (error $ "FlatCurry file for module \""++mod++"\" not found!")
               (\filename ->
                     putStr (filename++" ") >>
                     readFlatCurryFile filename >>= \prog ->
                     collectMods (mods++importsOf prog) (mod:implist) >>= \results ->
                     return (prog : results))
               mbfname

-- Read and process the imported interfaces of a module.
-- First argument is a processing action and second argument
-- is a module to be processed.
-- Nothing is returned if some interface files do not exists or are not
-- up-to-date
tryReadFlatCurryFileWithImports :: [String] -> String -> [String] -> IO (Maybe [Prog])
tryReadFlatCurryFileWithImports loadpath modname suffixes =
  collectMods [modname] []
 where
  collectMods [] _ = putStrLn "done" >> return (Just [])
  collectMods (mod:mods) implist =
    if mod `elem` implist
    then collectMods mods implist
    else
      readFlatCurryIfPossible loadpath mod suffixes >>= \mbprog ->
      maybe (return Nothing)
            (\prog->collectMods (mods++importsOf prog) (mod:implist) >>=
                    \mbresults -> return (maybe Nothing
                                                (\results->Just (prog : results))
                                                mbresults))
             mbprog

importsOf :: Prog -> [String]
importsOf (Prog _ imps _ _ _) = imps

-- Read a FlatCurry file for a module if it exists and is up-to-date
-- w.r.t. the source program. If no source exists, it is always assumed
-- to be up-to-date.
readFlatCurryIfPossible :: [String] -> String -> [String] -> IO (Maybe Prog)
readFlatCurryIfPossible loadpath modname suffixes = do
  mbfname <- lookupFileInPath modname [".lcurry",".curry"] loadpath
  maybe (lookupFileInPath modname suffixes loadpath >>= \fname ->
         maybe (return Nothing)
               (\progname -> readFlatCurryFile progname >>= return . Just)
               fname)
        (\sname -> do
           let moddir    = dirName sname
               pathofmod = [moddir,addCurrySubdir moddir]
           mbflatfile <- lookupFileInPath modname suffixes pathofmod
           maybe (return Nothing)
                 (\flatfile -> do
                    ctime <- getModificationTime sname
                    ftime <- getModificationTime flatfile
                    if ctime>ftime
                     then return Nothing
                     else putStr (flatfile++" ") >>
                          readFlatCurryFile flatfile >>= return . Just )
                 mbflatfile)
        mbfname

--------------------------------------------------------------------------
