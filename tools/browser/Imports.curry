-----------------------------------------------------------------
-- A library to get the import structures of a program.

module Imports(getImportedInterfaces,moduleImports,readFlatCurryFileInLoadPath,
               findFileInLoadPath,InterfaceOrFlatProg(..),ifOrProg,progOfIFFP)
  where

import FlatCurry
import FlatCurryGoodies
import FlatCurryRead
import FileGoodies
import System(getArgs)
import Distribution(getLoadPath,addCurrySubdir)
import Directory
import Maybe

-- Get all interfaces (i.e., main and all indirectly imported modules) of a program:
getImportedInterfaces :: String -> IO [(String,InterfaceOrFlatProg)]
getImportedInterfaces mod = do
  args <- getArgs
  imps <- readFlatCurryIntWithImports (if null args
                                       then mod
                                       else dirName(head args)++"/"++mod)
  return (map (\prog -> (moduleName prog, IF prog)) imps)

-- Extract a module and its imports:
moduleImports (Prog mod imps _ _ _) = (mod,imps)

-----------------------------------------------------------------------------
-- Unione type to distinguish between interface and FlatCurry program:
data InterfaceOrFlatProg = IF Prog | FP Prog

ifOrProg :: (Prog->a) -> (Prog->a) -> InterfaceOrFlatProg -> a
ifOrProg iffun _ (IF prog) = iffun prog
ifOrProg _ fpfun (FP prog) = fpfun prog

progOfIFFP (IF prog) = prog
progOfIFFP (FP prog) = prog

--------------------------------------------------------------------------
-- Read an existing(!) FlatCurry file w.r.t. current load path:
readFlatCurryFileInLoadPath prt mod = do
  mbfilename <- findFileInLoadPath mod [".fcy"]
  maybe (error $ "FlatCurry file of module "++mod++" not found!")
        (readFlatCurryFileAndReport prt mod)
        mbfilename

readFlatCurryFileAndReport prt mod filename = do
  size <- fileSize filename
  prt $ "Reading FlatCurry file of module '"++mod++"' ("++show size++" bytes)..."
  prog <- readFlatCurryFile filename
  seq (prog==prog) (return prog)

--------------------------------------------------------------------------
--- Finds the first file with a possible suffix in the load path:
findFileInLoadPath :: String -> [String] -> IO (Maybe String)
findFileInLoadPath file suffixes = do
    loadpath <- getLocalLoadPath suffixes
    lookupFileInPath file suffixes loadpath

-- computes real load path in case of non-local first program argument:
getLocalLoadPath _ = do
  loadpath <- getLoadPath
  args <- getArgs
  if null args
   then return loadpath
   else let dir = dirName (head args)
         in return (dir : addCurrySubdir dir : tail loadpath)

--------------------------------------------------------------------------
