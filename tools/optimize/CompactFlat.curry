------------------------------------------------------------------------------
--- Program to reduce the size of FlatCurry programs by replacing
--- the main FlatCurry file by a single FlatCurry file containing
--- no imports but all imported and potentially called functions.
---
--- @author Michael Hanus
--- @version June 2009
------------------------------------------------------------------------------

import List(intersperse)
import System
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Compact

-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
    [prog]              -> compactProgAndReplace [] prog
    ["-export",prog]    -> compactProgAndReplace [Exports] prog
    ["-main",func,prog] -> compactProgAndReplace [Main func] prog
    _ -> putStrLn $ "ERROR: Illegal arguments: " ++
                    concat (intersperse " " args) ++ "\n" ++
                    "Usage: compactflat [-export | -main func] <module_name>"

-- replace a FlatCurry program by a compactified version:
compactProgAndReplace options prog = do
  generateCompactFlatCurryFile (Required defaultRequired : options)
                               prog (prog++"_comp.fcy")
  let progfcy = flatCurryFileName prog
  system $ "mv "++prog++"_comp.fcy "++progfcy
  putStr $ "CompactFlat: compacted program '"++progfcy++"' written.\n"

