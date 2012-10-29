-- Simple graph visualization
module ShowGraph(viewDependencyGraph,getDotViewCmd,setDotViewCmd) where

import IO
import IOExts
import Char(isAlphaNum)
import List(intersperse)
import Distribution(rcFileName,getRcVar)
import PropertyFile(updatePropertyFile)

-- Show a dependency graph with dot.
-- A dependency graph consists of a list of triples of the form (n,as,ms),
-- where n is a node name, as (dot) attributes for node n, and ms the list
-- of direct dependents from n.
viewDependencyGraph :: [(String,[(String,String)],[String])] -> IO ()
viewDependencyGraph deps = viewDot (deps2dot deps)

-- translate dependencies into DOT language:
deps2dot :: [(String,[(String,String)],[String])] -> String
deps2dot deps =
  "digraph dependencies{\n" ++ concatMap dep2dot deps ++ "}\n"
 where
  dep2dot (x,attrs,xdeps) =
    let attrtxt = if null attrs then ""
                  else showDotID x ++
                       '[':concat (intersperse ","
                                    (map (\ (n,v)->n++"=\""++v++"\"") attrs)) ++ "]"
                        ++ ";\n"  in
    if null xdeps
    then if null attrs then showDotID x ++ ";\n" else attrtxt
    else concatMap (\i->showDotID x ++ " -> " ++ showDotID i ++ ";\n") xdeps ++ attrtxt

showDotID s | all isAlphaNum s = s
            | otherwise = '"' : concatMap escapeDQ s ++ "\""
 where escapeDQ c = if c=='"' then "\\\"" else [c]

-- visualize a DOT string:
viewDot :: String -> IO ()
viewDot dottxt = do
    dotview <- getDotViewCmd
    dotstr <- connectToCommand dotview
    hPutStr dotstr dottxt
    hClose dotstr

-------------------------------------------------------------------------
-- Read the command for viewing dot files from rc file:
getDotViewCmd :: IO String
getDotViewCmd = getRcVar "dotviewcommand" >>= return . maybe "" id

-- Set the command for viewing dot files in the rc file:
-- Set dot view command in kics2rc file:
setDotViewCmd :: String -> IO ()
setDotViewCmd dvcmd = do
  rcfile <- rcFileName
  updatePropertyFile rcfile "dotviewcommand" dvcmd
