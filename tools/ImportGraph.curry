-----------------------------------------------------------------
-- A tool to visualize the module dependencies of a program
-- (without the prelude which is used everywhere) as a graph with DaVinci:

module ImportGraph where

import FlatCurry
import FlatCurryRead
import DaVinci
import List
import System
import Directory
import FileGoodies

main = do
  args <- getArgs
  if length args /= 1
   then putStrLn $ "ERROR: Illegal arguments: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: importgraph <module_name>"
   else analyzeAndShow (head args)

-- Analyze and show module dependencies for a program:
analyzeAndShow :: String -> IO ()
analyzeAndShow prog = do
  --aimps <- readAndProcessModuleWithImports analyseImports prog
  imps <- readFlatCurryIntWithImports prog
  showDeps (map analyseImports imps)

analyseImports :: Prog -> (String,[String])
analyseImports (Prog mod imps _ _ _) = (mod,imps)

--------------------------------------------------------------------------
-- Show a list of module dependencies as a graph with DaVinci:
showDeps :: [(String,[String])] -> IO ()
showDeps deps = dvDisplay (deps2dvGraph deps)

deps2dvGraph :: [(String,[String])] -> DvGraph
deps2dvGraph deps = graph2dvGraph $
 Graph (map Node (delete "Prelude" (nub (map fst deps ++ concatMap snd deps))))
       (concatMap (\(m,is)->map (Edge m) (delete "Prelude" is)) deps)

--------------------------------------------------------------------------
-- A graph is a list of nodes and edges:
data Graph = Graph [Node] [Edge]

-- A node is identified by a string (we assume that these strings are unique):
data Node = Node String

-- An edge consists of a source and a target node (identified by their ids):
data Edge = Edge String String

-- Convert this representation into the daVinci
-- representation by grouping edges to their source node:
graph2dvGraph :: Graph -> DvGraph
graph2dvGraph (Graph nodes edges) =
 dvNewGraph 
   (map (\(dvid,label)->dvNodeWithEdges dvid label
                                        (map (\(Edge _ n2)->createEdge n2)
                                             (filter (\(Edge n1 _)->n1==label)
                                                     edges))
                                        dvEmptyH) idnodes)
 where
   idnodes = map node2idnode nodes

   createEdge tlabel =
     dvSimpleEdge e (fst (head (filter (\(_,l)->l==tlabel) idnodes))) dvEmptyH
              where e free

   -- transform node into node with a DvId:
   node2idnode (Node label) = (dvid,label)  where dvid free

