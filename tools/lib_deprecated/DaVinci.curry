-----------------------------------------------------------------------------
--- Binding for the daVinci graph visualization tool.
---
--- This library supports the visualization of graphs by the
--- [daVinci graph drawing tool](http://www.tzi.de/daVinci/)
--- through the following features:
---
---   * Graphs are displayed by the main functions
---     dvDisplay or dvDisplayInit
---
---   * Graphs to be displayed are constructed by the functions:
--- 
---     dvNewGraph:      takes a list of nodes to construct a graph
--- 
---     dvSimpleNode:    a node without outgoing edges
--- 
---     dvNodeWithEdges: a node with a list of outgoing edges
--- 
---     dvSimpleEdge:    an edge to a particular node
--- 
---     The constructors dvSimpleNode/dvNodeWithEdges/dvSimpleEdge
---     have a graph identifier (type DvId) as a first argument.
---     This identifier is a free variable (since type DvId is abstract)
---     and can be used in other functions to refer to this node or edge.

---   * The constructor functions for graph entities take an event handler
---     (of type "DvWindow -> Success") as the last argument.
---     This event handler is executed whenever the user clicks on the
---     corresponding graph entity.

---   * There are a number of predefined event handlers to manipulate
---     existing graphs (see functions dvSetNodeColor, dvAddNode, dvSetEdgeColor,
---     dvAddEdge, dvDelEdge, dvSetClickHandler).
---     dvEmptyH is the "empty handler" which does nothing.
---
--- For a correct installation of this library, the constant `dvStartCmd`
--- defined below must be correctly set to start your local installation
--- of DaVinci.
---
--- @author Johannes Koj (with modifications by Michael Hanus)
--- @version August 2003
-----------------------------------------------------------------------------

module DaVinci(dvNewGraph,dvSimpleNode,dvNodeWithEdges,dvSimpleEdge,
               dvSetNodeColor,dvAddNode,dvSetEdgeColor,dvAddEdge,
               dvDelEdge,dvSetClickHandler,dvEmptyH,
               DvWindow,DvGraph,DvNode,DvEdge,DvId,DvScheduleMsg,
               dvDisplay,dvDisplayInit) where

import List
import Ports

-----------------------------------------------------------------------------
--- This constant must be set to the shell command to start the
--- daVinci system API.
dvStartCmd = let dvHome = "/opt/daVinci-Presenter-3.0.2/"
              in "DAVINCIHOME="++dvHome++" && "++
                 "export DAVINCIHOME"++" && "++
                 dvHome++"/bin/daVinci -pipe"
-- for older version:
--dvStartCmd = let dvHome = "/home/pakcs/daVinci_V2.1/"
--              in "DAVINCIHOME="++dvHome++" && "++
--                 "export DAVINCIHOME"++" && "++
--                 dvHome++"/daVinci -pipe"

-----------------------------------------------------------------------------
-- the interface
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- basic display functions:

--- Displays a graph with daVinci and run the scheduler for handling events.
dvDisplay :: DvGraph -> IO ()
dvDisplay graph = dvDisplayInit graph(\_->success)

--- Displays a graph with daVinci and run the scheduler for handling events
--- after performing some initialization events.
dvDisplayInit :: DvGraph -> (DvWindow->Success) -> IO ()
dvDisplayInit graph evt = openDV >>= display
  where
    display dvport | initDvSchedule graph dvport evt  = done


-----------------------------------------------------------------------------
-- basic functions to construct graphs:

--- Constructs a new graph from a list of nodes.
dvNewGraph :: [DvNode] -> DvGraph
dvNewGraph nodes = DvGraph nodes

--- A node without outgoing edges.
--- @param node - a graph identifier (usually: a free variable)
--- @param label - a label written at this node
--- @param clickH - an event hander to be executed whenver the user clicks
---                 on this node
--- @return a new graph node
dvSimpleNode :: DvId -> String -> (DvWindow->Success) -> DvNode
dvSimpleNode node label clickH =
   DvNode node "" [DvAttribute "OBJECT" label] [] [DvClick clickH]

--- A node with a list of outgoing edges.
--- @param node - a graph identifier (usually: a free variable)
--- @param label - a label written at this node
--- @param edges - a list of outgoing edges
--- @param clickH - an event hander to be executed whenver the user clicks
---                 on this node
--- @return a new graph node
dvNodeWithEdges :: DvId -> String -> [DvEdge] -> (DvWindow->Success) -> DvNode
dvNodeWithEdges node label edges clickH =
   DvNode node "" [DvAttribute "OBJECT" label] edges [DvClick clickH]

--- An edge to a particular node.
--- @param edge - a graph identifier for this edge (usually: a free variable)
--- @param node - a graph identifier of the target node
--- @param clickH - an event hander to be executed whenver the user clicks
---                 on this edge
--- @return a new graph edge
dvSimpleEdge :: DvId -> DvId -> (DvWindow->Success) -> DvEdge
dvSimpleEdge edge node clickH
  | let dvid free in node=:=(DvId dvid)
  = DvEdge edge "" [] (DvNodeRef node) [DvClick clickH]


-----------------------------------------------------------------------------
-- basic event handlers for the manipulation of graphs:

--- An event handler that sets the color (second argument) of a node.
dvSetNodeColor :: DvId -> String -> DvWindow -> Success
dvSetNodeColor node color dvwin =
  send (DvCmdMsg (DvGraphCmd (DvChangeAttr 
                         [DvNodeAttrChange node [DvAttribute "COLOR" color]])))
       dvwin

--- An event handler that adds a new node to the graph.
dvAddNode :: DvId -> String -> (DvWindow->Success) -> DvWindow -> Success
dvAddNode node label clickH dvwin =
  send (DvCmdMsg (DvGraphCmd (DvUpdate
                                [DvNewNode node ""
                                           [DvAttribute "OBJECT" label]
                                           [DvClick clickH]] [])))
       dvwin

--- An event handler that sets the color (second argument) of an edge.
dvSetEdgeColor :: DvId -> String -> DvWindow -> Success
dvSetEdgeColor edge color dvwin =
  send (DvCmdMsg (DvGraphCmd (DvChangeAttr
                         [DvEdgeAttrChange edge [DvAttribute "COLOR" color]])))
       dvwin

--- An event handler that adds a new edge to the graph.
dvAddEdge :: DvId -> DvId -> DvId -> (DvWindow->Success) -> DvWindow -> Success
dvAddEdge edge node1 node2 clickH dvwin = 
  send (DvCmdMsg (DvGraphCmd (DvUpdate []
                       [DvNewEdge edge "" [] node1 node2 [DvClick clickH]])))
       dvwin

--- An event handler that deletes an existing edge from the graph.
dvDelEdge :: DvId -> DvWindow -> Success
dvDelEdge edge dvwin =
  send (DvCmdMsg (DvGraphCmd (DvUpdate [] [DvDeleteEdge edge]))) dvwin

--- An event handler that changes the event handler of a node or edge.
dvSetClickHandler :: DvId -> (DvWindow->Success) -> DvWindow -> Success
dvSetClickHandler (DvId dvid) handler dvwin =
  send (DvSetEvent dvid (DvClick handler)) dvwin

--- The "empty" event handler.
dvEmptyH :: DvWindow -> Success
dvEmptyH _ = success

-----------------------------------------------------------------------------
-- end of daVinci interface
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- the implementation of the daVinci binding:
-----------------------------------------------------------------------------

-- internal representation of commands:

type DvType = String

--- The abstract datatype for identifying nodes in a graph.
--- Used by the various functions to create and manipulate graphs.
data DvId = DvId String

data DvCmd = DvGraphCmd DvGraphCmd | DvMenuCmd DvMenuCmd | DvNothing

data DvGraphCmd =
              DvNewGraph DvGraph
            | DvNewPlaced DvGraph
            | DvUpdate [DvNodeUpdate] [DvEdgeUpdate]
            | DvChangeAttr [DvAttrChange]
            | DvUpdateChangeAttr [DvNodeUpdate] [DvEdgeUpdate] [DvAttrChange]

data DvMenuCmd = DvFile DvFileCmd

data DvFileCmd = DvNew
               | DvOpenGraph String
               | DvOpenGraphPlaced String
               | DvOpenStatus String
               | DvSaveGraph String
               | DvSaveStatus String
               | DvPrint String
               | DvPrintNoName
               | DvClose
               | DvExit 

--- The abstract datatype for graphs represented by daVinci.
--- Such graphs are constructed from a list of nodes by the
--- function <code>dvNewGraph</code>.
data DvGraph = DvGraph [DvNode]

--- The abstract datatype for nodes in a graph represented by daVinci.
--- Nodes are constructed by the functions
--- <code>dvSimpleNode</code> and <code>dvNodeWithEdges</code>.
data DvNode = DvNode DvId DvType [DvAttribute] [DvEdge] [DvEventH]
            | DvNodeRef DvId

--- The abstract datatype for edges in a graph represented by daVinci.
--- Edges are constructed by the function <code>dvSimpleEdge</code>.
data DvEdge = DvEdge DvId DvType [DvAttribute] DvNode [DvEventH]

data DvEventH = DvClick (DvWindow -> Success)

data DvAttribute = DvAttribute String String

data DvNodeUpdate = DvDeleteNode DvId
                  | DvNewNode DvId DvType [DvAttribute] [DvEventH]

data DvEdgeUpdate = DvDeleteEdge DvId
                  | DvNewEdge DvId DvType [DvAttribute] DvId DvId [DvEventH]
                  | DvNewEdgeBehind DvId DvId DvType [DvAttribute] DvId DvId

data DvAttrChange = DvNodeAttrChange DvId [DvAttribute]
                  | DvEdgeAttrChange DvId [DvAttribute]

--- The abstract datatype for communicating with the daVinci
--- visualization tool. The constructors of this datatype
--- are not important since all communications are wrapped
--- in this library. The only relevant point is that
--- <code>Port DvScheduleMsg -> Success</code>
--- is the type of an event handler that can manipulate a graph
--- visualized by daVinci (see <code>dvSetNodeColor</code>,
--- <code>dvAddNode</code> etc).
data DvScheduleMsg = DvAnswer String
                   | DvCmdMsg DvCmd
                   | DvSetEvent String DvEventH

-- access to a window of daVinci (through the scheduler):
type DvWindow = Port DvScheduleMsg

-----------------------------------------------------------------------------
-- functions to translate the interface functions into the daVinci API:

dvCmd2DaVinci :: DvCmd -> Int -> (String,[(String,DvEventH)],Int)
dvCmd2DaVinci (DvGraphCmd cmd) idindex =
  let (ncmd,evs,nidindex) = dvGraphCmd2DaVinci cmd idindex 
   in (if ncmd=="" then "" else "graph("++ncmd++")", evs, nidindex)
dvCmd2DaVinci DvNothing idindex = ("nothing",[],idindex)

dvGraphCmd2DaVinci :: DvGraphCmd -> Int -> (String,[(String,DvEventH)],Int)
dvGraphCmd2DaVinci (DvChangeAttr changes) idindex =
   ("change_attr("++(dvChangeAttrs2DaVinci changes)++")",[],idindex)
dvGraphCmd2DaVinci (DvUpdate nups eups) idindex = (ncmd,nevs2,nidindex2)
  where (nnups,_,nidindex)      = dvNodeUpdates2DaVinci nups idindex
        (neups,nevs2,nidindex2) = dvEdgeUpdates2DaVinci eups nidindex
        ncmd = if nnups=="[]" && neups=="[]"
               then ""
               else "update("++nnups++","++neups++")"

dvChangeAttrs2DaVinci changes =
  "["++(concat (intersperse "," (map dvChangeAttr2DaVinci changes)))++"]"
dvChangeAttr2DaVinci (DvNodeAttrChange (DvId dvid) attrs) =
  "node(\""++dvid++"\","++(dvAttrs2DaVinci attrs)++")"
dvChangeAttr2DaVinci (DvEdgeAttrChange (DvId dvid) attrs) =
  "edge(\""++dvid++"\","++(dvAttrs2DaVinci attrs)++")"

dvAttrs2DaVinci attrs =
  "["++(concat (intersperse "," (map dvAttr2DaVinci attrs)))++"]"
 where
   dvAttr2DaVinci (DvAttribute key value) = "a(\""++key++"\",\""++value++"\")"

dvNodeUpdates2DaVinci :: [DvNodeUpdate] -> Int ->
                                               (String,[(String,DvEventH)],Int)
dvNodeUpdates2DaVinci nups idindex =
  dvList2DaVinci nups dvNodeUpdate2DaVinci idindex

dvNodeUpdate2DaVinci :: DvNodeUpdate -> Int -> (String,[(String,DvEventH)],Int)
dvNodeUpdate2DaVinci (DvDeleteNode (DvId dvid)) idindex =
  ("delete_node(\""++dvid++"\")",[],idindex)
dvNodeUpdate2DaVinci (DvNewNode dvid typ attrs evs) idindex
  | dvid=:=(DvId ("node"++(show idindex)))
   = ("new_node(\""++("node"++show idindex)++"\",\""++typ++"\","++
      (dvAttrs2DaVinci attrs)++")",
      dvEvents2DaVinci ("node"++(show idindex)) evs,
      idindex+1)

dvEdgeUpdates2DaVinci :: [DvEdgeUpdate] -> Int ->
                                          (String,[(String,DvEventH)],Int)
dvEdgeUpdates2DaVinci eups idindex =
  dvList2DaVinci eups dvEdgeUpdate2DaVinci idindex

dvEdgeUpdate2DaVinci :: DvEdgeUpdate -> Int -> (String,[(String,DvEventH)],Int)
dvEdgeUpdate2DaVinci (DvDeleteEdge (DvId dvid)) idindex =
   ("delete_edge(\""++dvid++"\")",[],idindex)
dvEdgeUpdate2DaVinci (DvNewEdge dvid typ attrs (DvId node1) (DvId node2) evs)
                     idindex
  | dvid=:=(DvId ("edge"++(show idindex))) = res
 where
   res = ("new_edge(\""++("edge"++show idindex)++"\",\""++typ++"\","++
          (dvAttrs2DaVinci attrs)++",\""++node1++"\",\""++node2++"\")",
          dvEvents2DaVinci ("edge"++show idindex) evs,
          idindex+1)

dvGraph2DaVinci :: DvGraph -> Int -> (String,[(String,DvEventH)],Int)
dvGraph2DaVinci (DvGraph nodes) idindex = dvNodes2DaVinci nodes idindex

dvNodes2DaVinci :: [DvNode] -> Int -> (String,[(String,DvEventH)],Int)
dvNodes2DaVinci nodes idindex = dvList2DaVinci nodes dvNode2DaVinci idindex

dvNode2DaVinci :: DvNode -> Int -> (String,[(String,DvEventH)],Int)
dvNode2DaVinci (DvNode dvid typ attrs edges evs) idindex
  | dvid=:=(DvId ("node"++show idindex)) = res
 where
   (ncmd,nevs,nidindex) = dvEdges2DaVinci edges (idindex+1)
   res = ("l(\""++("node"++show idindex)++"\",n(\""++typ++"\","++
          (dvAttrs2DaVinci attrs)++","++ncmd++"))",
          (dvEvents2DaVinci ("node"++show idindex) evs)++nevs,
          nidindex)
dvNode2DaVinci (DvNodeRef (DvId dvid)) idindex =
  ("r(\""++dvid++"\")",[],idindex)

dvEdges2DaVinci :: [DvEdge] -> Int -> (String,[(String,DvEventH)],Int)
dvEdges2DaVinci edges idindex = dvList2DaVinci edges dvEdge2DaVinci idindex

dvEdge2DaVinci :: DvEdge -> Int -> (String,[(String,DvEventH)],Int)
dvEdge2DaVinci (DvEdge dvid typ attrs node evs) idindex
  | dvid =:= (DvId ("edge"++(show idindex))) = res
 where
   (ncmd,_,nidindex) = dvNode2DaVinci node (idindex+1)
   res = ("l(\""++("edge"++show idindex)++"\",e(\""++typ++"\","++
                             (dvAttrs2DaVinci attrs)++","++ncmd++"))",
          dvEvents2DaVinci ("edge"++show idindex) evs,
          nidindex)

dvEvents2DaVinci _ [] = []
dvEvents2DaVinci dvid (e:es) = (dvid,e):(dvEvents2DaVinci dvid es)

dvList2DaVinci :: [a] -> (a->Int->(String,[(String,DvEventH)],Int)) -> Int
                      -> (String,[(String,DvEventH)],Int)
dvList2DaVinci list f ididx = aux list "[" [] ididx
  where aux []     cmd evs idindex = (cmd++"]",evs,idindex)
        aux (x:xs) cmd evs idindex =
            let (ncmd,nevs,nidindex) = f x idindex
                ncmd2 = if (cmd=="[") || (ncmd=="") then cmd++ncmd
                                                    else cmd++","++ncmd
             in aux xs ncmd2 (evs++nevs) nidindex

-----------------------------------------------------------------------------
-- initialization and start functions:

openDV :: IO (Port SP_Msg)
openDV = openProcessPort dvStartCmd

initDvSchedule :: DvGraph -> Port SP_Msg -> (DvWindow->Success) -> Success
initDvSchedule initgraph dvport initcmd =
  let p,s,ans1,ans2 free in
    send (SP_GetLine ans1) dvport &>
    send (SP_Put ("graph(new("++cmd++"))")) dvport &>
    send (SP_GetLine ans2) dvport &>
    openPort p s &>
    initcmd p &>
    ((dvSchedule (map ensureNotFree (ensureSpine s)) evs idindex p dvport) &
     (dvAdaptor p dvport))
  where (cmd,evs,idindex) = dvGraph2DaVinci initgraph 0

-----------------------------------------------------------------------------
-- scheduler and adaptor

dvSchedule :: [DvScheduleMsg] -> [([Char],DvEventH)] -> Int -> DvWindow
                                                     -> Port SP_Msg -> Success
dvSchedule ((DvCmdMsg c):msgs) evs idindex sport dvport =
  let (ncmd,nevs,nidindex) = dvCmd2DaVinci c idindex 
   in (if ncmd=="" then success 
                   else send (SP_Put ncmd) dvport) &>
      dvSchedule msgs (nevs++evs) nidindex sport dvport

dvSchedule ((DvSetEvent dvid (DvClick newh)):msgs) evs idindex sport dvport =
  dvSchedule msgs (updateEventHandlers dvid "click" newh evs)
             idindex sport dvport

dvSchedule ((DvAnswer s):msgs) evs idindex sport dvport
  | s=="ok" = dvSchedule msgs evs idindex sport dvport
  | s=="quit" = success
  | takeWhile (/='(') s == "node_selections_labels"
   = foldr (&>) success
           (map (\h->h sport)
                (selectEventHandlers (dvParseNodes (dropWhile (/='(') s))
                                                   evs "click")) &>
     dvSchedule msgs evs idindex sport dvport
  | takeWhile (\c->c/='(') s=="edge_selection_label"
   = let handlers = selectEventHandlers [takeWhile (/='\"')
                             (tail (tail (dropWhile (/='(') s)))] evs "click" 
     in (if handlers==[] then success else (head handlers) sport) &>
        dvSchedule msgs evs idindex sport dvport
  | otherwise = dvSchedule msgs evs idindex sport dvport


dvAdaptor sport dvport | send (SP_GetLine ans) dvport = 
  send (DvAnswer ans) sport &>
  if ans=="quit" then success else dvAdaptor sport dvport
 where
    ans free

-----------------------------------------------------------------------------
-- auxiliary functions

selectEventHandlers dvids evs evtype =
   concatMap mapFun (filter (\(dvid,_)->(dvid `elem` dvids)) evs)
  where
    mapFun (_,DvClick handler) | evtype=="click" = [handler]
                               | otherwise       = []

updateEventHandlers dvid "click" newh evs =
  (dvid,DvClick newh):(deleteEventHandler dvid "click" evs)

deleteEventHandler _ _ [] = []
deleteEventHandler dvid1 evtype ((dvid2,DvClick handler):evs)
   | dvid1==dvid2 && evtype=="click" = evs
   | otherwise = (dvid2,DvClick handler):(deleteEventHandler dvid1 evtype evs)

dvParseNodes s = dvParseNodeList (tail (tail s))
dvParseNodeList s
   | s==")" = []
   | otherwise = takeWhile compare1 (tail s)
                  : dvParseNodeList (tail (dropWhile compare2 s))
  where compare1 c = c/='\"' && c/=']'
        compare2 c = c/=',' && c/=']'

-----------------------------------------------------------------------------
-- end of DaVinci
-----------------------------------------------------------------------------
