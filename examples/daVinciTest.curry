-----------------------------------------------------------------------------
-- Some examples for the use of the DaVinci library
-----------------------------------------------------------------------------

import DaVinci

-- display a simple graph with 3 nodes and 2 edges:
test1 = dvDisplay $
          dvNewGraph [dvNodeWithEdges node1 "Node1"
                             [dvSimpleEdge edge1 node3 dvEmptyH] dvEmptyH,
                      dvNodeWithEdges node2 "Node2"
                             [dvSimpleEdge edge2 node3 dvEmptyH] dvEmptyH,
                      dvSimpleNode node3 "Node3" dvEmptyH]
         where
           node1,node2,node3,edge1,edge2 free


-- display a simple cyclic graph with 2 nodes and 3 edges
test2 = dvDisplay $
          dvNewGraph [dvNodeWithEdges node1 "Node1"
                             [dvSimpleEdge edge1 node2 dvEmptyH] dvEmptyH,
                      dvNodeWithEdges node2 "Node2"
                             [dvSimpleEdge edge2 node1 dvEmptyH,
                              dvSimpleEdge edge3 node2 dvEmptyH] dvEmptyH]
         where
           node1,node2,edge1,edge2,edge3 free


-- display a simple graph and color the nodes with an
-- initialization handler:
test3 = dvDisplayInit
          (dvNewGraph [dvNodeWithEdges node1 "Node1"
                              [dvSimpleEdge edge1 node2 dvEmptyH] dvEmptyH,
                       dvNodeWithEdges node2 "Node2"
                              [dvSimpleEdge edge2 node2 dvEmptyH] dvEmptyH])
          (\dp->dvSetNodeColor node1 "red"  dp &>
                dvSetNodeColor node2 "blue" dp)
         where
           node1,node2,edge1,edge2 free


-- display a trivial graph with one node with can be extended by
-- clicking at the node:
test4 = dvDisplay $ dvNewGraph [dvSimpleNode node1 "Node1" eventH]
 where
    node1 free

    eventH dvwin = let node2,edge free
                    in dvAddNode node2 "New Node" dvEmptyH dvwin &>
                       dvAddEdge edge node2 node1 dvEmptyH dvwin


-- display a simple graph with changing event handlers:
test5 = dvDisplay $
          dvNewGraph [dvNodeWithEdges node1 "Node1"
                                    [dvSimpleEdge edge1 node2 eventH3] eventH1,
                      dvSimpleNode node2 "Node2" dvEmptyH]
  where
    node1,node2,edge1 free

    eventH1 dvwin = dvSetNodeColor node1 "red" dvwin &>
                    dvSetClickHandler node1 eventH2 dvwin

    eventH2 dvwin = dvSetNodeColor node1 "blue" dvwin &>
                    dvSetClickHandler node1 eventH1 dvwin

    eventH3 dvwin = let edge2 free
                     in dvAddEdge edge2 node1 node2 dvEmptyH dvwin &>
                        dvSetClickHandler edge1 (eventH4 edge2) dvwin

    eventH4 edge dvwin = dvDelEdge edge dvwin &>
                         dvSetClickHandler edge1 eventH3 dvwin


--------------------------------------------------------------------------
-- Next we show the visualization of graphs with a different structure
-- by transforming them into daVinci graphs:

-- Here, a graph is a list of nodes and edges:
data Graph = Graph [Node] [Edge]

-- A node is identified by a string (we assume that these strings are unique):
data Node = Node String

-- An edge consists of a source and a target node (identified by their ids):
data Edge = Edge String String

-- Now it is simple to convert this representation into the daVinci
-- representation by grouping edges to their source node:
showGraph :: Graph -> IO ()
showGraph (Graph nodes edges) = dvDisplay $
  dvNewGraph 
   (map (\(dvid,label)->dvNodeWithEdges dvid label
                                        (map (\(Edge _ n2)->createEdge n2)
                                             (filter (\(Edge n1 _)->n1==label)
                                                     edges))
                                        dvEmptyH) idnodes)
 where
   idnodes = map node2idnode nodes

   createEdge tlabel =
              dvSimpleEdge e
                           (fst (head (filter (\(_,l)->l==tlabel) idnodes)))
                           dvEmptyH
              where e free

   -- transform node into node with a DvId:
   node2idnode (Node label) = (dvid,label)  where dvid free


-- ...and now a simple test:
testn1 = showGraph $
             Graph [Node "n1", Node "n2", Node "n3"]
                   [Edge "n1" "n2", Edge "n3" "n2", Edge "n1" "n3",
                    Edge "n3" "n3"]
