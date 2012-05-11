------------------------------------------------------------------------------
--- Library for inductive graphs (port of a Haskell library by Martin Erwig).
---
--- In this library, graphs are composed and decomposed in an inductive way.
---
--- The key idea is as follows: 
---
--- A graph is either <i>empty</i> or it consists of <i>node context</i> 
--- and a <i>graph g'</i> which 
--- are put together by a constructor `(:&amp;)`.
---
--- This constructor `(:&amp;)`, however, is not a constructor in 
--- the sense of abstract 
--- data type, but more basically a defined constructing funtion. 
---
--- A <i>context</i> is a node together withe the edges to and from this node
--- into the nodes in the graph g'.
--- 
--- For examples of how to use this library, cf. the module `GraphAlgorithms`.
---
--- @author Bernd Brassel
--- @version May 2005
------------------------------------------------------------------------------


module GraphInductive (
  empty,
  mkGraph,
  buildGr,
  mkUGraph,

  (:&),
  insNode, insNodes,
  insEdge, insEdges,
  delNode, delNodes,
  delEdge, delEdges,
  
  isEmpty,
  match,
  matchAny,
  noNodes,
  nodeRange,
  context,
  lab,
  neighbors,
  suc,lsuc,
  pre,lpre,
  out,outdeg,
  inn,indeg,
  deg, 
  gelem,
  equal,

  node',
  lab',
  labNode',
  neighbors',
  suc',lsuc',
  pre',lpre',
  out',outdeg',
  inn',indeg',
  deg',

  labNodes,
  labEdges,
  nodes,
  edges,
  newNodes,
  
  ufold, 
  gmap,nmap,emap,
  labUEdges,labUNodes,

  showGraph,

  Graph,
  Node,LNode,UNode,
  Edge,LEdge,UEdge,
  Context,MContext,Context',UContext,
  GDecomp,Decomp,UDecomp,
  
  Path,LPath,UPath,
  UGr) where

import FiniteMap
import Sort (mergeSort)
import Maybe 

infixr 5 .:


---------------------------------------
--- Graph composition
---------------------------------------

infixr 5 :&


--- (:&amp;) takes a node-context and a Graph and yields a new graph.
---
--- The according key idea is detailed at the beginning.
--- 
--- nl is the type of the node labels and el the edge labels.
---
--- Note that it is an error to induce a context for 
--- a node already contained in the graph.

(:&) :: Context nl el -> Graph nl el -> Graph nl el
(p,v,l,s) :& (Gr g) 
  | elemFM v g = error ("Node Exception, Node: "++show v++": "++show l)
  | otherwise  = Gr g3
      where g1 = addToFM g v (p,l,s)
            g2 = updAdj g1 p (addSucc v)
            g3 = updAdj g2 s (addPred v)

--- The type variables of Graph are <i>nodeLabel</i> and <i>edgeLabel</i>.
--- The internal representation of Graph is hidden.

data Graph nodeLabel edgeLabel = Gr (GraphRep nodeLabel edgeLabel)

--- Nodes and edges themselves (in contrast to their labels) are coded as integers.
---
--- For both of them, there are variants as labeled, unlabelwd and quasi unlabeled 
--- (labeled with ()).
---
-- Nodes and their labels
---
--- Unlabeled node
type  Node   = Int              
--- Labeled node
type LNode a = (Node,a)         
--- Quasi-unlabeled node
type UNode   = LNode ()

-- Edges and their labels

--- Unlabeled edge
type  Edge   = (Node,Node)      
--- Labeled edge
type LEdge b = (Node,Node,b)    
--- Quasi-unlabeled edge
type UEdge   = LEdge ()         

--- The context of a node is the node itself (along with label) and its adjacent nodes.
--- Thus, a context is a quadrupel, for node n it is of the form
--- (edges to n,node n,n's label,edges from n)

type Context a b  = (Adj b,Node,a,Adj b) -- Context a b "=" Context' a b "+" Node

--- Labeled links to or from a 'Node'.
type Adj b = [(b,Node)]

-- there are some useful variants of the context type 
--- maybe context
type MContext a b = Maybe (Context a b)
--- context with edges and node label only, without the node identifier itself
type Context' a b = (Adj b,a,Adj b)
--- Unlabeled context.
type UContext     = ([Node],Node,[Node])

------------------------------------
-- graph decomposition
------------------------------------

--- decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
--- and the remaining 'Graph'.
--- 
--- In order to use graphs as abstract data structures, we also need means to
--- decompose a graph. This decompostion should work as much like pattern matching
--- as possible. The normal matching is done by the function matchAny, which takes
--- a graph and yields a graph decompostion.
---
--- According to the main idea, matchAny . (:&amp;) should be an identity.

matchAny  :: Graph a b -> GDecomp a b
matchAny (Gr g)
  | isEmptyFM g = error "Match Exception, Empty Graph"
  | otherwise = case head (fmToListPreOrder g) of
                  (v,_) -> case match v (Gr g) of
                            (Just c,g') -> (c,g') 

--- A graph decompostion is a context for a node n and the remaining graph without
--- that node.

type GDecomp a b  = (Context a b,Graph a b)

--- a decomposition with a maybe context 
type Decomp a b = (MContext a b,Graph a b)

--- Unlabeled decomposition.
type UDecomp g    = (Maybe UContext,g)

----------------------------------------------------------------------
-- basic graph operations
----------------------------------------------------------------------

----------------------------------
-- creating graphs
----------------------------------

--- An empty 'Graph'.
-- internal representation by finite maps
empty :: Graph _ _
empty = Gr (emptyFM (<))

--- Create a 'Graph' from the list of 'LNode's and 'LEdge's.
mkGraph   :: [LNode a] -> [LEdge b] -> Graph a b
mkGraph vs es  = (insEdges es . insNodes vs) empty

--- Build a 'Graph' from a list of 'Context's.
buildGr ::  [Context a b] -> Graph a b
buildGr = foldr (:&) empty

--- Build a quasi-unlabeled 'Graph' from the list of 'Node's and 'Edge's.
mkUGraph ::  [Node] -> [Edge] -> Graph () ()
mkUGraph vs es = mkGraph (labUNodes vs) (labUEdges es) 

----------------------------------------------
-- adding to and deleting from graphs
----------------------------------------------


--- Insert a 'LNode' into the 'Graph'.
insNode ::  LNode a -> Graph a b -> Graph a b
insNode (v,l) = (([],v,l,[]):&)

--- Insert a 'LEdge' into the 'Graph'.
insEdge ::  LEdge b -> Graph a b -> Graph a b
insEdge (v,w,l) g = (pr,v,la,(l,w):su) :& g'
                    where (Just (pr,_,la,su),g') = match v g

--- Remove a 'Node' from the 'Graph'.
delNode ::  Node -> Graph a b -> Graph a b
delNode v = delNodes [v]

--- Remove an 'Edge' from the 'Graph'.
delEdge ::  Edge -> Graph a b -> Graph a b
delEdge (v,w) g = case match v g of
                  (Nothing,_)        -> g
                  (Just (p,v',l,s),g') -> (p,v',l,filter ((/=w).snd) s) :& g'

--- Insert multiple 'LNode's into the 'Graph'.
insNodes   ::  [LNode a] -> Graph a b -> Graph a b
insNodes vs g = foldr insNode g vs

--- Insert multiple 'LEdge's into the 'Graph'.
insEdges ::  [LEdge b] -> Graph a b -> Graph a b
insEdges es g = foldr insEdge g es

--- Remove multiple 'Node's from the 'Graph'.
delNodes ::  [Node] -> Graph a b -> Graph a b
delNodes []     g = g
delNodes (v:vs) g = delNodes vs (snd (match v g))  

--- Remove multiple 'Edge's from the 'Graph'.
delEdges ::  [Edge]    -> Graph a b -> Graph a b
delEdges es g = foldr delEdge g es


-----------------------------------------
-- retrieving information about graphs
-----------------------------------------

--- test if the given 'Graph' is empty.
isEmpty :: Graph _ _ -> Bool
isEmpty (Gr g)  = isEmptyFM g

--- match is the complement side of (:&amp;), decomposing a 'Graph' into the 
--- 'MContext' found for the given node and the remaining 'Graph'.
match     :: Node -> Graph a b -> Decomp a b
match v (Gr g) = 
  maybe 
   (Nothing,Gr g) 
   (\ (g',(_,(p,l,s))) ->
             let s'   = filter ((/=v) . snd) s
                 p'   = filter ((/=v) . snd) p
                 g1   = updAdj g' s' (clearPred v)
                 g2   = updAdj g1 p' (clearSucc v)
              in (Just (p',v,l,s),Gr g2))
   (splitFM g v)



--- The number of 'Node's in a 'Graph'.
noNodes   :: Graph _ _ -> Int
noNodes   (Gr g) = sizeFM g

--- The minimum and maximum 'Node' in a 'Graph'.
nodeRange :: Graph _ _ -> (Node,Node)
nodeRange (Gr g) | isEmptyFM g = (0,0)
                 | otherwise = (ix (minFM g),ix (maxFM g)) where ix = fst . fromJust


--- Find the context for the given 'Node'.  In contrast to "match",
--- "context" causes an error if the 'Node' is
--- not present in the 'Graph'.
context ::  Graph a b -> Node -> Context a b
context g v = case match v g of
                (Nothing,_) -> error ("Match Exception, Node: "++show v)
                (Just c,_)  -> c 

--- Find the label for a 'Node'.
lab ::  Graph a _ -> Node -> Maybe a
lab g v = fst (match v g) >>- Just . lab' 

--- Find the neighbors for a 'Node'.
neighbors ::  Graph _ _ -> Node -> [Node] 
neighbors = (\(p,_,_,s) -> map snd (p++s)) .: context

--- Find all 'Node's that have a link from the given 'Node'.
suc ::  Graph _ _ -> Node -> [Node]
suc = map snd .: context4

--- Find all 'Node's that link to to the given 'Node'.
pre ::  Graph _ _ -> Node -> [Node] 
pre = map snd .: context1

--- Find all Nodes and their labels, which are linked from the given 'Node'.
lsuc ::  Graph _ b -> Node -> [(Node,b)]
lsuc = map flip2 .: context4

--- Find all 'Node's that link to the given 'Node' and the label of each link.
lpre ::  Graph _ b -> Node -> [(Node,b)] 
lpre = map flip2 .: context1

--- Find all outward-bound 'LEdge's for the given 'Node'.
out ::  Graph _ b -> Node -> [LEdge b] 
out g v = map (\(l,w)->(v,w,l)) (context4 g v)

--- Find all inward-bound 'LEdge's for the given 'Node'.
inn ::  Graph _ b -> Node -> [LEdge b] 
inn g v = map (\(l,w)->(w,v,l)) (context1 g v)

--- The outward-bound degree of the 'Node'.
outdeg ::  Graph _ _ -> Node -> Int
outdeg = length .: context4

--- The inward-bound degree of the 'Node'.
indeg ::  Graph _ _ -> Node -> Int
indeg  = length .: context1

--- The degree of the 'Node'.
deg ::  Graph _ _ -> Node -> Int
deg = (\(p,_,_,s) -> length p+length s) .: context

--- 'True' if the 'Node' is present in the 'Graph'.
gelem ::  Node -> Graph _ _ -> Bool
gelem v g = isJust (fst (match v g)) 
           

--- graph equality
equal :: Graph a b -> Graph a b -> Bool
equal g g' = slabNodes g == slabNodes g' && slabEdges g == slabEdges g'

-- comparing nodes 
nodeComp :: LNode b -> LNode b -> Ordering
nodeComp n n' | n == n'      = EQ
              | fst n<fst n' = LT
              | otherwise    = GT

-- sort contained nodes 
slabNodes :: Graph a _ -> [LNode a]
slabNodes = sortBy nodeComp . labNodes

-- comparing edges
edgeComp :: LEdge b -> LEdge b -> Ordering
edgeComp e e' | e == e'              = EQ
              | v<x || (v==x && w<y) = LT
              | otherwise            = GT

  where
    (v,w,_) = e
    (x,y,_) = e'

-- sort contained edges
slabEdges :: Graph _ b -> [LEdge b]
slabEdges = sortBy edgeComp . labEdges

-------------------------------------------
-- retrieving information from contexts
-------------------------------------------

--- The 'Node' in a 'Context'.
node' :: Context _ _ -> Node
node' (_,v,_,_) = v

--- The label in a 'Context'.
lab' :: Context a _ -> a
lab' (_,_,l,_) = l

--- The 'LNode' from a 'Context'.
labNode' :: Context a _ -> LNode a
labNode' (_,v,l,_) = (v,l)

--- All 'Node's linked to or from in a 'Context'.
neighbors' :: Context _ _ -> [Node] 
neighbors' (p,_,_,s) = map snd p++map snd s

--- All 'Node's linked to in a 'Context'.
suc' :: Context _ _ -> [Node]
suc' (_,_,_,s) = map snd s

--- All 'Node's linked from in a 'Context'.
pre' :: Context _ _ -> [Node] 
pre' (p,_,_,_) = map snd p

--- All 'Node's linked from in a 'Context', and the label of the links.
lpre' :: Context _ b -> [(Node,b)] 
lpre' (p,_,_,_) = map flip2 p

--- All 'Node's linked from in a 'Context', and the label of the links.
lsuc' :: Context _ b -> [(Node,b)]
lsuc' (_,_,_,s) = map flip2 s

--- All outward-directed 'LEdge's in a 'Context'.
out' :: Context _ b -> [LEdge b] 
out' (_,v,_,s) = map (\(l,w)->(v,w,l)) s

--- All inward-directed 'LEdge's in a 'Context'.
inn' :: Context _ b -> [LEdge b] 
inn' (p,v,_,_) = map (\(l,w)->(w,v,l)) p

--- The outward degree of a 'Context'.
outdeg' :: Context _ _ -> Int
outdeg' (_,_,_,s) = length s

--- The inward degree of a 'Context'.
indeg' :: Context _ _ -> Int
indeg' (p,_,_,_) = length p

--- The degree of a 'Context'.
deg' :: Context _ _ -> Int
deg' (p,_,_,s) = length p+length s

------------------------------------
-- listifying graphs
------------------------------------

--- A list of all 'LNode's in the 'Graph'.
labNodes (Gr g) = map (\(v,(_,l,_))->(v,l)) (fmToList g)

--- A list of all 'LEdge's in the 'Graph'.
labEdges  :: Graph _ b -> [LEdge b]
labEdges  (Gr g) = concatMap (\(v,(_,_,s))->map (\(l,w)->(v,w,l)) s) (fmToList g)

--- List all 'Node's in the 'Graph'.
nodes ::  Graph _ _ -> [Node]
nodes = map fst . labNodes

--- List all 'Edge's in the 'Graph'.
edges ::  Graph _ _ -> [Edge]
edges = map (\(v,w,_)->(v,w)) . labEdges

--- List N available 'Node's, ie 'Node's that are not used in the 'Graph'.
newNodes ::  Int -> Graph _ _ -> [Node]
newNodes i g = [n+1..n+i] where (_,n) = nodeRange g

------------------------------------
-- some convenient type synonyms
------------------------------------

-- Paths and their labels
--- Unlabeled path
type Path    = [Node]           
--- Labeled path
type LPath a = [LNode a]
--- Quasi-unlabeled path
type UPath   = [UNode]          

type GraphRep a b = FM Node (Context' a b)

--- a graph without any labels
type UGr = Graph () ()

------------------------
-- Functions on Graphs
------------------------

--- Fold a function over the graph.
ufold :: ((Context a b) -> c -> c) -> c -> Graph a b -> c
ufold f u g | isEmpty g = u
            | otherwise = f c (ufold f u g') 
            where (c,g') = matchAny g

--- Map a function over the graph.
gmap :: (Context a b -> Context c d) -> Graph a b -> Graph c d
gmap f = ufold (\c->((f c):&)) empty

--- Map a function over the 'Node' labels in a graph.
nmap ::  (a -> c) -> Graph a b -> Graph c b
nmap f = gmap (\(p,v,l,s)->(p,v,f l,s))

--- Map a function over the 'Edge' labels in a graph.
emap ::  (b -> c) -> Graph a b -> Graph a c
emap f = gmap (\(p,v,l,s)->(map1 f p,v,l,map1 f s))
         where map1 g = map (\(l,v)->(g l,v))

--- add label () to list of edges (node,node)
labUEdges = map (\(v,w)->(v,w,()))

--- add label () to list of nodes
labUNodes = map (\v->(v,()))
 
----------------------------------------------------------------------
-- textual Graph representation 
----------------------------------------------------------------------

--- Represent Graph as String
showGraph :: Graph _ _ -> String
showGraph (Gr g) = unlines (map showNode (fmToList g))

--  
showNode (v,(_,l',s)) = show v ++ ":" ++ show l' ++ "->"++ show s 

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------
-- auxiliary functions used in the implementation of the 
-- derived class members
-- 
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)

fst4 (x,_,_,_) = x
{- not used
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x
-}
fth4 (_,_,_,x) = x

{- not used
fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
-}

flip2 (x,y) = (y,x)

-- projecting on context elements
--
-- context1 g v = fst4 (contextP g v)
context1 :: Graph _ b -> Node -> Adj b
{- not used
context2 :: Graph gr => gr a b -> Node -> Node
context3 :: Graph gr => gr a b -> Node -> a
-}
context4 :: Graph _ b -> Node -> Adj b

context1 = fst4 .: context
{- not used
context2 = snd4 .: context
context3 = thd4 .: context
-}
context4 = fth4 .: context

addSucc v l (p,l',s) = (p,l',(l,v):s)
addPred v l (p,l',s) = ((l,v):p,l',s)

clearSucc v _ (p,l,s) = (p,l,filter ((/=v).snd) s)
clearPred v _ (p,l,s) = (filter ((/=v).snd) p,l,s)

updAdj :: GraphRep a b -> Adj b -> (b -> Context' a b -> Context' a b) -> GraphRep a b
updAdj g []         _              = g
updAdj g ((l,v):vs) f 
   | elemFM v g = updAdj (updFM g v (f l)) vs f
   | otherwise  = error ("Edge Exception, Node: "++show v)

sortBy p = mergeSort (\x y -> let pxy = p x y in pxy==EQ||pxy==LT)



