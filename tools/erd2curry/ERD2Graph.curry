---------------------------------------------------------------------
--- This module defines an operation to visualize an ERD term with dot.
---------------------------------------------------------------------

module ERD2Graph(viewERD) where

import IO
import IOExts
import Char(isAlphaNum)
import List(intersperse)
import ERD

test = viewERD erd1

-- Should a relation represented as an explicit node?
-- If not, it will be represented as an arc with a label.
-- However, some graph drawing tools have problems to write the
-- labels in a good manner to the arcs.
relationAsNode = True

-- Visualize an ERD term with dot.
viewERD :: ERD -> IO ()
viewERD = viewDot . showDotGraph . erd2dot

-- translate dependencies into DOT language:
erd2dot :: ERD -> DotGraph
erd2dot (ERD erdname ens rels) =
  Graph erdname (enodes++concat rnodes) (concat redges)
 where
  enodes = map entity2dot ens
  (rnodes,redges) = unzip (map relationship2dot rels)

  entity2dot (Entity ename attrs) =
    Node ename [("shape","record"),("style","bold"),
                ("label","{"++ename ++ "|" ++
                      concat (intersperse ("\\n") (map showAttr attrs))++"}")]

  showAttr (Attribute aname dom key isnull) =
    aname ++ " :: " ++ showDomain dom ++
    (if key==NoKey then "" else " / "++show key) ++
    (if isnull then " / null" else "")

  showDomain (IntDom _)        = "Int"
  showDomain (FloatDom _)      = "Float"
  showDomain (CharDom _)       = "Char"
  showDomain (StringDom _)     = "String"
  showDomain (BoolDom _)       = "Bool"
  showDomain (DateDom _)       = "Date"
  showDomain (UserDefined t _) = t
  showDomain (KeyDom _)        = "KeyDom"

  relationship2dot (Relationship rname [REnd en1 r1 c1, REnd en2 r2 c2]) =
    if relationAsNode
    then ([Node rname [("shape","diamond"),("style","filled")],
           Node (rname++r1) [("shape","plaintext"),("label",r1++"\\n"++showCard c1)],
           Node (rname++r2) [("shape","plaintext"),("label",r2++"\\n"++showCard c2)]],
          map (\ (n1,n2) -> Edge n1 n2 [("dir","none")])
              [(rname,rname++r1),(rname++r1,en1),
               (rname,rname++r2),(rname++r2,en2)])
    else ([Node rname [("shape","diamond"),("style","filled")]],
          [Edge rname en1 [("dir","none"),("label",r1++"\\n"++showCard c1)],
           Edge rname en2 [("dir","none"),("label",r2++"\\n"++showCard c2)]])

  showCard (Exactly n) = '(' : show n ++ "," ++ show n ++ ")"
  showCard (Between n Infinite) = '(' : show n ++ ",n)"
  showCard (Between n (Max m)) = '(' : show n ++ "," ++ show m ++ ")"

data DotGraph = Graph String [Node] [Edge]
data Node = Node String [(String,String)]
data Edge = Edge String String [(String,String)]

showDotGraph :: DotGraph -> String
showDotGraph (Graph name nodes edges) =
  "digraph "++name++"{\n" ++
  concatMap node2dot nodes ++ concatMap edge2dot edges ++ "}\n"
 where
  node2dot (Node nname attrs) =
    if null attrs
    then showDotID nname ++ ";\n"
    else showDotID nname ++
            '[':concat (intersperse ","
                           (map (\ (n,v)->n++"=\""++v++"\"") attrs)) ++ "]"
                        ++ ";\n"

  edge2dot (Edge i j attrs) =
    showDotID i ++ " -> " ++ showDotID j ++
    (if null attrs then "" else
       '[':concat (intersperse ","
                     (map (\ (n,v)->n++"=\""++v++"\"") attrs)) ++ "]")
    ++ ";\n"

  showDotID s | all isAlphaNum s = s
              | otherwise = '"' : concatMap escapeDQ s ++ "\""
   where escapeDQ c = if c=='"' then "\\\"" else [c]

-- visualize a DOT string:
viewDot :: String -> IO ()
viewDot dottxt = do
    writeFile "/home/mh/tmp.dot" dottxt
    dotstr <- connectToCommand dotCmd
    hPutStr dotstr dottxt
    hClose dotstr

dotCmd = "dot -Tps | kghostview -"
--dotCmd = "neato -Tps | kghostview -"
--dotCmd = "circo -Tps | kghostview -"
--dotCmd = "fdp -Tps | kghostview -"

erd1 =
 ERD "Uni"
   [Entity "Student"
           [Attribute "MatNum" (IntDom Nothing) PKey False,
            Attribute "Name" (StringDom Nothing) NoKey False,
            Attribute "Firstname" (StringDom Nothing) NoKey False,
            Attribute "Email" (UserDefined "MyModule.Email" Nothing) NoKey True],
    Entity "Lecture"
           [Attribute "Id" (IntDom Nothing) PKey False,
            Attribute "Title" (StringDom Nothing) Unique False,
            Attribute "Hours" (IntDom (Just 4)) NoKey False],
    Entity "Lecturer"
           [Attribute "Id" (IntDom Nothing) PKey False,
            Attribute "Name" (StringDom Nothing) NoKey False,
            Attribute "Firstname" (StringDom Nothing) NoKey False],
    Entity "Group"
           [Attribute "Time" (StringDom Nothing) NoKey False]]
   [Relationship "Teaching"
                 [REnd "Lecturer" "taught_by" (Exactly 1),
                  REnd "Lecture" "teaches" (Between 0 Infinite)],
    Relationship "Participation"
                 [REnd "Student" "participated_by" (Between 0 Infinite),
                  REnd "Lecture" "participates" (Between 0 Infinite)],
    Relationship "Membership"
                 [REnd "Student" "consists_of" (Exactly 3),
                  REnd "Group" "member_of" (Between 0 Infinite)]]
