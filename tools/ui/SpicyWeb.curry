module SpicyWeb where  
  
import Json 
import Traversal 
import UI(Ref(..))
import HTML(CgiRef(..))

infixl 1 `onLoad`--, `withStyle`
--infixr 0 <>

showID (Ref (CgiRef r)) = r

data Doc a
  = Doc a   
  | OnLoad (Doc a) (Action () a)

d `onLoad` a = OnLoad d (a *> yield Null)

yieldDoc d = yieldIDs [iD d]  

--module Action (

infixl 4 !
infixl 3 <*>
infixr 2 <*, *>

data Action _ a = Action (UntypedAction a)

type UntypedEvent = String

data UntypedAction a
  = Apply (UntypedAction a) (UntypedAction a)
  | Seq Bool (UntypedAction a) (UntypedAction a)
  | YieldID [a] -- [ID]
  | Pure Json
  | Window
  | Event
  | Select (UntypedAction a) String Bool
  | Curryfy Int (UntypedAction a)
  | Observe Bool [a] -- [ID] 
      UntypedEvent (UntypedAction a)

--tAction :: Traversable UntypedAction UntypedAction
tAction act = 
  case act of
    Apply a b        -> ([a,b], \ [a',b'] -> Apply a' b')
    Select a prop b  -> ([a],   \ [a']    -> Select a' prop b)
    Seq first a b    -> ([a,b], \ [a',b'] -> Seq first a' b')
    Observe o is e a -> ([a],   \ [a']    -> Observe o is e a')
    Curryfy n a      -> ([a],   \ [a']    -> Curryfy n a')
    _                -> noChildren act

--yieldIDs :: [ID] -> Action _
yieldIDs is = Action (YieldID is)

--yield :: Json -> Action _
yield x = Action (Pure x)

--(<*>) :: Action (a -> b) -> Action a -> Action b
Action f <*> Action a = Action (Apply f a) 

--(*>) :: Action _ -> Action a -> Action a
Action a *> Action b = Action (Seq False a b)

--(<*) :: Action a -> Action _ -> Action a
Action a <* Action b = Action (Seq True a b)

--(!) :: Action _ -> String -> Action _
(Action a) ! prop = Action (Select a prop False)

--(!.) :: Action _ -> String -> Action _
(Action a) !. prop = Action (Select a prop True)

--prim :: Int -> String -> Action _
prim n = curryfy n . foldl (!) window . words . map dot2space
 where
  dot2space c = if c=='.' then ' ' else c

--curryfy :: Int -> Action _ -> Action _
curryfy n (Action a) = Action (Curryfy n a)

--window :: Action _
window = Action Window

data EventRep = EventRep -- phantom type

--event :: Action EventRep
event = Action Event

--observe :: Bool -> [ID] -> UntypedEvent -> Action _ -> Action ()
observe b is e (Action a) = Action (Observe b is e a)


--module ActionUtils where




--actionIDs :: Action _ -> [ID]
actionIDs (Action a) =
  [ i | YieldID is <- acts, i <- is ] ++
  [ i | Observe _ is _ _ <- acts, i <- is ]
 where
  acts = family tAction a

--actionToJson :: UntypedAction -> Json
actionToJson = fold tAction toJson
 where
  toJson (Apply _ _) [a,b]
    = action "apply" [("fun",a),("arg",b)]
  toJson (Seq first _ _) [a,b]
    = action "seq" [("first",Bool first),("actions",Array [a,b])]
  toJson (YieldID is) []
    = action "elems" [("elems",Array (map (String . showID) is))]
  toJson (Pure js) []
    = action "pure" [("value",js)]
  toJson Window []
    = action "window" []
  toJson Event []
    = action "event" []
  toJson (Select _ prop b) [a]
    = action "select" [("obj",a),("prop",String prop),("bind",Bool b)]
  toJson (Curryfy arity _) [a]
    = action "curryfy" [("arity", Int arity),("fun",a)]
  toJson (Observe o is e _) [a]
    = action "observe"
        [("once",Bool o),("elems",Array (map (String . showID) is))
        ,("event",String e),("action",a)]

--action :: String -> [(String,Json)] -> Json
action kind args = Object (("kind",String kind):args)

--showAction :: Action _ -> String
showAction (Action a) = showJson (actionToJson a)

--showOnLoadAction :: Action _ -> String
showOnLoadAction act = "Action.performOnLoad(" ++ showAction act ++ ");"

--mapActionIDs :: ([ID] -> [ID]) -> UntypedAction -> UntypedAction
mapActionIDs f = mapFamily tAction fid
 where fid a = case a of
                 YieldID is -> YieldID (f is)
                 Observe b is x y -> Observe b (f is) x y
                 _ -> a


--elimNullActions :: Action a -> Action a
elimNullActions (Action act) = Action (evalFamily tAction elim act)
 where
  elim a = case a of
             Seq False (Pure Null) x -> Just x
             Seq True x (Pure Null) -> Just x
             _ -> Nothing

--module Event (

infixl 1 `onClick`, `onFirstClick`

iD (Doc d) = d

--observeAll, observeOne :: Doc -> UntypedEvent -> Action _ -> Action ()
observeAll d = observe False [iD d]
observeOne d = observe True  [iD d]

--onClick, onFirstClick :: Doc -> Action _ -> Doc
onClick d a = d `onLoad` observeAll d "click" a
onFirstClick d a = d `onLoad` observeOne d "click" a
  
-- module PrimActions where

--eventSourceAction :: Action (EventRep -> Doc)
eventSourceAction = prim 1 "Prim.eventSource"

--displayAction, hideAction :: Action (Doc -> Doc) 
displayAction = prim 1 "Prim.display"
hideAction = prim 1 "Prim.hide"

--display, hide :: Doc -> Action Doc
display d = displayAction <*> yieldDoc d
hide    d = hideAction <*> yieldDoc d

--addClassNameAction :: Action (Doc -> String -> Doc)
addClassNameAction = prim 2 "Prim.addClassName"

--addClassName :: Doc -> String -> Action Doc
addClassName d css = addClassNameAction <*> yieldDoc d <*> yield (String css)
