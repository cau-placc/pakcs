------------------------------------------------------------------------------
--- Library for creating web applications from gui descriptions
--- Same interface and documentation as the GUI library from PAKCS
---
--- @author Christof Kluss
--- @version July 2013
------------------------------------------------------------------------------

module GUI2HTML (
  GuiPort,GUI2HTML.Widget(..),Button,ConfigButton,
  TextEditScroll,ListBoxScroll,CanvasScroll,EntryScroll,
  ConfItem(..),ReconfigureItem(..),
  GUI2HTML.Cmd,GUI2HTML.Command,GUI2HTML.Event(..),ConfCollection(..),
  MenuItem(..),
  GUI2HTML.CanvasItem(..),
  WidgetRef, GUI2HTML.Style(..), GUI2HTML.Color(..),
  col,row,matrix,
  runGUI,runInitGUI,
  --runGUIwithParams,runInitGUI,runInitGUIwithParams,runPassiveGUI,
  --runControlledGUI,runConfigControlledGUI,runInitControlledGUI,
  --runHandlesControlledGUI,runInitHandlesControlledGUI,
  exitGUI,getValue,setValue,updateValue,appendValue,
  --appendStyledValue,addRegionStyle,removeRegionStyle,
  --getCursorPosition,
  seeText,
  focusInput,addCanvas,setConfig,
  --getOpenFile,getOpenFileWithTypes,
  getSaveFile --,
  --,getSaveFileWithTypes,
  --chooseColor,popup_message,debugTcl
 
)  where


import UI2HTML 
import qualified UI
import FunctionInversion(invf1)
import Maybe
import qualified HTML
import IOExts
import Read
import Char(isDigit,isSpace)

data Widget = 
   PlainButton             [ConfItem]
 | Canvas                  [ConfItem]
 | CheckButton             [ConfItem]
 | Entry                   [ConfItem]
 | Label                   [ConfItem]
 | ListBox                 [ConfItem]
 | Message                 [ConfItem]
 | MenuButton              [ConfItem]
 | Scale Int Int           [ConfItem]
 | ScrollH WidgetRef      [ConfItem]
 | ScrollV WidgetRef      [ConfItem]
 | TextEdit                [ConfItem]
 | Row    [ConfCollection] [Widget]
 | Col    [ConfCollection] [Widget] 
 | Matrix [ConfCollection] [[Widget]]


type UIRef = UI.Ref HTML.CgiRef

data ConfItem =
   Active Bool
 | Anchor String
 | Background String
 | Foreground String
 | Handler Event (Command) 
 | Height Int
 | CheckInit String
 | CanvasItems [CanvasItem]
 | List [String]
 | Menu [MenuItem]
 | WRef UIRef
 | Text String
 | Width Int 
 | Fill | FillX | FillY
 | TclOption String


type GuiPort = UI2HTML.UIEnv
type WidgetRef =  UI.Ref HTML.CgiRef 

type Command = GuiPort -> IO [ReconfigureItem]

data ReconfigureItem = 
   WidgetConf WidgetRef ConfItem
-- | StreamHandler Handle (Handle -> GuiPort -> IO [ReconfigureItem])
-- | RemoveStreamHandler Handle



--data WidgetRef = WReflabel GuiPort String String

data Event = 
   DefaultEvent
 | MouseButton1
 | MouseButton2
 | MouseButton3
 | KeyPress
 | Return


gui2uievent :: Event -> UI.Event
gui2uievent event = case event of
  MouseButton1 -> UI.MouseButton1
  MouseButton2 -> UI.MouseButton2
  MouseButton3 -> UI.MouseButton3
  KeyPress     -> UI.KeyPress
  Return       -> UI.Return
  _            -> UI.DefaultEvent



data MenuItem =
   MButton (GuiPort -> IO [ReconfigureItem]) String
 | MSeparator
 | MMenuButton String [MenuItem]

data ConfCollection =
   CenterAlign | LeftAlign | RightAlign | TopAlign | BottomAlign


data CanvasItem = 
    CLine [(Int,Int)] String
  | CPolygon [(Int,Int)] String
  | CRectangle (Int,Int) (Int,Int) String
  | COval (Int,Int) (Int,Int) String
  | CText (Int,Int) String String


Button :: (UIEnv -> IO _) -> [ConfItem] -> Widget
Button cmnd confs = PlainButton (Cmd cmnd : confs)



TextEditScroll :: [ConfItem] -> Widget
TextEditScroll = TextEdit
EntryScroll :: [ConfItem] -> Widget
EntryScroll = Entry
CanvasScroll :: [ConfItem] -> Widget
CanvasScroll = Canvas 

ConfigButton :: (UIEnv -> IO ([ReconfigureItem])) -> [ConfItem] -> Widget
ConfigButton cmnd = Button cmd2  
  where cmd2 x = do wconfs <- cmnd x
                    widgetconf2cmd wconfs x


Cmd :: (UIEnv -> IO _) -> ConfItem
Cmd cmnd = Handler DefaultEvent (\env -> cmnd env >> return [])  -- (UI2HTML.Cmd cmnd) 
Command :: (UIEnv -> IO ([ReconfigureItem])) -> ConfItem
Command cmnd = Handler DefaultEvent cmnd  

button = Button
command = Cmd
cmd = Command

data Style = Bold | Italic | Underline | Fg Color | Bg Color

--- The data type of possible colors.
data Color 
  = Black | Blue | Brown | Cyan | Gold | Gray | Green | Magenta | Navy | Orange
  | Pink | Purple | Red | Tomato| Turquoise | Violet | White | Yellow

-------------------------------------------------------------------------------


gui2ui :: Widget -> UIWidget
gui2ui widget = case widget of 
  PlainButton confitems -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                           in UI.Widget UI.Button
                                mbtext mbref hs (c2s confitems) []
  CheckButton confitems -> let (mbtext,mbref,hs) = (c2lrh confitems)
                               checked = (getCheckInit confitems == 1)
                           in UI.Widget (UI.CheckButton checked)
                                mbtext mbref hs (c2s confitems) []
  Entry confitems       -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                           in UI.Widget UI.Entry
                                mbtext mbref hs (c2s confitems) []
  Label confitems       -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                           in UI.Widget UI.Label   
                                mbtext mbref hs (c2s confitems) []
  Message confitems     -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                           in UI.Widget UI.Label    
                                mbtext mbref hs (c2s confitems) []
  Canvas confitems      -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                           in UI.Widget (UI.Canvas w h) 
                                mbtext mbref hs (c2s nconfs) []
    where
      h = fromMaybe 150 (getWidth confitems)
      w = fromMaybe 300 (getHeight confitems)
      nconfs = rmWidthHeight confitems

  TextEdit confitems    -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                           in UI.Widget (UI.TextEdit r c)
                                mbtext mbref hs (c2s nconfs) []
    where
      c = fromMaybe 50 (getWidth confitems)
      r = fromMaybe 10 (getHeight confitems)
      nconfs = rmWidthHeight confitems

  Scale min max confitems -> let (mbtext,mbref,hs) = (c2lrh confitems) 
                             in UI.Widget (UI.Scale min max) 
                                  mbtext mbref hs (c2s confitems) []

  ListBox confitems -> let (mbtext,mbref,hs) = (c2lrh confitems)
                       in UI.Widget 
                            (UI.ListBox size xs (getCheckInit confitems))
                            mbtext mbref hs (c2s confitems) []
    where
      xs = getList confitems
      size = fromMaybe 1 (getHeight confitems)

  MenuButton confitems -> 
    UI.menuBar [(UI.menu (fromJust (getText confitems)) 
                 (map menu2ui (fromJust $ getMenu confitems)))]
    where      
      menu2ui menuitem = case menuitem of   
        MButton (cmnd) label       -> UI.menuItem cmd2 label
          where 
            cmd2 x = do wconfs <- cmnd x
                        widgetconf2cmd wconfs x

        MMenuButton label items   -> UI.menu label (map menu2ui items) 
        MSeparator                -> UI.menuSeparator

  ScrollH _ _ ->  
    UI.label "GUI2HTML.gui2ui, ScrollH not implemented"
  ScrollV _ _ ->  
    UI.label "GUI2HTML.gui2ui, ScrollV not implemented"

  Row confs widgets -> UI.rowS (collconfs2s confs) (map gui2ui widgets)
  Col confs widgets -> UI.colS (collconfs2s confs) (map gui2ui widgets)

  Matrix confs widgetss -> UI.matrixS c (map (map gui2ui) widgetss)
    where c = collconfs2s confs

 where
    c2lrh = confitems2lrh

    c2s confitems = 
      let h = confitems2styleclass confitems in
      if null h then [] else [UI.Class h]  

    collconfs2s confs = 
      let h = collectionconf2styleclass confs in
      if null h then [] else [UI.Class h]

    confitems2lrh confitems = c2lrh' (Nothing,Nothing,[]) confitems 
      where        
       foo ref | ref =:= UI.Ref cref = ref 
         where cref free

       c2lrh' lrh []     = lrh
       c2lrh' (mblabel,mbref,handlers) (conf:confs) = case conf of
         Text label -> c2lrh' ((Just label),mbref,handlers) confs 
         WRef ref   -> c2lrh' (mblabel,(Just (foo ref)),handlers) confs

         Handler event cmnd -> c2lrh' 
            (mblabel,mbref,
             ((UI.Handler
                 (gui2uievent event) (UI.Cmd cmd2)):handlers)) 
            confs   
           where cmd2 x = do wconfs <- cmnd x
                             widgetconf2cmd wconfs x  
         _          -> c2lrh' (mblabel,mbref,handlers) confs

    confitems2styleclass confitems = c2s' confitems 
      where 
        c2s' []           = []
        c2s' (conf:confs) = case conf of
          Active b         -> UI.Active b:c2s' confs
          Background color -> UI.Bg (string2Color color):c2s' confs
          Foreground color -> UI.Fg (string2Color color):c2s' confs
          Fill             -> UI.Fill UI.Both:c2s' confs
          FillX            -> UI.Fill UI.X:c2s' confs
          FillY            -> UI.Fill UI.Y:c2s' confs
          Height n         -> UI.Height n:c2s' confs
          Width  n         -> UI.Width n:c2s' confs
          _                -> c2s' confs  


    collectionconf2styleclass collconfs = c2s' collconfs 
      where 
        c2s' []           = []
        c2s' (conf:confs) = case conf of
          CenterAlign -> UI.Align UI.Center:c2s' confs
          LeftAlign   -> UI.Align UI.Left:c2s' confs
          RightAlign  -> UI.Align UI.Right:c2s' confs
          TopAlign    -> UI.Align UI.Top:c2s' confs
          BottomAlign -> UI.Align UI.Bottom:c2s' confs
          _           -> c2s' confs



canvasgui2canvasui :: [CanvasItem] -> [UI.CanvasItem]
canvasgui2canvasui canvasitems = map conv canvasitems
  where
    conv (CLine points str)    = UI.CLine points str 
    conv (CPolygon points str) = UI.CPolygon points str
    conv (CRectangle x y str)  = UI.CRectangle x y str
    conv (COval x y str)       = UI.COval x y str
    conv (CText x str1 str2)   = UI.CText x str1 str2

getText :: [ConfItem] -> Maybe (String)
getText []     = Nothing
getText (x:xs) = case x of
  Text str -> Just str
  _        -> getText xs  


getWidth :: [ConfItem] -> Maybe Int
getWidth []     = Nothing
getWidth (x:xs) = case x of
  Width w -> Just w
  _       -> getWidth xs

getHeight :: [ConfItem] -> Maybe Int
getHeight []     = Nothing
getHeight (x:xs) = case x of
  Height w -> Just w
  _        -> getHeight xs 

getMenu :: [ConfItem] -> Maybe ([MenuItem])
getMenu []     = Nothing
getMenu (x:xs) = case x of
  Menu items -> Just items 
  _          -> getMenu xs

getList :: [ConfItem] -> [String]
getList []     = [] --Nothing
getList (x:xs) = case x of
  List items -> items -- Just items 
  _          -> getList xs
  
getCheckInit :: [ConfItem] -> Int -- Bool
getCheckInit []     = -1 --False
getCheckInit (x:xs) = case x of
  CheckInit str -> readInt str --if str == "1" then True else False 
  _             -> getCheckInit xs

-------------------------------------------------------------------------------

rmWidthHeight :: [ConfItem] -> [ConfItem]
rmWidthHeight []     = []
rmWidthHeight (x:xs) = case x of
    Height _ -> rmWidthHeight xs
    Width _  -> rmWidthHeight xs
    _        -> x:rmWidthHeight xs 

-------------------------------------------------------------------------------

--widgetconf2cmd :: [ReconfigureItem] -> GuiPort -> IO ()
widgetconf2cmd :: [ReconfigureItem] -> UIEnv -> IO ()
widgetconf2cmd [] _ = done
widgetconf2cmd ((WidgetConf ref confItem):items) x = do
    
  case confItem of    
    Active b      -> setDisabled ref (not b) x 
    --Anchor String
    Background str -> changeStyles ref [UI.Class [UI.Bg (string2Color str)]] x
    Foreground str -> changeStyles ref [UI.Class [UI.Fg (string2Color str)]] x
    --Handler event cmnd ->
    Handler event cmnd -> setHandler ref (gui2uievent event) cmd2 x 
      where 
        cmd2 env' = do
          wconfs <- cmnd env'
          widgetconf2cmd wconfs env'  
          done

    Height n -> changeStyles ref [UI.Class [UI.Height n]] x
    --CheckInit -> 
    --CanvasItems [CanvasItem]
    List l -> UI2HTML.setConfig ref (UI2HTML.List l) x 
    --Menu [MenuItem]
    --WRef Ref
    Text text -> setValue ref text x            
    --Width Int 
    Fill  -> changeStyles ref [UI.Class [UI.Fill Both]] x
    FillX -> changeStyles ref [UI.Class [UI.Fill X]] x
    FillY -> changeStyles ref [UI.Class [UI.Fill Y]] x   
    _             -> done
  widgetconf2cmd items x
  
-------------------------------------------------------------------------------

string2Color :: String -> UI.Color
string2Color = invf1 UI.showColor

ListBoxScroll :: [ConfItem] -> Widget
ListBoxScroll = ListBox

 
setConfig :: UIRef -> ConfItem -> UIEnv -> IO ()
setConfig cref value env = case value of
  List l -> UI2HTML.setConfig cref (UI2HTML.List l) env 
  --Text t -> setValue cref (show t) env
  _      -> done
  
seeText :: UIRef -> (Int,Int) -> UIEnv -> IO ()
seeText = UI2HTML.seeText 

getSaveFile :: IO ([_])
getSaveFile = return [] --"xxxx"
--writeFile2 _ text = return $ HTML.HtmlAnswer "text/plain" text

focusInput :: _ -> UIEnv -> IO ()
focusInput _ env = 
  showMessage "GUI2HTML.focusInput is not implemented" env

chooseColor :: IO (String)
chooseColor = return "red" --const done

col :: [Widget] -> Widget
col = Col []

row :: [Widget] -> Widget
row = Row []

matrix :: [[Widget]] -> Widget
matrix = Matrix []

getValue :: UIRef -> UIEnv -> IO (String)
getValue = UI2HTML.getValue

setValue :: UIRef -> String -> UIEnv -> IO ()
setValue = UI2HTML.setValue

updateValue :: (String -> String) -> UIRef -> UIEnv -> IO ()
updateValue = UI2HTML.updateValue

appendValue :: UIRef -> String -> UIEnv -> IO ()
appendValue = UI2HTML.appendValue  

exitGUI :: UIEnv -> IO ()
exitGUI = UI2HTML.exitUI

runGUI :: String -> Widget -> IO HTML.HtmlForm
runGUI title gui = UI2HTML.runUI title (gui2ui gui)

runInitGUI :: String -> Widget -> _ -> IO HTML.HtmlForm
runInitGUI title gui _ = UI2HTML.runUI title (gui2ui gui)

addCanvas :: UIRef -> [CanvasItem] -> UIEnv -> IO ()
addCanvas ref items env = UI2HTML.addCanvas ref (canvasgui2canvasui items) env


-----------------------------------------------------------------------------------

-- Read a (possibly negative) integer in a string.
-- Return Nothing is this is not an integer string.
readMaybeInt :: String -> Maybe Int
readMaybeInt "" = Nothing
readMaybeInt (v:s) | v=='-'  = maybe Nothing (\i->Just (-i)) (acc 0 s)
                   | isDigit v  = acc 0 (v:s)
                   | otherwise  = Nothing
 where
  acc n "" = Just n
  acc n (c:cs) | isDigit c = acc (10*n + ord c - ord '0') cs
               | otherwise = Nothing
