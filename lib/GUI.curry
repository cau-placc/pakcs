------------------------------------------------------------------------------
--- Library for GUI programming in Curry (based on Tcl/Tk).
--- [This paper](http://www.informatik.uni-kiel.de/~mh/papers/PADL00.html)
--- contains a description of the basic ideas behind this library.
---
--- This library is an improved and updated version of the library Tk.
--- The latter might not be supported in the future.
---
--- @authors Michael Hanus, Bernd Brassel
--- @version November 2006
------------------------------------------------------------------------------

module GUI(GuiPort,Widget(..),Button,ConfigButton,
           TextEditScroll,ListBoxScroll,CanvasScroll,EntryScroll,
           ConfItem(..),ReconfigureItem(..),
           Cmd,Command,Event(..),ConfCollection(..),MenuItem(..),
           CanvasItem(..),WidgetRef, Style(..), Color(..),
           col,row,matrix,
           runGUI,runGUIwithParams,runInitGUI,runInitGUIwithParams,
           runPassiveGUI,
           runControlledGUI,runConfigControlledGUI,runInitControlledGUI,
           runHandlesControlledGUI,runInitHandlesControlledGUI,
           exitGUI,getValue,setValue,updateValue,appendValue,
           appendStyledValue,addRegionStyle,removeRegionStyle,
           getCursorPosition,seeText,
           focusInput,addCanvas,setConfig,
           getOpenFile,getOpenFileWithTypes,getSaveFile,getSaveFileWithTypes,
           chooseColor,popup_message,debugTcl)  where

import Read
import Unsafe(trace)
import IO
import IOExts(connectToCommand)
import Char(isSpace,toUpper)

-- If showTclTkErrors is true, all synchronization errors occuring in the
-- Tcl/Tk communication are shown (such errors should only occur on
-- slow machines in exceptional cases; they should be handled by this library
-- but might be interesting to see for debugging)
showTclTkErrors = False

-- If showTclTkCommunication is true, the all strings sent to and from
-- the Tcl/Tk GUI are shown in stdout:
showTclTkCommunication = False

--- The port to a GUI is just the stream connection to a GUI
--- where Tcl/Tk communication is done.
data GuiPort = GuiPort Handle

handleOf :: GuiPort -> Handle
handleOf (GuiPort h) = h

------------------------------------------------------------------------
-- the basic data types for GUIs:
------------------------------------------------------------------------

--- The type of possible widgets in a GUI.
--- @cons PlainButton - a button in a GUI whose event handler is activated
---                     if the user presses the button
--- @cons Canvas      - a canvas to draw pictures containing CanvasItems
--- @cons CheckButton - a check button: it has value "0" if it is unchecked and
---                     value "1" if it is checked
--- @cons Entry       - an entry widget for entering single lines
--- @cons Label       - a label for showing a text
--- @cons ListBox     - a widget containing a list of items for selection
--- @cons Message     - a message for showing simple string values
--- @cons MenuButton  - a button with a pull-down menu
--- @cons Scale       - a scale widget to input values by a slider
--- @cons ScrollH     - a horizontal scroll bar
--- @cons ScrollV     - a vertical scroll bar
--- @cons TextEdit    - a text editor widget to show and manipulate larger
---                     text paragraphs
--- @cons Row         - a horizontal alignment of widgets
--- @cons Col         - a vertical alignment of widgets
--- @cons Matrix      - a 2-dimensional (matrix) alignment of widgets
data Widget = PlainButton            [ConfItem]
            | Canvas                 [ConfItem]
            | CheckButton            [ConfItem]
            | Entry                  [ConfItem]
            | Label                  [ConfItem]
            | ListBox                [ConfItem]
            | Message                [ConfItem]
            | MenuButton             [ConfItem]
            | Scale Int Int          [ConfItem]
            | ScrollH WidgetRef      [ConfItem]
            | ScrollV WidgetRef      [ConfItem]
            | TextEdit               [ConfItem]
            | Row    [ConfCollection] [Widget]
            | Col    [ConfCollection] [Widget]
            | Matrix [ConfCollection] [[Widget]]

--- The data type for possible configurations of a widget.
--- @cons Active    - define the active state for buttons, entries, etc.
--- @cons Anchor    - alignment of information inside a widget where the
---                   argument must be: n, ne, e, se, s, sw, w, nw, or center
--- @cons Background - the background color
--- @cons Foreground - the foreground color
--- @cons Handler - an event handler associated to a widget.
---                 The event handler returns a list of widget
---                 ref/configuration pairs that are applied after the handler
---                 in order to configure GUI widgets
--- @cons Height - the height of a widget (chars for text, pixels for graphics)
--- @cons CheckInit - initial value for checkbuttons
--- @cons CanvasItems - list of items contained in a canvas
--- @cons List  - list of values shown in a listbox
--- @cons Menu  - the items of a menu button
--- @cons WRef  - a reference to this widget
--- @cons Text  - an initial text contents
--- @cons Width - the width of a widget (chars for text, pixels for graphics)
--- @cons Fill  - fill widget in both directions
--- @cons FillX - fill widget in horizontal direction
--- @cons FillY - fill widget in vertical direction
--- @cons TclOption - further options in Tcl syntax (unsafe!)
data ConfItem =
   Active Bool
 | Anchor String
 | Background String
 | Foreground String
 | Handler Event (GuiPort -> IO [ReconfigureItem])
 | Height Int
 | CheckInit String
 | CanvasItems [CanvasItem]
 | List [String]   
 | Menu [MenuItem] 
 | WRef WidgetRef  
 | Text String     
 | Width Int       
 | Fill | FillX | FillY           
 | TclOption String

--- Data type for describing configurations that are applied
--- to a widget or GUI by some event handler.
--- @cons WidgetConf wref conf - reconfigure the widget referred by wref
---                              with configuration item conf
--- @cons StreamHandler hdl handler - add a new handler to the GUI
---       that processes inputs on an input stream referred by hdl
--- @cons RemoveStreamHandler hdl - remove a handler for an input stream
---       referred by hdl from the GUI (usually used to remove handlers
---       for closed streams)
data ReconfigureItem =
   WidgetConf WidgetRef ConfItem
 | StreamHandler Handle (Handle -> GuiPort -> IO [ReconfigureItem])
 | RemoveStreamHandler Handle

--- The data type of possible events on which handlers can react.
--- This list is still incomplete and might be extended or restructured
--- in future releases of this library.
--- @cons DefaultEvent - the default event of the widget
--- @cons MouseButton1 - left mouse button pressed
--- @cons MouseButton2 - middle mouse button pressed
--- @cons MouseButton3 - right mouse button pressed
--- @cons KeyPress     - any key is pressed
--- @cons Return       - return key is pressed
data Event = DefaultEvent
           | MouseButton1
           | MouseButton2
           | MouseButton3
           | KeyPress
           | Return

-- translate event into corresponding Tcl string (except for DefaultEvent)
-- with a leading blank:
event2tcl DefaultEvent = " default"
event2tcl MouseButton1 = " <ButtonPress-1>"
event2tcl MouseButton2 = " <ButtonPress-2>"
event2tcl MouseButton3 = " <ButtonPress-3>"
event2tcl KeyPress     = " <KeyPress>"
event2tcl Return       = " <Return>"


--- The data type for possible configurations of widget collections
--- (e.g., columns, rows).
--- @cons CenterAlign  - centered alignment
--- @cons LeftAlign    - left alignment
--- @cons RightAlign   - right alignment
--- @cons TopAlign     - top alignment
--- @cons BottomAlign  - bottom alignment
data ConfCollection =
   CenterAlign | LeftAlign | RightAlign | TopAlign | BottomAlign

--- The data type for specifying items in a menu.
--- @cons MButton - a button with an associated command
---                    and a label string
--- @cons MSeparator - a separator between menu entries
--- @cons MMenuButton - a submenu with a label string
data MenuItem =
   MButton (GuiPort -> IO [ReconfigureItem]) String
 | MSeparator
 | MMenuButton String [MenuItem]

--- The data type of items in a canvas.
--- The last argument are further options in Tcl/Tk (for testing).
data CanvasItem = CLine [(Int,Int)] String
                | CPolygon [(Int,Int)] String
                | CRectangle (Int,Int) (Int,Int) String
                | COval (Int,Int) (Int,Int) String
                | CText (Int,Int) String String


--- The (hidden) data type of references to a widget in a GUI window.
--- Note that the constructor WRefLabel will not be exported so that values
--- can only be created inside this module.
--- @cons WRefLabel wp label type - here "wp" is the GUI port related
---       to the widget, "label" is the (globally unique) identifier of
---       this widget used in Tk, and "type" is one of
---       button / canvas / checkbutton / entry / label / listbox /
---       message / scale / scrollbar / textedit
data WidgetRef = WRefLabel GuiPort String String

wRef2Label (WRefLabel _ var _)   = wRefname2Label var
wRef2Wtype (WRefLabel _ _ wtype) = wtype

--- The data type of possible text styles.
--- @cons Bold - text in bold font
--- @cons Italic - text in italic font
--- @cons Underline - underline text
--- @cons Fg - foreground color, i.e., color of the text font
--- @cons Bg - background color of the text
data Style = Bold | Italic | Underline | Fg Color | Bg Color

--- The data type of possible colors.
data Color 
  = Black | Blue | Brown | Cyan | Gold | Gray | Green | Magenta | Navy | Orange
  | Pink | Purple | Red | Tomato| Turquoise | Violet | White | Yellow

--- Converts a style value into its textual representation.
showStyle :: Style -> String
showStyle Bold      = "bold"
showStyle Italic    = "italic"
showStyle Underline = "underline"
showStyle (Fg fg)   = dropSpaces $ showColor fg
showStyle (Bg bg)   = camelCase $ showColor bg

dropSpaces :: String -> String
dropSpaces = filter (not . isSpace)

camelCase :: String -> String
camelCase (c:cs) = toUpper c : cc cs
 where
  cc "" = ""
  cc [x] = [x]
  cc (x:y:xs)
    | isSpace x = toUpper y : cc xs
    | otherwise = x : cc (y:xs)

--- Converts a color value into its textual representation.
showColor :: Color -> String
showColor Black     = "black"
showColor Blue      = "blue"
showColor Brown     = "brown"
showColor Cyan      = "cyan"
showColor Gold      = "gold"
showColor Gray      = "gray"
showColor Green     = "forest green"
showColor Magenta   = "magenta"
showColor Navy      = "navy"
showColor Orange    = "orange"
showColor Pink      = "pink"
showColor Purple    = "purple"
showColor Red       = "red"
showColor Tomato    = "tomato"
showColor Turquoise = "turquoise"
showColor Violet    = "violet"
showColor White     = "white"
showColor Yellow    = "yellow"

------------------------------------------------------------------------
-- Some useful abbreviations:
------------------------------------------------------------------------

--- Horizontal alignment of widgets.
row :: [Widget] -> Widget
row = Row []

--- Vertical alignment of widgets.
col :: [Widget] -> Widget
col = Col []

--- Matrix alignment of widgets.
matrix :: [[Widget]] -> Widget
matrix = Matrix []


------------------------------------------------------------------------
-- internal translation functions from GUI terms into Tcl:
------------------------------------------------------------------------

-- An event handler specification consists of an identifying string of
-- the widget for which this handler is repsonsible, an event type
-- to which the handler should react, and a handler:
type EventHandler = (String,Event,GuiPort -> IO [ReconfigureItem])

-- translate a widget into a pair of Tcl command string / event list
-- argument 1: port for the GUI
-- argument 2: current label prefix
-- argument 3: the widget to translate
-- result: pair of (Tcl command string,
--                  list of (eventname, eventtype, eventhandler))

widget2tcl :: GuiPort -> String -> Widget -> (String,[EventHandler])
widget2tcl wp label (PlainButton confs) =
    ("button "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "button" wp label confs

widget2tcl wp label (Canvas confs) =
    ("canvas "++label++"\n"
     ++"set "++refname++"_scrollx 100\n"
     ++"set "++refname++"_scrolly 100\n"
     ++"proc set"++refname++"_scrollx {x}"
     ++" { global "++refname++"_scrollx ; global "++refname++"_scrolly ;\n"
     ++"  if {$"++refname++"_scrollx  < $x} {set "++refname++"_scrollx $x ;\n"
     ++"   "++label++" configure -scrollregion [list 0 0 $"
                   ++refname++"_scrollx $"++refname++"_scrolly]}}\n"
     ++"proc set"++refname++"_scrolly {y}"
     ++" { global "++refname++"_scrollx ; global "++refname++"_scrolly ;\n"
     ++"  if {$"++refname++"_scrolly  < $y} {set "++refname++"_scrolly $y ;\n"
     ++"   "++label++" configure -scrollregion [list 0 0 $"
                   ++refname++"_scrollx $"++refname++"_scrolly]}}\n"
     ++ conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "canvas" wp label confs

widget2tcl wp label (CheckButton confs) =
    ("checkbutton "++label++"\n" ++
     label++" configure -variable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "checkbutton" wp label confs

widget2tcl wp label (Entry confs) =
    ("entry "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "entry" wp label confs

widget2tcl wp label (Label confs) =
    ("label "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "label" wp label confs

widget2tcl wp label (ListBox confs) =
    ("listbox "++label++" -exportselection false\n" ++
     "proc getvar"++refname++" {} { return ["++label++" curselection]}\n" ++
     "proc setvar"++refname++" {s} { "++label++" selection clear 0 end ; "
             ++label++" selection set $s ; "++label++" see $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "listbox" wp label confs

widget2tcl wp label (Message confs) =
    ("message "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "message" wp label confs

widget2tcl wp label (MenuButton confs) =
    ("menubutton "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "menubutton" wp label confs

widget2tcl wp label (Scale from to confs) =
    ("scale "++label++" -from "++show from++" -to "++show to++
     " -orient horizontal -length 200\n" ++
     "variable "++refname++" "++show from++"\n"++  -- initialize scale variable
     label++" configure -variable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "scale" wp label confs

widget2tcl wp label (ScrollH widget confs) =
    ("scrollbar "++label++" -orient horizontal -command {"++
                                         wRef2Label widget++" xview}\n" ++
     wRef2Label widget++" configure -xscrollcommand {"++label++" set}\n" ++
     wRef2Label widget++" configure -wrap none\n" ++ -- no line wrap
     conf_tcl , conf_evs)
   where (conf_tcl,conf_evs) = configs2tcl "scrollbar" wp label confs

widget2tcl wp label (ScrollV widget confs) =
    ("scrollbar "++label++" -command {"++wRef2Label widget++" yview}\n" ++
     wRef2Label widget++" configure -yscrollcommand {"++label++" set}\n" ++
     conf_tcl , conf_evs)
   where (conf_tcl,conf_evs) = configs2tcl "scrollbar" wp label confs

widget2tcl wp label (TextEdit confs) =
    ("text "++label++"\n"++ --" -height 15\n" ++
     "proc getvar"++refname++" {} { "++label++" get 1.0 {end -1 chars}}\n" ++
     "proc setvar"++refname++" {s} { "++label++" delete 1.0 end ; "
                                      ++label++" insert 1.0 $s}\n" ++
     conf_tcl ++
     enableFont "italic" "-slant italic" ++
     enableFont "underline" "-underline on" ++
     enableFont "bold" "-weight bold" ++
     unlines (map enableForeground colors) ++
     unlines (map enableBackground colors)
    , conf_evs)
   where refname = wLabel2Refname label
         (conf_tcl,conf_evs) = configs2tcl "textedit" wp label confs

         enableFont tag style
           = label ++ " tag configure " ++ tag ++ " -font \"[font actual [" ++
             label ++ " cget -font]] " ++ style ++ "\"\n"

         colors = map showColor
           [Black,Blue,Brown,Cyan,Gold,Gray,Green,Magenta,Navy,Orange,Pink
           ,Purple,Red,Tomato,Turquoise,Violet,White,Yellow]

         enableForeground color
           = label ++ " tag configure " ++ dropSpaces color ++
             " -foreground \"" ++ color ++ "\""

         enableBackground color
           = label++" tag configure "++ camelCase color ++
             " -background \"" ++ color ++ "\""

widget2tcl wp label (Row confs ws) =
  ((if label=="" then "wm resizable . " ++ resizeBehavior wsGridInfo++"\n"
    else "frame "++label++"\n") ++
   wstcl ++
   (snd $ foldl (\ (n,g) l->(n+1,g++"grid "++label ++ labelIndex2string (96+n)
                                  ++" -row 1 -column "++show n++" "
                                  ++confCollection2tcl confs
                                  ++gridInfo2tcl n label "col" l ++ "\n")) 
                (1,"")
                wsGridInfo),
   wsevs)
  where (wstcl,wsevs) = widgets2tcl wp label 97 ws
        wsGridInfo = widgets2gridinfo ws
        

widget2tcl wp label (Col confs ws) =
  ((if label=="" then "wm resizable . " ++ resizeBehavior wsGridInfo++"\n"
    else "frame "++label++"\n") ++
      wstcl ++
      (snd $ foldl (\ (n,g) l->(n+1,g++"grid "++label ++ labelIndex2string (96+n)
                                     ++" -column 1 -row "++show n++" "
                                     ++confCollection2tcl confs
                                     ++gridInfo2tcl n label "row" l ++ "\n"))
                   (1,"")
                   (widgets2gridinfo ws)),
      wsevs)
  where (wstcl,wsevs) = widgets2tcl wp label 97 ws
        wsGridInfo = widgets2gridinfo ws

widget2tcl wp label (Matrix confs ws) = 
  ((if label == "" then "wm resizable . " ++ resizeBehavior wsGridInfo++"\n" 
    else "frame "++label++"\n") ++ wstcl,wsevs)

  where
    (wstcl,wsevs) =  matrix2tcl 97 1 wp label confs ws
    wsGridInfo = concatMap widgets2gridinfo ws

-- actual translation function of the list of lists of widgets in a matrix
matrix2tcl :: Int -> Int -> GuiPort -> String -> [ConfCollection] 
                    -> [[Widget]] -> (String,[EventHandler])
matrix2tcl _ _ _ _ _ [] = ("",[])
matrix2tcl nextLabel n wp label confs (ws:wss) =
   (wstcl ++ 
   (snd $ foldl (\ (m,g) l->(m+1,g++"grid "++label ++ labelIndex2string (nextLabel+m-1)
                                  ++" -row "++show n ++" -column "++show m++" "
                                  ++confCollection2tcl confs
                                  ++gridInfo2tcl m label "col" l ++ "\n"))
                (1,"")
                wsGridInfo) ++ wsstcl, wsevs++wssevs)
  where (wsstcl,wssevs) = matrix2tcl (nextLabel+length ws) (n+1) wp label confs wss
        (wstcl,wsevs) = widgets2tcl wp label nextLabel ws
        wsGridInfo = widgets2gridinfo ws

-- compute the required resize behavior of the top window
resizeBehavior :: [[ConfItem]] -> String
resizeBehavior ws = if any (elem Fill) ws then "1 1" else
                    if any (elem FillX) ws then "1 0" else
                    if any (elem FillY) ws then "0 1" else "0 0"


-- list of labels of the widgets
widgets2gridinfo [] = []
widgets2gridinfo (w:ws) =
    (tclfill ++ getConfs w): widgets2gridinfo ws
 where
  fillx    = hasFillX w 
  filly    = hasFillY w 
  flexible = hasFill  w 
  tclfill  = if flexible || (fillx && filly) then [Fill] else
             if fillx then [FillX] else
             if filly then [FillY] else []
             
hasFillX w = any isFillXConf (propagateFillInfo w)
isFillXConf conf = case conf of
  FillX -> True
  _       -> False

hasFillY w = any isFillYConf (propagateFillInfo w)
isFillYConf conf = case conf of
  FillY -> True
  _       -> False

hasFill w = any isFillConf (propagateFillInfo w)
isFillConf conf = case conf of
  Fill -> True
  _      -> False

isFillInfo conf = case conf of
                    FillX -> True
                    FillY -> True
                    Fill  -> True
                    _       -> False

-- propagate FillInfo for those kinds of widgets which are resizable on their on
propagateFillInfo (PlainButton _)     = []
propagateFillInfo (Canvas      confs) = filter isFillInfo confs
propagateFillInfo (CheckButton _)     = []
propagateFillInfo (Entry       confs) = filter isFillInfo confs
propagateFillInfo (Label       confs) = filter isFillInfo confs
propagateFillInfo (ListBox     confs) = filter isFillInfo confs
propagateFillInfo (Message     confs) = filter isFillInfo confs
propagateFillInfo (MenuButton  _)     = []
propagateFillInfo (Scale _ _   confs) = filter isFillInfo confs
propagateFillInfo (ScrollV _   _)     = []
propagateFillInfo (ScrollH _   _)     = []
propagateFillInfo (TextEdit    confs) = filter isFillInfo confs
propagateFillInfo (Row _ ws) = concatMap propagateFillInfo ws
propagateFillInfo (Col _ ws) = concatMap propagateFillInfo ws
propagateFillInfo (Matrix _ wss) = concatMap (concatMap propagateFillInfo) wss

-- get the configurations of a widget
getConfs (PlainButton confs) = confs
getConfs (Canvas      confs) = filter isFillInfo confs
getConfs (CheckButton confs) = confs
getConfs (Entry       confs) = filter isFillInfo confs
getConfs (Label       confs) = filter isFillInfo confs
getConfs (ListBox     confs) = filter isFillInfo confs
getConfs (Message     confs) = filter isFillInfo confs
getConfs (MenuButton  confs) = confs
getConfs (Scale _ _   confs) = filter isFillInfo confs
getConfs (ScrollV _   confs) = confs
getConfs (ScrollH _   confs) = confs
getConfs (TextEdit    confs) = filter isFillInfo confs
getConfs (Row _ _)           = []
getConfs (Col _ _)           = []
getConfs (Matrix _ _)        = [] 


-- translate configuration options for collections (rows or columns)
-- into parameters for the Tcl/Tk command "grid":
confCollection2tcl [] = ""
confCollection2tcl (CenterAlign : confs) = confCollection2tcl confs
confCollection2tcl (LeftAlign : confs)   = "-sticky w " ++ confCollection2tcl confs
confCollection2tcl (RightAlign : confs)  = "-sticky e " ++ confCollection2tcl confs
confCollection2tcl (TopAlign : confs)    = "-sticky n " ++ confCollection2tcl confs
confCollection2tcl (BottomAlign : confs) = "-sticky s " ++ confCollection2tcl confs

-- translate the Fill - options to sticky options and grid configures
gridInfo2tcl :: Int -> String -> String -> [ConfItem] -> String
gridInfo2tcl n label "col" confs 
  | elem Fill confs || (elem FillX confs && elem FillY confs)
  = "-sticky nsew \ngrid columnconfigure "++lab++" "++show n++
    " -weight 1\ngrid rowconfigure "++lab++" 1 -weight 1"
  | elem FillX confs = "-sticky we \ngrid columnconfigure "++lab++
                         " "++show n++" -weight 1"
  | elem FillY confs = "-sticky ns \ngrid rowconfigure "++lab++
                         " 1 -weight 1"
  | otherwise = ""
  where
    lab = if label=="" then "." else label

gridInfo2tcl n label "row" confs 
  | elem Fill confs || (elem FillX confs && elem FillY confs)
  = "-sticky nsew \ngrid columnconfigure "++lab++
    " 1 -weight 1\ngrid rowconfigure "++lab++" "++show n++" -weight 1"
  | elem FillX confs = "-sticky we \ngrid columnconfigure "++lab++
                         " 1 -weight 1"
  | elem FillY confs =  "-sticky ns \ngrid rowconfigure "++lab++
                          " "++show n++" -weight 1"
  | otherwise = ""
  where
    lab = if label=="" then "." else label

-- translate a single configuration option into Tcl/Tk commands
-- to configure the widget:
-- the first argument specifies the type of the widget
-- (button/canvas/checkbutton/entry/label/listbox/message/scale/scrollbar/
--  textedit)
-- and the third argument is the widget label
config2tcl :: String -> GuiPort -> String -> ConfItem -> String

-- is the state of the widget active ("normal" in Tcl/Tk) or
-- inactive ("disabled" in Tcl/Tk)?
-- (inactive widgets do not accept any events)
config2tcl wtype _ label (Active active) =
  if wtype=="button" || wtype=="checkbutton" || wtype=="entry" ||
     wtype=="menubutton" || wtype=="scale" || wtype=="textedit"
  then if active
       then label++" configure -state normal\n"
       else label++" configure -state disabled\n"
  else trace ("WARNING: GUI.Active ignored for widget type \""++wtype++"\"\n") ""

-- alignment of information inside a widget
-- argument must be: n, ne, e, se, s, sw, w, nw, or center
config2tcl wtype _ label (Anchor align) =
  if wtype=="button" || wtype=="checkbutton" || wtype=="label" ||
     wtype=="menubutton" || wtype=="message"
  then label++" configure -anchor "++align++"\n"
  else trace ("WARNING: GUI.Anchor ignored for widget type \""++wtype++"\"\n") ""

-- background color:
config2tcl _ _ label (Background color)
 = label++" configure -background \""++color++"\"\n"

-- foreground color:
config2tcl _ _ label (Foreground color)
 = label++" configure -foreground \""++color++"\"\n"

-- command associated to various widgets:
config2tcl wtype _ label (Handler evtype _)
 | evtype == DefaultEvent
 = if wtype=="button"
   then label++" configure -command"++writeEvent else
   if wtype=="checkbutton"
   then label++" configure -command"++writeEvent else
   if wtype=="entry"
   then "bind "++label++" <Return>"++writeEvent else
   if wtype=="scale"
   then label++" configure -command { putlabel \""++label++event2tcl evtype++"\"}\n" else
   if wtype=="listbox"
   then "bind "++label++" <ButtonPress-1>"++writeEvent else
   if wtype=="textedit"
   then "bind "++label++" <KeyPress>"++writeEvent
   else
    trace ("WARNING: GUI.Handler with DefaultEvent ignored for widget type \""++
           wtype++"\"\n") ""
 | otherwise
 = "bind "++label++event2tcl evtype++writeEvent
 where
  writeEvent = " { writeevent \""++label++event2tcl evtype++"\" }\n"

-- height of a widget (not defined for all widget types):
config2tcl wtype _ label (Height h)
 | wtype=="entry" || wtype=="message" || wtype=="menubutton" ||
   wtype=="scale"
  = trace ("WARNING: GUI.Height ignored for widget type \""++wtype++"\"\n") ""
 | wtype=="canvas"
  = label++" configure -height "++show h++"\n"++
    "set"++wLabel2Refname label++"_scrolly "++show h++"\n"
 | otherwise
  = label++" configure -height "++show h++"\n"

-- value of checkbuttons:
config2tcl wtype _ label (CheckInit s)
 | wtype=="checkbutton"
   = "setvar"++wLabel2Refname label++" \""++s++"\"\n"
 | otherwise
 = trace ("WARNING: GUI.CheckInit ignored for widget type \""++wtype++"\"\n") ""

-- items in a canvas:
config2tcl wtype _ label (CanvasItems items)
 | wtype=="canvas" = canvasItems2tcl label items
 | otherwise
 = trace ("WARNING: GUI.CanvasItems ignored for widget type \""++wtype++"\"\n") ""

-- value lists for listboxes:
config2tcl wtype _ label (List l)
 | wtype=="listbox"
   = label++" delete 0 end\n" ++ setlistelems (ensureSpine l)
 | otherwise
 = trace ("WARNING: GUI.List ignored for widget type \""++wtype++"\"\n") ""

 where
   setlistelems [] = ""
   setlistelems (e:es) = label++" insert end \""++escape_tcl e++"\"\n"++
                         setlistelems es

-- items in a menu button:
config2tcl wtype _ label (Menu l)
 | wtype=="menubutton"
   = label++" configure -menu "++label++".a\n" ++
     menu2tcl (label++".a") l
 | otherwise
 = trace ("WARNING: GUI.Menu ignored for widget type \""++wtype++"\"\n") ""

-- references to widgets are bound to actual widget labels:
config2tcl wtype wp label (WRef r)
 | r =:= WRefLabel wp (wLabel2Refname label) wtype = ""

-- initial text value of widgets:
config2tcl wtype _ label (Text s)
 | wtype=="canvas"
   = trace "WARNING: GUI.Text ignored for Canvas\n" ""
 | wtype=="checkbutton"
   = label++" configure -text \""++escape_tcl s++"\"\n"
 | otherwise
   = "setvar"++wLabel2Refname label++" \""++escape_tcl s++"\"\n"

-- width of a widget:
config2tcl wtype _ label (Width w)
 | wtype=="canvas"
   = label++" configure -width "++show w++"\n"++
     "set"++wLabel2Refname label++"_scrollx "++show w++"\n"
 | otherwise = label++" configure -width "++show w++"\n"

-- configuration options for widget composition are ignored here
-- since they are used during geometry management 
config2tcl _ _ _ Fill = ""
config2tcl _ _ _ FillX = ""
config2tcl _ _ _ FillY = ""

-- for testing, put arbitrary Tk options for this widget:
config2tcl _ _ label (TclOption tcloptions)
 = label++" configure "++tcloptions++"\n"


-- translation of a menu with a given label:
menu2tcl label menu =
  "menu "++label++" -tearoff false\n" ++
  label++" delete 0 end\n" ++ 
  setmenuelems menu 0
 where setmenuelems [] _ = ""
       setmenuelems (MButton _ text : es) i =
          label++" add command -label \""++escape_tcl text++
                "\" -command { writeevent \""++label++"."++show i++
                                          event2tcl DefaultEvent++"\" }\n"++
          setmenuelems es (i+1)
       setmenuelems (MSeparator : es) i =
          label++" add separator\n"++ setmenuelems es (i+1)
       setmenuelems (MMenuButton text l : es) i =
          label++" add cascade -label \""++escape_tcl text++
                "\" -menu "++label++labelIndex2string (i+97)++"\n"++
          menu2tcl (label++labelIndex2string (i+97)) l ++
          setmenuelems es (i+1)

-- get the event handlers in a list of configuration options:
-- and bind widget references:
configs2handler :: String -> [ConfItem] -> [EventHandler]
configs2handler _ [] = []
configs2handler label (confitem : cs) = case confitem of
  Handler evtype handler -> (label,evtype,handler) : configs2handler label cs
  Menu m                 -> menu2handler (label++".a") m 0 ++ configs2handler label cs
  _                      -> configs2handler label cs

menu2handler _ [] _ = []
menu2handler label (MButton handler _ : ms) i =
         (label++"."++show i, DefaultEvent, handler) : menu2handler label ms (i+1)
menu2handler label (MSeparator : ms) i = menu2handler label ms (i+1)
menu2handler label (MMenuButton _ menu : ms) i =
  menu2handler (label++labelIndex2string (i+97)) menu 0 ++
  menu2handler label ms (i+1)

-- translate configuration options into Tcl/Tk commands and event handler map:
configs2tcl :: String -> GuiPort -> String -> [ConfItem]
               -> (String,[EventHandler])
configs2tcl wtype wp label confs =
  (concatMap (config2tcl wtype wp label) confs,
   configs2handler label confs)


-- translate a list of canvas items into a Tcl string:
canvasItems2tcl _ [] = ""
canvasItems2tcl label (i:is) = 
   canvasItem2tcl label i ++ canvasItems2tcl label is

canvasItem2tcl label (CLine coords opts) =
  label++ " create line "++showCoords coords++" "++opts++"\n"++
  concatMap (\(x,_)->"set"++refname++"_scrollx "++show x++"\n") coords ++
  concatMap (\(_,y)->"set"++refname++"_scrolly "++show y++"\n") coords
    where refname = wLabel2Refname label
canvasItem2tcl label (CPolygon coords opts) =
  label++ " create polygon "++showCoords coords++" "++opts++"\n"++
  concatMap (\(x,_)->"set"++refname++"_scrollx "++show x++"\n") coords ++
  concatMap (\(_,y)->"set"++refname++"_scrolly "++show y++"\n") coords
    where refname = wLabel2Refname label
canvasItem2tcl label (CRectangle (x1,y1) (x2,y2) opts) =
  label++ " create rectangle "++showCoords [(x1,y1),(x2,y2)]++" "++opts++"\n"++
  concatMap (\x->"set"++refname++"_scrollx "++show x++"\n") [x1,x2] ++
  concatMap (\y->"set"++refname++"_scrolly "++show y++"\n") [y1,y2]
    where refname = wLabel2Refname label
canvasItem2tcl label (COval (x1,y1) (x2,y2) opts) =
  label++ " create oval "++showCoords [(x1,y1),(x2,y2)]++" "++opts++"\n"++
  concatMap (\x->"set"++refname++"_scrollx "++show x++"\n") [x1,x2] ++
  concatMap (\y->"set"++refname++"_scrolly "++show y++"\n") [y1,y2]
    where refname = wLabel2Refname label
canvasItem2tcl label (CText (x,y) text opts) = 
  label++ " create text "++show x++" "++show y++
          " -text \""++escape_tcl text++"\" "++opts++"\n"++
  "set"++refname++"_scrollx "++show (x+5*(length text))++"\n"++
  "set"++refname++"_scrolly "++show y++"\n"
    where refname = wLabel2Refname label

showCoords [] = ""
showCoords ((x,y):cs) = show x++" "++show y++" "++showCoords cs


-- translate a widget label into a name (replacing dots by underscores)
wLabel2Refname l = map (\c -> if c=='.' then '_' else c) l

-- translate a name into a widget label (replacing underscores by dots)
wRefname2Label l = map (\c -> if c=='_' then '.' else c) l


-- translate a list of widgets into pair Tcl string / event list:
widgets2tcl _ _ _ [] = ("",[])
widgets2tcl wp lab nr (w:ws) = (wtcl ++ wstcl, wevs ++ wsevs)
  where (wtcl,wevs) = widget2tcl wp (lab++labelIndex2string nr) w
        (wstcl,wsevs) = widgets2tcl wp lab (nr+1) ws

-- translate a label index into a textual label
-- (e.g., 97->".a" or 123->".z1"):
labelIndex2string :: Int -> String
labelIndex2string li = if li<123 then ['.',chr li]
                                 else ['.','z'] ++ show (li-122)

-- translate main widget:
mainWidget2tcl :: GuiPort -> Widget -> (String,[EventHandler])
mainWidget2tcl wp widget =
  ("proc writeevent {l} { puts \":EVT$l\" }\n" ++
   "proc putlabel {l v} { writeevent $l }\n" ++
   "proc putvar {var value} { puts \":VAR$var%[string length $value]*$value\"}\n" ++
   widgettcl, evs)
  where (widgettcl,evs) = widget2tcl wp "" widget


--- Prints the generated Tcl commands of a main widget (useful for debugging).
debugTcl :: Widget -> IO ()
debugTcl widget = putStrLn (fst (mainWidget2tcl wp widget))  where wp free


------------------------------------------------------------------------
-- Operations to communicate with Tcl/Tk:
------------------------------------------------------------------------

reportTclTk s =
 if showTclTkCommunication then hPutStrLn stdout s else done

reportTclTkError s =
 if showTclTkErrors then hPutStrLn stderr s else done

-- Open a GUI port by connecting to new "wish" process.
-- The first argument are parameters passed to the wish command.
openGuiPort :: String -> IO GuiPort
openGuiPort wishparams = do
  reportTclTk ("OPEN CONNECTION TO WISH WITH PARAMS: "++wishparams)
  tclhdl <- connectToCommand ("wish "++wishparams)
  return (GuiPort tclhdl)

-- Send a string (Tcl/Tk command) to GUI port:
send2tk :: String -> GuiPort -> IO ()
send2tk s (GuiPort tclhdl) = do
  reportTclTk ("GUI SEND: "++s)
  hPutStrLn tclhdl s
  hFlush tclhdl

-- Receive an output line from the wish process:
receiveFromTk :: GuiPort -> IO String
receiveFromTk (GuiPort tclhdl) = do
  s <- hGetLine tclhdl
  reportTclTk ("GUI RECEIVED: "++s)
  return s

-- Choice over the output of the wish process and a stream of external messages
choiceOverHandlesMsgs :: [Handle] -> [msg] -> IO (Either (Int,Handle) [msg])
choiceOverHandlesMsgs hdls msgs = do
  iormsgs <- hWaitForInputsOrMsg hdls msgs
  return (either (\i -> Left (i,hdls!!i)) Right iormsgs)

-- Choice over the output of the wish process and handles to input streams:
choiceOverHandles :: [Handle] -> IO (Int,Handle)
choiceOverHandles hdls = do
  i <- hWaitForInputs hdls (-1)
  return (i,hdls!!i)

-- Close connection to wish process:
closeGuiPort :: GuiPort -> IO ()
closeGuiPort (GuiPort tclhdl) = do
  reportTclTk "CLOSE CONNECTION TO WISH"
  hClose tclhdl


------------------------------------------------------------------------
-- functions for running a GUI:
------------------------------------------------------------------------

--- Creates a new GUI window with a "title" for the top-level window
--- (but unspecified contents). A GUI port is returned that can be
--- used to start a GUI specification on this port.
--- @param title - the title of the top-level window
--- @param params - parameter string passed to the initial wish command
openWish :: String -> String -> IO GuiPort
openWish title params = do
  gport <- openGuiPort params
  send2tk ("wm title . \""++title++"\"\n") gport
  return gport


--- IO action to show a Widget in a new GUI window in passive mode,
--- i.e., ignore all GUI events.
--- @param title - the title of the main window containing the widget
--- @param widget - the widget shown in the new window
runPassiveGUI :: String -> Widget -> IO GuiPort
runPassiveGUI title widget = do
  gport <- openWish (escape_tcl title) ""
  send2tk (fst (mainWidget2tcl gport widget)) gport
  return gport


--- IO action to run a Widget in a new window.
--- @param title - the title of the main window containing the widget
--- @param widget - the widget shown in the new window
runGUI :: String -> Widget -> IO ()
runGUI title widget = runInitGUIwithParams title "" widget (const done)

--- IO action to run a Widget in a new window.
--- @param title - the title of the main window containing the widget
--- @param params - parameter string passed to the initial wish command
--- @param widget - the widget shown in the new window
runGUIwithParams :: String -> String -> Widget -> IO ()
runGUIwithParams title params widget =
  runInitGUIwithParams title params widget (const done)

--- IO action to run a Widget in a new window. The GUI events
--- are processed after executing an initial action on the GUI.
--- @param title - the title of the main GUI window
--- @param widget - the widget shown in the new GUI window
--- @param initcmd - the initial command executed before activating the GUI
runInitGUI :: String -> Widget -> (GuiPort -> IO ()) -> IO ()
runInitGUI title widget initcmd = do
  gport <- openWish (escape_tcl title) ""
  initSchedule widget gport [] [] initcmd

--- IO action to run a Widget in a new window. The GUI events
--- are processed after executing an initial action on the GUI.
--- @param title - the title of the main GUI window
--- @param params - parameter string passed to the initial wish command
--- @param widget - the widget shown in the new GUI window
--- @param initcmd - the initial command executed before activating the GUI
runInitGUIwithParams :: String -> String -> Widget -> (GuiPort -> IO ()) -> IO ()
runInitGUIwithParams title params widget initcmd = do
  gport <- openWish (escape_tcl title) params
  initSchedule widget gport [] [] initcmd


--- Runs a Widget in a new GUI window and process GUI events.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- This operation is useful to run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (widget,exth) where widget is the widget shown in the
---             new window and exth is the event handler for external messages
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
runControlledGUI :: String -> (Widget, msg -> GuiPort -> IO ()) -> [msg] -> IO ()
runControlledGUI title (widget,exth) msgs =
  runInitControlledGUI title (widget,exth) (const done) msgs


--- Runs a Widget in a new GUI window and process GUI events.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- This operation is useful to run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (widget,exth) where widget is the widget shown in the
---             new window and exth is the event handler for external messages
---             that returns a list of widget reference/configuration pairs
---             which is applied after the handler in order to configure
---             some GUI widgets
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
runConfigControlledGUI :: String ->
       (Widget, msg -> GuiPort -> IO [ReconfigureItem]) -> [msg] -> IO ()
runConfigControlledGUI title (widget,exth) msgs = do
  gport <- openWish (escape_tcl title) ""
  initSchedule widget gport [PortMsgHandler exth] msgs (\_->done)

--- Runs a Widget in a new GUI window and process GUI events
--- after executing an initial action on the GUI window.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- This operation is useful to run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (widget,exth) where widget is the widget shown in the
---             new window and exth is the event handler for external messages
--- @param initcmd - the initial command executed before starting the GUI
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
runInitControlledGUI :: String -> (Widget, msg -> GuiPort -> IO ()) ->
                        (GuiPort -> IO ()) -> [msg] -> IO ()
runInitControlledGUI title (widget,exth) initcmd msgs = do
  gport <- openWish (escape_tcl title) ""
  initSchedule widget gport
               [PortMsgHandler (\msg wp -> exth msg wp >> return [])]
               msgs initcmd


--- Runs a Widget in a new GUI window and process GUI events.
--- In addition, a list of event handlers is provided that process
--- inputs received from a corresponding list of handles to input streams.
--- Thus, if the i-th handle has some data available, the i-th event handler
--- is executed with the i-th handle as a parameter.
--- This operation is useful to run a GUI that should react on
--- inputs provided by other processes, e.g., via sockets.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (widget,handlers) where widget is the widget shown in the
---             new window and handlers is a list of event handler for external inputs
--- @param handles - a list of handles to the external input streams for the
---                  corresponding event handlers
runHandlesControlledGUI :: String -> (Widget,[Handle -> GuiPort -> IO ()])
                           -> [Handle] -> IO ()
runHandlesControlledGUI title widgethandlers handles =
  runInitHandlesControlledGUI title widgethandlers (const done) handles


--- Runs a Widget in a new GUI window and process GUI events
--- after executing an initial action on the GUI window.
--- In addition, a list of event handlers is provided that process
--- inputs received from a corresponding list of handles to input streams.
--- Thus, if the i-th handle has some data available, the i-th event handler
--- is executed with the i-th handle as a parameter.
--- This operation is useful to run a GUI that should react on
--- inputs provided by other processes, e.g., via sockets.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (widget,handlers) where widget is the widget shown in the
---             new window and handlers is a list of event handler for external inputs
--- @param initcmd - the initial command executed before starting the GUI
--- @param handles - a list of handles to the external input streams for the
---                  corresponding event handlers
runInitHandlesControlledGUI :: String -> (Widget,[Handle -> GuiPort -> IO ()])
                               -> (GuiPort -> IO ()) -> [Handle] -> IO ()
runInitHandlesControlledGUI title (widget,handlers) initcmd handles =
 do gport <- openWish (escape_tcl title) ""
    initSchedule widget gport
                 (map IOHandler (zip handles (map toIOHandler handlers)))
                 [] initcmd

-- The type of external event handlers currently supported.
-- It is either a handler processing messages from an external port
-- or a handler processing input from various IO streams
data ExternalHandler msg =
   PortMsgHandler (msg -> GuiPort -> IO [ReconfigureItem])
 | IOHandler (Handle,
       [EventHandler] -> Handle -> GuiPort -> IO (Maybe [ReconfigureItem]))

-- start the scheduler (see below) with a given Widget on a wish port
-- and an initial command:
initSchedule :: Widget -> GuiPort -> [ExternalHandler msg] ->
                [msg] -> (GuiPort -> IO ()) -> IO ()
initSchedule widget gport exths msgs initcmd = do
  send2tk tcl gport
  initcmd gport
  -- add handler on wish connection as first handler:
  scheduleTkEvents evs gport
                   (IOHandler (handleOf gport,processTkEvent) : exths) msgs
 where
  (tcl,evs) = mainWidget2tcl gport widget

-- Scheduler for Tcl/Tk events:
--
-- Meaning of arguments:
-- evs: list of EventHandlers
-- gport: port to a wish
-- exth: handler for external messages
-- msgs: list of external messages
scheduleTkEvents :: [EventHandler] -> GuiPort -> [ExternalHandler msg]
                    -> [msg] -> IO ()
-- schedule GUI with handler for external port:
scheduleTkEvents evs gport exthds msgs = do
  (newmsgs,newconfigs) <- processEvent evs gport (splitHandlers exthds) msgs
  configAndProceedScheduler evs gport exthds newmsgs newconfigs
 where
  -- split ExternalHandlers into list of PortMsgHandler and list of IOHandler:
  splitHandlers [] = ([],[])
  splitHandlers (PortMsgHandler ph : exths) =
    let (phs,iohs) = splitHandlers exths in (ph : phs, iohs)
  splitHandlers (IOHandler ioh : exths) =
    let (phs,iohs) = splitHandlers exths in (phs, ioh : iohs)


processEvent evs gport (portmsghandler:_,iohandlers) msgs = do
  -- for implementation reasons, we take only the first PortMsgHandler
  -- (more than one should not occur)
  answer <- choiceOverHandlesMsgs (map fst iohandlers) msgs
  either (\ (i,hdl) -> do confs <- (snd (iohandlers!!i)) evs hdl gport
                          return (msgs,confs))
         (\ (newmsg:newmsgs) -> do configs <- portmsghandler newmsg gport
                                   return (newmsgs, Just configs) )
         answer
-- schedule GUI with handlers for external streams:
processEvent evs gport ([],iohandlers) msgs = do
  (i,hdl) <- choiceOverHandles (map fst iohandlers)
  mbconfigs <- (snd (iohandlers!!i)) evs hdl gport
  return (msgs,mbconfigs)


-- process an event from the wish and return the new configuration items:
processTkEvent :: [EventHandler] -> Handle -> GuiPort
                  -> IO (Maybe [ReconfigureItem])
processTkEvent evs str gport =
  hIsEOF str >>= \eof ->
  if eof then return Nothing -- connection closed (by wish)
  else 
    hGetLine str >>= \ans ->
    reportTclTk ("GUI RECEIVED: "++ans) >>
    if (take 4 ans)==":EVT"
    then let (evwidget,evtype) = break (==' ') (drop 4 ans) in
         selectEvent evwidget evtype evs gport >>= \configs ->
         return (Just configs)
    else do reportTclTkError("ERROR in scheduleTkEvents: Received: "++ans++"\n")
            -- ignore other outputs:
            return (Just [])


-- Reconfigure scheduler with new configurations and proceed.
-- If the configs are Nothing, then terminate the scheduler
-- (this case occurs of the connection is closed by wish)
configAndProceedScheduler _ gport _ _ Nothing = closeGuiPort gport
configAndProceedScheduler evs gport exths msgs (Just configs) = do
  mapIO_ reconfigureGUI configs
  scheduleTkEvents (configEventHandlers evs configs) gport
                   (configStreamHandlers exths configs)
                   msgs
 where
  reconfigureGUI (WidgetConf r ci) = setConfig r ci gport
  reconfigureGUI (StreamHandler _ _) = done
  reconfigureGUI (RemoveStreamHandler _) = done

configEventHandlers evs [] = evs
configEventHandlers evs (WidgetConf ref confitem : confitems) =
 let label = wRef2Label ref in
 case confitem of
   Handler evtype handler ->
        configEventHandlers ((label,evtype,handler) :
                              (filter (\ (l,t,_)->l/=label || t/=evtype) evs))
                            confitems
   _ -> configEventHandlers evs confitems
configEventHandlers evs (StreamHandler _ _ : confitems) =
  configEventHandlers evs confitems
configEventHandlers evs (RemoveStreamHandler _ : confitems) =
  configEventHandlers evs confitems

-- reconfigure external stream handlers:
configStreamHandlers exths [] = exths
configStreamHandlers exths (WidgetConf _ _ : confitems) =
  configStreamHandlers exths confitems
configStreamHandlers exths (StreamHandler handle handler : confitems) =
  configStreamHandlers
    (exths++[IOHandler (handle,\_ hdl gp -> handler hdl gp >>= return . Just)])
    confitems
configStreamHandlers exths (RemoveStreamHandler handle : confitems) =
  configStreamHandlers (removeHandler handle exths) confitems
 where
  removeHandler _ [] = []
  removeHandler h (PortMsgHandler hr : ehs) =
    PortMsgHandler hr : removeHandler h ehs
  removeHandler h (IOHandler (h',hr) : ehs) =
    if h==h' then removeHandler h ehs
             else IOHandler (h',hr) : removeHandler h ehs

-- transform external handler into an IO Handler used in the scheduler
-- which alwaus returns empty configurations:
toIOHandler handler _ handle gport = handler handle gport >> return (Just [])

--- Changes the current configuration of a widget
--- (deprecated operation, only included for backward compatibility).
--- Warning: does not work for Command options!
setConfig :: WidgetRef -> ConfItem -> GuiPort -> IO ()
setConfig (WRefLabel wpv var wtype) confitem gport = do
  checkWishConsistency wpv gport
  send2tk (config2tcl wtype wpv (wRefname2Label var) confitem) gport


selectEvent evwidget evtype [] _ =
  trace ("Internal error in GUI.curry: no handler for event: "++evwidget++evtype++"\n")
        (return [])
selectEvent evwidget evtype ((ev,hevtype,handler):evs) gport =
  if evwidget==ev && event2tcl hevtype == evtype
  then handler gport
  else selectEvent evwidget evtype evs gport


-- get the current value of a widget <w>" by
-- 1. executing the Tcl procedure "putvar [getvar_<w>]"
-- 2. reading the message ":VAR<w>%<len>*<value>
--    (where <len> is the length of <value> which can be more than one line)
getWidgetVar :: String -> GuiPort -> IO String
getWidgetVar var gport = do
  send2tk ("putvar "++var++" [getvar"++var++"]") gport
  getWidgetVarMsg var gport

getWidgetVarMsg var gport =
  receiveFromTk gport >>= \varmsg ->
  if takeWhile (/='%') varmsg == ":VAR"++var
  then let (len,value) = break (=='*') (tail (dropWhile (/='%') varmsg))
        in getWidgetVarValue (readNat len) (tail value) gport
  else do reportTclTkError ("ERROR in getWidgetVar \""++var++"\": Received: "
                            ++varmsg++"\n")
          getWidgetVarMsg var gport -- ignore other messages and try again

getWidgetVarValue len valmsg gport =
  if length valmsg < len
  then do remvalmsg <- getWidgetVarRemValue (len - (length valmsg + 1)) gport
          return (valmsg++"\n"++remvalmsg)
  else do if length valmsg > len
            then reportTclTkError ("ERROR in getWidgetVar: answer too short\n")
            else done
          return valmsg

getWidgetVarRemValue len gport =
  receiveFromTk gport >>= \valmsg ->
  if length valmsg < len
  then getWidgetVarRemValue (len - (length valmsg + 1)) gport >>= \remvalmsg ->
       return (valmsg++"\n"++remvalmsg)
  else do if length valmsg > len
            then reportTclTkError ("ERROR in getWidgetVar: answer too short\n")
            else done
          return valmsg


-- Check consistency of access to widget variables via GUI ports, i.e.,
-- check whether the accessed variable really belongs to the GUI referenced
-- by the GUI port.
checkWishConsistency wp1 wp2 =
 if wp1==wp2 then done else
  trace "Inconsistent use of Tk ports during access to Tk variables\n" failed


-- escape some Tcl special characters (brackets, dollars):
escape_tcl [] = []
escape_tcl (c:s) = if c=='[' || c==']' || c=='$' || c=='"' || c=='\\'
                   then '\\':c:escape_tcl s
                   else c:escape_tcl s


----------------------------------------------------------------------------
-- Some useful IO actions for implementing event handlers...
----------------------------------------------------------------------------

--- An event handler for terminating the GUI.
exitGUI :: GuiPort -> IO ()
exitGUI gport = send2tk "exit" gport -- this also terminates the scheduler
                                     -- due to EOF on the gport handle

--- Gets the (String) value of a variable in a GUI.
getValue :: WidgetRef -> GuiPort -> IO String
getValue (WRefLabel wpv var _) gport = do
  checkWishConsistency wpv gport
  getWidgetVar var gport

--- Sets the (String) value of a variable in a GUI.
setValue :: WidgetRef -> String -> GuiPort -> IO ()
setValue (WRefLabel wpv var _) val gport = do
  checkWishConsistency wpv gport
  send2tk ("setvar"++var++" \""++escape_tcl val++"\"") gport

--- Updates the (String) value of a variable w.r.t. to an update function.
updateValue :: (String->String) -> WidgetRef -> GuiPort -> IO ()
updateValue upd wref gport = do
  val <- getValue wref gport
  setValue wref (upd val) gport

--- Appends a String value to the contents of a TextEdit widget and
--- adjust the view to the end of the TextEdit widget.
appendValue :: WidgetRef -> String -> GuiPort -> IO ()
appendValue (WRefLabel wpv var wtype) val gport =
  if wtype/="textedit"
  then trace ("WARNING: GUI.appendValue ignored for widget type \""++wtype++"\"\n") done
  else checkWishConsistency wpv gport >>
       send2tk (wRefname2Label var++" insert end \""++escape_tcl val++"\"") gport >>
       send2tk (wRefname2Label var++" see end") gport

--- Appends a String value with style tags to the contents of a TextEdit widget
--- and adjust the view to the end of the TextEdit widget.
--- Different styles can be combined, e.g., to get bold blue text on a 
--- red background. If <code>Bold</code>, <code>Italic</code> and 
--- <code>Underline</code> are combined, currently all but one of these are 
--- ignored.
--- This is an experimental function and might be changed in the future.
appendStyledValue :: WidgetRef -> String -> [Style] -> GuiPort -> IO ()
appendStyledValue (WRefLabel wpv var wtype) val styles gport =
  if wtype/="textedit"
   then trace ("WARNING: GUI.appendStyledValue ignored for widget type \""++wtype++"\"\n") done
   else checkWishConsistency wpv gport >>
        send2tk (wRefname2Label var++" insert end \""++escape_tcl val++"\""
                 ++" \""++showStyles styles++"\"") gport >>
        send2tk (wRefname2Label var++" see end") gport
 where
  showStyles = foldr (\st s -> showStyle st ++ " " ++ s) ""


--- Adds a style value in a region of a TextEdit widget.
--- The region is specified a start and end position similarly
--- to <code>getCursorPosition</code>.
--- Different styles can be combined, e.g., to get bold blue text on a 
--- red background. If <code>Bold</code>, <code>Italic</code> and 
--- <code>Underline</code> are combined, currently all but one of these are 
--- ignored.
--- This is an experimental function and might be changed in the future.
addRegionStyle :: WidgetRef -> (Int,Int) -> (Int,Int) -> Style -> GuiPort
               -> IO ()
addRegionStyle (WRefLabel wpv var wtype) (l1,c1) (l2,c2) style gport =
  if wtype/="textedit"
  then trace ("WARNING: GUI.setRegionStyle ignored for widget type \""++wtype++"\"\n") done
  else checkWishConsistency wpv gport >>
       send2tk (wRefname2Label var++" tag add "++showStyle style++" "++
                show l1++"."++show c1++" "++show l2++"."++show c2) gport


--- Removes a style value in a region of a TextEdit widget.
--- The region is specified a start and end position similarly
--- to <code>getCursorPosition</code>.
--- This is an experimental function and might be changed in the future.
removeRegionStyle :: WidgetRef -> (Int,Int) -> (Int,Int) -> Style -> GuiPort
                  -> IO ()
removeRegionStyle (WRefLabel wpv var wtype) (l1,c1) (l2,c2) style gport =
  if wtype/="textedit"
  then trace ("WARNING: GUI.setRegionStyle ignored for widget type \""++wtype++"\"\n") done
  else checkWishConsistency wpv gport >>
       send2tk (wRefname2Label var++" tag remove "++showStyle style++" "++
                show l1++"."++show c1++" "++show l2++"."++show c2) gport


--- Get the position (line,column) of the insertion cursor in a TextEdit
--- widget. Lines are numbered from 1 and columns are numbered from 0.
getCursorPosition :: WidgetRef -> GuiPort -> IO (Int,Int)
getCursorPosition (WRefLabel wpv var wtype) gport =
  if wtype/="textedit"
  then error ("GUI.getCursorPosition not applicable to widget type \""++
              wtype++"\"")
  else do checkWishConsistency wpv gport
          send2tk ("puts [ "++wRefname2Label var++" index insert ]") gport
          line <- receiveFromTk gport
          let (ls,ps) = break (=='.') line
          return (if null ps then (0,0) else (readNat ls, readNat (tail ps)))


--- Adjust the view of a TextEdit widget so that the specified line/column
--- character is visible.
--- Lines are numbered from 1 and columns are numbered from 0.
seeText :: WidgetRef -> (Int,Int) -> GuiPort -> IO ()
seeText (WRefLabel wpv var wtype) (line,column) gport =
  if wtype/="textedit"
  then trace ("WARNING: GUI.seeText ignored for widget type \""++wtype++"\"\n") done
  else checkWishConsistency wpv gport >>
       send2tk (wRefname2Label var++" see "++show line++"."++show column) gport


--- Sets the input focus of this GUI to the widget referred by the first
--- argument.
--- This is useful for automatically selecting input entries in an application.
focusInput :: WidgetRef -> GuiPort -> IO ()
focusInput (WRefLabel wpv var _) gport = do
  checkWishConsistency wpv gport
  send2tk ("focus "++wRefname2Label var) gport

--- Adds a list of canvas items to a canvas referred by the first argument.
addCanvas :: WidgetRef -> [CanvasItem] -> GuiPort -> IO ()
addCanvas (WRefLabel wpv var wtype) items gport = do
  checkWishConsistency wpv gport
  send2tk (config2tcl wtype wpv (wRefname2Label var) (CanvasItems items)) gport


----------------------------------------------------------------------------
-- Example GUIs:
----------------------------------------------------------------------------

--- A simple popup message.
popup_message :: String -> IO ()
popup_message s = runGUI "" (Col [] [Label [Text s],
                                     Button exitGUI [Text "Dismiss"]])

--- A simple event handler that can be associated to a widget.
--- The event handler takes a GUI port as parameter in order to
--- read or write values from/into the GUI.
Cmd :: (GuiPort -> IO ()) -> ConfItem
Cmd cmd = Command (\gport -> cmd gport >> return [])

--- An event handler that can be associated to a widget.
--- The event handler takes a GUI port as parameter (in order to
--- read or write values from/into the GUI) and returns a list
--- of widget reference/configuration pairs
--- which is applied after the handler in order to configure some GUI widgets.
Command :: (GuiPort -> IO [ReconfigureItem]) -> ConfItem
Command cmd = Handler DefaultEvent cmd

--- A button with an associated event handler which is activated
--- if the button is pressed.
Button :: (GuiPort -> IO ()) -> [ConfItem] -> Widget
Button cmd confs = PlainButton (Cmd cmd : confs)


--- A button with an associated event handler which is activated
--- if the button is pressed. The event handler is a configuration handler
--- (see Command) that allows the configuration of some widgets.
ConfigButton :: (GuiPort -> IO [ReconfigureItem]) -> [ConfItem] -> Widget
ConfigButton cmd confs = PlainButton (Command cmd : confs)


--- A text edit widget with vertical and horizontal scrollbars.
--- The argument contains the configuration options for the text edit widget.
TextEditScroll :: [ConfItem] -> Widget
TextEditScroll confs =
   Matrix [] [[TextEdit ([WRef txtref, Fill]++confs),
               ScrollV txtref [FillY]],
              [ScrollH txtref [FillX]]]     where txtref free


--- A list box widget with vertical and horizontal scrollbars.
--- The argument contains the configuration options for the list box widget.
ListBoxScroll :: [ConfItem] -> Widget
ListBoxScroll confs =
   Matrix [] [[ListBox ([WRef lbref, Fill]++confs),
               ScrollV lbref [FillY]],
              [ScrollH lbref [FillX]]]     where lbref free


--- A canvas widget with vertical and horizontal scrollbars.
--- The argument contains the configuration options for the text edit widget.
CanvasScroll :: [ConfItem] -> Widget
CanvasScroll confs =
   Col []
     [Row [] [Canvas ([WRef cref, Fill]++confs),
              ScrollV cref [FillY]],
      ScrollH cref [FillX]]     where cref free


--- An entry widget with a horizontal scrollbar.
--- The argument contains the configuration options for the entry widget.
EntryScroll :: [ConfItem] -> Widget
EntryScroll confs =
   Col []
    [Entry ([WRef entryref, FillX]++confs),
     ScrollH entryref [Width 10, FillX]]
  where entryref free


--- Pops up a GUI for selecting an existing file.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
getOpenFile :: IO String
getOpenFile = getOpenFileWithTypes []

--- Pops up a GUI for selecting an existing file. The parameter is
--- a list of pairs of file types that could be selected.
--- A file type pair consists of a name and an extension for that file type.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
getOpenFileWithTypes :: [(String,String)] -> IO String
getOpenFileWithTypes filetypes = do
  gport <- openWish "" ""
  send2tk ("wm withdraw .\nputs [tk_getOpenFile" ++
              (if null filetypes then "" else
               " -filetypes {"++
               concatMap (\(x,y)->"{{"++x++"} {"++y++"}} ") filetypes ++"}") ++
              "]\n") gport
  filename <- receiveFromTk gport
  exitGUI gport
  return filename


--- Pops up a GUI for choosing a file to save some data.
--- If the user chooses an existing file, she/he will asked to confirm
--- to overwrite it.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
getSaveFile :: IO String
getSaveFile = getSaveFileWithTypes []

--- Pops up a GUI for choosing a file to save some data. The parameter is
--- a list of pairs of file types that could be selected.
--- A file type pair consists of a name and an extension for that file type.
--- If the user chooses an existing file, she/he will asked to confirm
--- to overwrite it.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
getSaveFileWithTypes :: [(String,String)] -> IO String
getSaveFileWithTypes filetypes = do
  gport <- openWish "" ""
  send2tk ("wm withdraw .\nputs [tk_getSaveFile" ++
              (if null filetypes then "" else
               " -filetypes {"++
               concatMap (\(x,y)->"{{"++x++"} {"++y++"}} ") filetypes ++"}") ++
              "]\n") gport
  filename <- receiveFromTk gport
  exitGUI gport
  return filename


--- Pops up a GUI dialog box to select a color.
--- The name of the color will be returned (or "" if the user
--- cancels the selection).
chooseColor :: IO String
chooseColor = do
  gport <- openWish "" ""
  send2tk "wm withdraw .\nputs [tk_chooseColor]" gport
  color <- receiveFromTk gport
  exitGUI gport
  return color


-- end of GUI library
