------------------------------------------------------------------------------
--- Library for GUI programming in Curry (based on Tcl/Tk).
--- <a href="http://www.informatik.uni-kiel.de/~mh/papers/PADL00.html">
--- This paper</a> contains a description of the basic ideas
--- behind this library.
---
--- @authors Michael Hanus, Bernd Brassel
--- @version November 2004
------------------------------------------------------------------------------

module Tk(TkWidget(..),TkButton,TkConfigButton,
          TkTextEditScroll,TkCanvasScroll,TkEntryScroll,
          TkConfItem(..),TkCmd,TkConfigCmd,TkConfCollection(..),TkMenuItem(..),
          TkCanvasItem(..),TkRefType,
          runWidget,runWidgetInit,runWidgetPassive,
          runControlledWidget,runConfigControlledWidget,
          runControlledWidgetInit,
          runWidgetOnPort,runControlledWidgetOnPort,runWidgetOnPortInit,
          forkWish,openWish,
          tkVoid,tkExit,tkGetValue,tkSetValue,tkUpdate,tkAppendValue,
          tkConfig,tkFocus,tkAddCanvas,tkAppendTaggedValue,
          tkCVoid,tkCExit,tkCGetValue,tkCSetValue,tkCUpdate,
          tkCConfig,tkCFocus,tkCAddCanvas,
          popup_message,
          tkGetOpenFile,tkGetOpenFileWithTypes,
          tkGetSaveFile,tkGetSaveFileWithTypes,
          tkChooseColor,debugTcl)  where

import Ports
import Read
import Unsafe(trace)

-- if tkShowErrors is true, all synchronization errors in Tcl/Tk communication
-- are shown (such errors should only occur on slow machines in exceptional
-- cases; they should be handled by this library but might be interesting
-- to see for debugging)
tkShowErrors = False

------------------------------------------------------------------------
-- the basic data types for GUIs:
------------------------------------------------------------------------

--- The type of possible widgets in a GUI.
--- "a" is the result type of event handlers, currently "IO ()" or "Success".
data TkWidget a = TkPlainButton            [TkConfItem a]
                | TkCanvas                 [TkConfItem a]
                | TkCheckButton            [TkConfItem a]
                | TkEntry                  [TkConfItem a]
                | TkLabel                  [TkConfItem a]
                | TkListBox                [TkConfItem a]
                | TkMessage                [TkConfItem a]
                | TkMenuButton             [TkConfItem a]
                | TkScale Int Int          [TkConfItem a]
                | TkScrollV TkRefType      [TkConfItem a]
                | TkScrollH TkRefType      [TkConfItem a]
                | TkTextEdit               [TkConfItem a]
                | TkRow [TkConfCollection] [TkWidget a] -- horizontal alignment
                | TkCol [TkConfCollection] [TkWidget a] -- vertical alignment
                | TkMatrix [TkConfCollection] [[TkWidget a]] -- matrix alignment

--- The data type for possible configurations of a widget.
data TkConfItem a =
   TkActive Bool            -- active state for buttons, entries, etc.
 | TkAnchor String          -- alignment of information in a widget, arg. must
                            -- be: n, ne, e, se, s, sw, w, nw, or center
 | TkBackground String      -- the background color
 | TkForeground String      -- the foreground color
 | TkHandler ([(TkRefType,TkConfItem a)] -> Port SP_Msg -> a)
    -- An associated event handler. The event handler must instantiate
    -- its first argument with a list of widget ref/configuration pairs
    -- which is applied after the handler in order to configure the GUI widgets
 | TkHeight Int             -- the height of a widget
                            -- (chars for text, pixels for graphics
 | TkInit String            -- initial value for checkbuttons
 | TkItems [TkCanvasItem]   -- list of items in a canvas
 | TkList [String]          -- value list in a listbox
 | TkMenu [TkMenuItem a]    -- the items of a menu button
 | TkRef TkRefType          -- a reference to this widget
 | TkText String            -- an initial text contents
 | TkWidth Int              -- the width of a widget
                            -- (chars for text, pixels for graphics
 | TkFill                   -- fill widget in both directions
 | TkFillX                  -- fill widget in horizontal direction
 | TkFillY                  -- fill widget in vertical direction
 | TkTcl String             -- further options in Tcl syntax (unsafe!)


--- The data type for possible configurations of a collection of widgets.
data TkConfCollection =
   TkCenter  -- centered alignment
 | TkLeft    -- left alignment
 | TkRight   -- right alignment
 | TkTop     -- top alignment
 | TkBottom  -- bottom alignment

--- The data type for specifying items in a menu.
data TkMenuItem a =
   TkMButton (Port SP_Msg -> a) String  -- a button with an associated command
                                        -- and a label string
 | TkMSeparator                         -- a separator between entries
 | TkMMenuButton String [TkMenuItem a]  -- a submenu ("cascade" in Tk)

--- The data type of items in a canvas.
--- The last argument are further options in Tcl/Tk (for testing).
data TkCanvasItem = TkLine [(Int,Int)] String
                  | TkPolygon [(Int,Int)] String
                  | TkRectangle (Int,Int) (Int,Int) String
                  | TkOval (Int,Int) (Int,Int) String
                  | TkCText (Int,Int) String String


--- The (hidden) data type of references to a widget in a Tk window.
--- Note that the constructor TkRefLabel will not be exported so that values
--- can only be created inside this module.
--- @cons TkRefLabel wp label type - here "wp" is the GUI port related
---       to the widget, "label" is the (globally unique) identifier of
---       this widget used in Tk, and "type" is one of
---       button / canvas / checkbutton / entry / label / listbox /
---       message / scale / scrollbar / textedit
data TkRefType = TkRefLabel (Port SP_Msg) String String

tkRef2Label (TkRefLabel _ var _)   = tkRefname2Label var
tkRef2Wtype (TkRefLabel _ _ wtype) = wtype


------------------------------------------------------------------------
-- internal translation functions from GUI terms into Tcl:
------------------------------------------------------------------------

-- translate a widget into a pair of Tcl command string / event list
-- argument 1: port for Tk GUI
-- argument 2: current label prefix
-- argument 3: the widget to translate
-- result: pair of (Tcl command string, list of (eventname, eventhandler))

--tk2tcl :: (Port SP_Msg) -> String -> TkWidget a
--    -> (String,[(String, Either (Port SP_Msg -> a)
--                        (Port SP_Msg -> IO (TkRefType,TkConfItem (IO ()))))])
tk2tcl wp label (TkPlainButton confs) =
    ("button "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "button" wp label confs

tk2tcl wp label (TkCanvas confs) =
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
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "canvas" wp label confs

tk2tcl wp label (TkCheckButton confs) =
    ("checkbutton "++label++"\n" ++
     label++" configure -variable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "checkbutton" wp label confs

tk2tcl wp label (TkEntry confs) =
    ("entry "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "entry" wp label confs

tk2tcl wp label (TkLabel confs) =
    ("label "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "label" wp label confs

tk2tcl wp label (TkListBox confs) =
    ("listbox "++label++" -exportselection false\n" ++
     "proc getvar"++refname++" {} { return ["++label++" curselection]}\n" ++
     "proc setvar"++refname++" {s} { "++label++" selection clear 0 end ; "
             ++label++" selection set $s ; "++label++" see $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "listbox" wp label confs

tk2tcl wp label (TkMessage confs) =
    ("message "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "message" wp label confs

tk2tcl wp label (TkMenuButton confs) =
    ("menubutton "++label++"\n" ++
     label++" configure -textvariable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "menubutton" wp label confs

tk2tcl wp label (TkScale from to confs) =
    ("scale "++label++" -from "++show from++" -to "++show to++
     " -orient horizontal -length 200\n" ++
     "variable "++refname++" "++show from++"\n"++  -- initialize scale variable
     label++" configure -variable "++refname++"\n" ++
     "proc getvar"++refname++" {} { global "++refname++" ; return $"
                                                     ++refname++" }\n" ++
     "proc setvar"++refname++" {s} { global "++refname++" ; set "
                                                     ++refname++" $s}\n" ++
     conf_tcl , conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "scale" wp label confs

tk2tcl wp label (TkScrollH widget confs) =
    ("scrollbar "++label++" -orient horizontal -command {"++
                                         tkRef2Label widget++" xview}\n" ++
     tkRef2Label widget++" configure -xscrollcommand {"++label++" set}\n" ++
     tkRef2Label widget++" configure -wrap none\n" ++ -- no line wrap
     conf_tcl , conf_evs)
   where (conf_tcl,conf_evs) = tkConfs2tcl "scrollbar" wp label confs

tk2tcl wp label (TkScrollV widget confs) =
    ("scrollbar "++label++" -command {"++tkRef2Label widget++" yview}\n" ++
     tkRef2Label widget++" configure -yscrollcommand {"++label++" set}\n" ++
     conf_tcl , conf_evs)
   where (conf_tcl,conf_evs) = tkConfs2tcl "scrollbar" wp label confs

tk2tcl wp label (TkTextEdit confs) =
    ("text "++label++"\n"++ --" -height 15\n" ++
     "proc getvar"++refname++" {} { "++label++" get 1.0 {end -1 chars}}\n" ++
     "proc setvar"++refname++" {s} { "++label++" delete 1.0 end ; "
                                      ++label++" insert 1.0 $s}\n" ++
     conf_tcl
     ++label++" tag configure gray -foreground gray\n", conf_evs)
   where refname = tkLabel2Refname label
         (conf_tcl,conf_evs) = tkConfs2tcl "textedit" wp label confs

tk2tcl wp label (TkRow confs ws) =
  ((if label=="" then "wm resizable . " ++ resizeBehavior wsGridInfo++"\n"
    else "frame "++label++"\n") ++
   wstcl ++
   (snd $ foldl (\ (n,g) l->(n+1,g++"grid "++label ++ labelIndex2string (96+n)
                                  ++" -row 1 -column "++show n++" "
                                  ++tkConfCollection2tcl confs
                                  ++tkGridInfo2tcl n label "col" l ++ "\n")) 
                (1,"")
                wsGridInfo),
   wsevs)
  where (wstcl,wsevs) = tks2tcl wp label 97 ws
        wsGridInfo = widgets2gridinfo ws
        

tk2tcl wp label (TkCol confs ws) =
  ((if label=="" then "wm resizable . " ++ resizeBehavior wsGridInfo++"\n"
    else "frame "++label++"\n") ++
      wstcl ++
      (snd $ foldl (\ (n,g) l->(n+1,g++"grid "++label ++ labelIndex2string (96+n)
                                     ++" -column 1 -row "++show n++" "
                                     ++tkConfCollection2tcl confs
                                     ++tkGridInfo2tcl n label "row" l ++ "\n"))
                   (1,"")
                   (widgets2gridinfo ws)),
      wsevs)
  where (wstcl,wsevs) = tks2tcl wp label 97 ws
        wsGridInfo = widgets2gridinfo ws

tk2tcl wp label (TkMatrix confs ws) = 
  ((if label == "" then "wm resizable . " ++ resizeBehavior wsGridInfo++"\n" 
    else "frame "++label++"\n") ++ wstcl,wsevs)

  where
    (wstcl,wsevs) =  tkMatrix2tcl 97 1 wp label confs ws
    wsGridInfo = concatMap widgets2gridinfo ws

-- actual translation function of the list of lists of widgets in a matrix
tkMatrix2tcl :: Int -> Int -> Port SP_Msg -> String -> [TkConfCollection] 
                    -> [[TkWidget a]]
         -> (String,[(String,[(TkRefType,TkConfItem a)] -> Port SP_Msg -> a)])
tkMatrix2tcl _ _ _ _ _ [] = ("",[])
tkMatrix2tcl nextLabel n wp label confs (ws:wss) =
   (wstcl ++ 
   (snd $ foldl (\ (m,g) l->(m+1,g++"grid "++label ++ labelIndex2string (nextLabel+m-1)
                                  ++" -row "++show n ++" -column "++show m++" "
                                  ++tkConfCollection2tcl confs
                                  ++tkGridInfo2tcl m label "col" l ++ "\n"))
                (1,"")
                wsGridInfo) ++ wsstcl, wsevs++wssevs)
  where (wsstcl,wssevs) = tkMatrix2tcl (nextLabel+length ws) (n+1) wp label confs wss
        (wstcl,wsevs) = tks2tcl wp label nextLabel ws
        wsGridInfo = widgets2gridinfo ws

-- compute the required resize behavior of the top window
resizeBehavior :: [[TkConfItem _]] -> String
resizeBehavior ws = if any (elem TkFill) ws then "1 1" else
                    if any (elem TkFillX) ws then "1 0" else
                    if any (elem TkFillY) ws then "0 1" else "0 0"


-- list of labels of the widgets
widgets2gridinfo [] = []
widgets2gridinfo (w:ws) =
    (tclfill ++ getConfs w): widgets2gridinfo ws
 where
  fillx    = hasFillX w 
  filly    = hasFillY w 
  flexible = hasFill  w 
  tclfill  = if flexible || (fillx && filly) then [TkFill] else
             if fillx then [TkFillX] else
             if filly then [TkFillY] else []
             
hasFillX w = any isFillXConf (propagateFillInfo w)
isFillXConf conf = case conf of
  TkFillX -> True
  _       -> False

hasFillY w = any isFillYConf (propagateFillInfo w)
isFillYConf conf = case conf of
  TkFillY -> True
  _       -> False

hasFill w = any isFillConf (propagateFillInfo w)
isFillConf conf = case conf of
  TkFill -> True
  _      -> False

isFillInfo conf = case conf of
                    TkFillX -> True
                    TkFillY -> True
                    TkFill  -> True
                    _       -> False

-- propagate FillInfo for those kinds of widgets which are resizable on their on
propagateFillInfo (TkPlainButton _)     = []
propagateFillInfo (TkCanvas      confs) = filter isFillInfo confs
propagateFillInfo (TkCheckButton _)     = []
propagateFillInfo (TkEntry       confs) = filter isFillInfo confs
propagateFillInfo (TkLabel       confs) = filter isFillInfo confs
propagateFillInfo (TkListBox     confs) = filter isFillInfo confs
propagateFillInfo (TkMessage     confs) = filter isFillInfo confs
propagateFillInfo (TkMenuButton  _)     = []
propagateFillInfo (TkScale _ _   confs) = filter isFillInfo confs
propagateFillInfo (TkScrollV _   _)     = []
propagateFillInfo (TkScrollH _   _)     = []
propagateFillInfo (TkTextEdit    confs) = filter isFillInfo confs
propagateFillInfo (TkRow _ ws) = concatMap propagateFillInfo ws
propagateFillInfo (TkCol _ ws) = concatMap propagateFillInfo ws
propagateFillInfo (TkMatrix _ wss) = concatMap (concatMap propagateFillInfo) wss

-- get the configurations of a widget
getConfs (TkPlainButton confs) = confs
getConfs (TkCanvas      confs) = filter isFillInfo confs
getConfs (TkCheckButton confs) = confs
getConfs (TkEntry       confs) = filter isFillInfo confs
getConfs (TkLabel       confs) = filter isFillInfo confs
getConfs (TkListBox     confs) = filter isFillInfo confs
getConfs (TkMessage     confs) = filter isFillInfo confs
getConfs (TkMenuButton  confs) = confs
getConfs (TkScale _ _   confs) = filter isFillInfo confs
getConfs (TkScrollV _   confs) = confs
getConfs (TkScrollH _   confs) = confs
getConfs (TkTextEdit    confs) = filter isFillInfo confs
getConfs (TkRow _ _)           = []
getConfs (TkCol _ _)           = []
getConfs (TkMatrix _ _)        = [] 


-- translate configuration options for collections (rows or columns)
-- into parameters for the Tcl/Tk command "grid":
tkConfCollection2tcl [] = ""
tkConfCollection2tcl (TkCenter : confs) = tkConfCollection2tcl confs
tkConfCollection2tcl (TkLeft : confs)   = "-sticky w " ++ tkConfCollection2tcl confs
tkConfCollection2tcl (TkRight : confs)  = "-sticky e " ++ tkConfCollection2tcl confs
tkConfCollection2tcl (TkTop : confs)    = "-sticky n " ++ tkConfCollection2tcl confs
tkConfCollection2tcl (TkBottom : confs) = "-sticky s " ++ tkConfCollection2tcl confs

-- translate the TkFill - options to sticky options and grid configures
tkGridInfo2tcl :: Int -> String -> String -> [TkConfItem _] -> String
tkGridInfo2tcl n label "col" confs 
  | elem TkFill confs || (elem TkFillX confs && elem TkFillY confs)
  = "-sticky nsew \ngrid columnconfigure "++lab++" "++show n++
    " -weight 1\ngrid rowconfigure "++lab++" 1 -weight 1"
  | elem TkFillX confs = "-sticky we \ngrid columnconfigure "++lab++
                         " "++show n++" -weight 1"
  | elem TkFillY confs = "-sticky ns \ngrid rowconfigure "++lab++
                         " 1 -weight 1"
  | otherwise = ""
  where
    lab = if label=="" then "." else label

tkGridInfo2tcl n label "row" confs 
  | elem TkFill confs || (elem TkFillX confs && elem TkFillY confs)
  = "-sticky nsew \ngrid columnconfigure "++lab++
    " 1 -weight 1\ngrid rowconfigure "++lab++" "++show n++" -weight 1"
  | elem TkFillX confs = "-sticky we \ngrid columnconfigure "++lab++
                         " 1 -weight 1"
  | elem TkFillY confs =  "-sticky ns \ngrid rowconfigure "++lab++
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
tkConf2tcl :: String -> Port SP_Msg -> String -> TkConfItem _ -> String

-- is the state of the widget active ("normal" in Tcl/Tk) or
-- inactive ("disabled" in Tcl/Tk)?
-- (inactive widgets do not accept any events)
tkConf2tcl wtype _ label (TkActive active) =
  if wtype=="button" || wtype=="checkbutton" || wtype=="entry" ||
     wtype=="menubutton" || wtype=="scale" || wtype=="textedit"
  then if active
       then label++" configure -state normal\n"
       else label++" configure -state disabled\n"
  else trace ("WARNING: TkActive ignored for widget type \""++wtype++"\"\n") ""

-- alignment of information inside a widget
-- argument must be: n, ne, e, se, s, sw, w, nw, or center
tkConf2tcl wtype _ label (TkAnchor align) =
  if wtype=="button" || wtype=="checkbutton" || wtype=="label" ||
     wtype=="menubutton" || wtype=="message"
  then label++" configure -anchor "++align++"\n"
  else trace ("WARNING: TkAnchor ignored for widget type \""++wtype++"\"\n") ""

-- background color:
tkConf2tcl _ _ label (TkBackground color)
 = label++" configure -background "++color++"\n"

-- foreground color:
tkConf2tcl _ _ label (TkForeground color)
 = label++" configure -foreground "++color++"\n"

-- command associated to various widgets:
tkConf2tcl wtype _ label (TkHandler _)
 | wtype=="button"
   = label++" configure -command { writeevent \""++label++"\" }\n"
 | wtype=="checkbutton"
   = label++" configure -command { writeevent \""++label++"\" }\n"
 | wtype=="entry"
   = "bind "++label++" <Return> { writeevent \""++label++"\" }\n"
 | wtype=="scale"
   = label++" configure -command { putlabel \""++label++"\"}\n"
 | wtype=="listbox"
   = "bind "++label++" <ButtonPress-1> { writeevent \""++label++"\"}\n"
 | wtype=="textedit"
   = "bind "++label++" <KeyPress> { writeevent \""++label++"\"}\n"
 | otherwise
   = trace ("WARNING: TkHandler ignored for widget type \""++wtype++"\"\n") ""

-- height of a widget (not defined for all widget types):
tkConf2tcl wtype _ label (TkHeight h)
 | wtype=="entry" || wtype=="message" || wtype=="menubutton" ||
   wtype=="scale"
  = trace ("WARNING: TkHeight ignored for widget type \""++wtype++"\"\n") ""
 | wtype=="canvas"
  = label++" configure -height "++show h++"\n"++
    "set"++tkLabel2Refname label++"_scrolly "++show h++"\n"
 | otherwise
  = label++" configure -height "++show h++"\n"

-- value of checkbuttons:
tkConf2tcl wtype _ label (TkInit s)
 | wtype=="checkbutton"
   = "setvar"++tkLabel2Refname label++" \""++s++"\"\n"
 | otherwise
 = trace ("WARNING: TkInit ignored for widget type \""++wtype++"\"\n") ""

-- items in a canvas:
tkConf2tcl wtype _ label (TkItems items)
 | wtype=="canvas" = tkcitems2tcl label items
 | otherwise
 = trace ("WARNING: TkItems ignored for widget type \""++wtype++"\"\n") ""

-- value lists for listboxes:
tkConf2tcl wtype _ label (TkList l)
 | wtype=="listbox"
   = label++" delete 0 end\n" ++ setlistelems (ensureSpine l)
 | otherwise
 = trace ("WARNING: TkList ignored for widget type \""++wtype++"\"\n") ""

 where
   setlistelems [] = ""
   setlistelems (e:es) = label++" insert end \""++escape_tcl e++"\"\n"++
                         setlistelems es

-- items in a menu button:
tkConf2tcl wtype _ label (TkMenu l)
 | wtype=="menubutton"
   = label++" configure -menu "++label++".a\n" ++
     tkMenu2tcl (label++".a") l
 | otherwise
 = trace ("WARNING: TkMenu ignored for widget type \""++wtype++"\"\n") ""

-- references to widgets are bound to actual widget labels:
tkConf2tcl wtype wp label (TkRef r)
 | r =:= TkRefLabel wp (tkLabel2Refname label) wtype = ""

-- initial text value of widgets:
tkConf2tcl wtype _ label (TkText s)
 | wtype=="canvas"
   = trace "WARNING: TkText ignored for TkCanvas\n" ""
 | wtype=="checkbutton"
   = label++" configure -text \""++escape_tcl s++"\"\n"
 | otherwise
   = "setvar"++tkLabel2Refname label++" \""++escape_tcl s++"\"\n"

-- width of a widget:
tkConf2tcl wtype _ label (TkWidth w)
 | wtype=="canvas"
   = label++" configure -width "++show w++"\n"++
     "set"++tkLabel2Refname label++"_scrollx "++show w++"\n"
 | otherwise = label++" configure -width "++show w++"\n"

-- configuration options for widget composition are ignored here
-- since they are used during geometry management 
tkConf2tcl _ _ _ TkFill = ""
tkConf2tcl _ _ _ TkFillX = ""
tkConf2tcl _ _ _ TkFillY = ""

-- for testing, put arbitrary Tk options for this widget:
tkConf2tcl _ _ label (TkTcl tcloptions)
 = label++" configure "++tcloptions++"\n"


-- translation of a menu with a given label:
tkMenu2tcl label menu =
  "menu "++label++" -tearoff false\n" ++
  label++" delete 0 end\n" ++ 
  setmenuelems menu 0
 where setmenuelems [] _ = ""
       setmenuelems (TkMButton _ text : es) i =
          label++" add command -label \""++escape_tcl text++
                "\" -command { writeevent \""++label++"."++show i++"\" }\n"++
          setmenuelems es (i+1)
       setmenuelems (TkMSeparator : es) i =
          label++" add separator\n"++ setmenuelems es (i+1)
       setmenuelems (TkMMenuButton text l : es) i =
          label++" add cascade -label \""++escape_tcl text++
                "\" -menu "++label++labelIndex2string (i+97)++"\n"++
          tkMenu2tcl (label++labelIndex2string (i+97)) l ++
          setmenuelems es (i+1)



-- get the event handlers in a list of configuration options:
-- and bind widget references:
tkConfs2handler :: String -> [TkConfItem a]
                   -> [(String,[(TkRefType,TkConfItem a)] -> Port SP_Msg -> a)]

tkConfs2handler _ [] = []
tkConfs2handler label (confitem : cs) = case confitem of
  TkHandler handler -> (label,handler) : tkConfs2handler label cs
  TkMenu m          -> tkMenu2handler (label++".a") m 0 ++ tkConfs2handler label cs
  _                 -> tkConfs2handler label cs

tkMenu2handler _ [] _ = []
tkMenu2handler label (TkMButton handler _ : ms) i =
         (label++"."++show i, confighandler) : tkMenu2handler label ms (i+1)
 where confighandler [] wport = handler wport
tkMenu2handler label (TkMSeparator : ms) i = tkMenu2handler label ms (i+1)
tkMenu2handler label (TkMMenuButton _ menu : ms) i =
         tkMenu2handler (label++labelIndex2string (i+97)) menu 0 ++ tkMenu2handler label ms (i+1)

-- translate configuration options into Tcl/Tk commands and event handler map:
tkConfs2tcl :: String -> Port SP_Msg -> String -> [TkConfItem a]
          -> (String,[(String,[(TkRefType,TkConfItem a)] -> Port SP_Msg -> a)])
tkConfs2tcl wtype wp label confs =
  (concat (map (tkConf2tcl wtype wp label) confs),
   tkConfs2handler label confs)


-- translate a list of canvas items into a Tcl string:
tkcitems2tcl _ [] = ""
tkcitems2tcl label (i:is) = tkcitem label i ++ tkcitems2tcl label is

tkcitem label (TkLine coords opts) =
  label++ " create line "++tkShowCoords coords++" "++opts++"\n"++
  concatMap (\(x,_)->"set"++refname++"_scrollx "++show x++"\n") coords ++
  concatMap (\(_,y)->"set"++refname++"_scrolly "++show y++"\n") coords
    where refname = tkLabel2Refname label
tkcitem label (TkPolygon coords opts) =
  label++ " create polygon "++tkShowCoords coords++" "++opts++"\n"++
  concatMap (\(x,_)->"set"++refname++"_scrollx "++show x++"\n") coords ++
  concatMap (\(_,y)->"set"++refname++"_scrolly "++show y++"\n") coords
    where refname = tkLabel2Refname label
tkcitem label (TkRectangle (x1,y1) (x2,y2) opts) =
  label++ " create rectangle "++tkShowCoords [(x1,y1),(x2,y2)]++" "++opts++"\n"++
  concatMap (\x->"set"++refname++"_scrollx "++show x++"\n") [x1,x2] ++
  concatMap (\y->"set"++refname++"_scrolly "++show y++"\n") [y1,y2]
    where refname = tkLabel2Refname label
tkcitem label (TkOval (x1,y1) (x2,y2) opts) =
  label++ " create oval "++tkShowCoords [(x1,y1),(x2,y2)]++" "++opts++"\n"++
  concatMap (\x->"set"++refname++"_scrollx "++show x++"\n") [x1,x2] ++
  concatMap (\y->"set"++refname++"_scrolly "++show y++"\n") [y1,y2]
    where refname = tkLabel2Refname label
tkcitem label (TkCText (x,y) text opts) = 
  label++ " create text "++show x++" "++show y++
          " -text \""++escape_tcl text++"\" "++opts++"\n"++
  "set"++refname++"_scrollx "++show (x+5*(length text))++"\n"++
  "set"++refname++"_scrolly "++show y++"\n"
    where refname = tkLabel2Refname label

tkShowCoords [] = ""
tkShowCoords ((x,y):cs) = show x++" "++show y++" "++tkShowCoords cs


-- translate a widget label into a name (replacing dots by underscores)
tkLabel2Refname l = map (\c -> if c=='.' then '_' else c) l

-- translate a name into a widget label (replacing underscores by dots)
tkRefname2Label l = map (\c -> if c=='_' then '.' else c) l


-- translate a list of widgets into pair Tcl string / event list:
tks2tcl _ _ _ [] = ("",[])
tks2tcl wp lab nr (w:ws) = (wtcl ++ wstcl, wevs ++ wsevs)
  where (wtcl,wevs) = tk2tcl wp (lab++labelIndex2string nr) w
        (wstcl,wsevs) = tks2tcl wp lab (nr+1) ws

-- translate a label index into a textual label
-- (e.g., 97->".a" or 123->".z1"):
labelIndex2string li = if li<123 then ['.',chr li]
                                 else ['.','z'] ++ show (li-122)

-- translate main widget:
tkmain2tcl wp tkw =
  ("proc writeevent {l} { puts \":EVT$l\" }\n" ++
   "proc putlabel {l v} { writeevent $l }\n" ++
   "proc putvar {var value} { puts \":VAR$var%[string length $value]*$value\"}\n" ++
   tkwtcl, evs)
  where (tkwtcl,evs) = tk2tcl wp "" tkw


-- show the generated Tcl commands of the main widget tkw for debugging:
debugTcl tkw = putStrLn (fst (tkmain2tcl wp tkw))  where wp free


--- Initializes Tcl/Tk with some tcl text and ignores wish output.
forkWish title tcl = openWish (escape_tcl title) >>= tkSendPort (SP_Put tcl)

tkSendPort msg p | send msg p = done



------------------------------------------------------------------------
-- functions for running a GUI:
------------------------------------------------------------------------

--- Creates a new GUI window with a "title" for the top-level window
--- (but unspecified contents). A GUI port is returned that can be
--- used to start a GUI specification on this port
--- (by the operations runWidgetOnPort, runWidgetOnPortInit, or
--- runControlledWidgetOnPort).
openWish :: String -> IO (Port SP_Msg)
openWish title =
 do wport <- openProcessPort "wish"
    tkSendPort (SP_Put ("wm title . \""++title++"\"\n")) wport
    return wport

--- IO action to run a TkWidget in a new GUI window.
--- @param title - the title of the main window containing the widget
--- @param tkw - the widget shown in the new window
runWidget :: String -> TkWidget (IO ()) -> IO ()
runWidget title tkw =
 do wport <- openWish (escape_tcl title)
    initSchedule (>>) done tkConfig tkw wport (\_ _ _ -> done) [] (\_->done)


--- IO action to run a TkWidget in a new GUI window. The GUI events
--- are processed after executing an initial action on the GUI.
--- @param title - the title of the main GUI window
--- @param tkw - the widget shown in the new GUI window
--- @param initcmd - the initial command executed before activating the GUI
runWidgetInit :: String -> TkWidget (IO ()) -> (Port SP_Msg -> IO ()) -> IO ()
runWidgetInit title tkw initcmd =
 do wport <- openWish (escape_tcl title)
    initSchedule (>>) done tkConfig tkw wport (\_ _ _ -> done) [] initcmd


--- IO action to show a TkWidget in a new GUI window in passive mode,
--- i.e., ignore all GUI events.
--- @param title - the title of the main window containing the widget
--- @param tkw - the widget shown in the new window
runWidgetPassive :: String -> TkWidget _ -> IO (Port SP_Msg)
runWidgetPassive title tkw =
 do wport <- openWish (escape_tcl title)
    tkSendPort (SP_Put (fst (tkmain2tcl wport tkw))) wport
    return wport


--- Runs a TkWidget in a new GUI window and process GUI events.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- This operation is useful to run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (tkw,exth) where tkw is the widget shown in the
---             new window and exth is the event handler for external messages
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
runControlledWidget :: String ->
             (TkWidget (IO ()),msg -> Port SP_Msg -> IO ()) -> [msg] -> IO ()
runControlledWidget title (tkw,exth) msgs =
  runControlledWidgetInit title (tkw,exth) (\_->done) msgs


--- Runs a TkWidget in a new GUI window and process GUI events.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- This operation is useful to run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (tkw,exth) where tkw is the widget shown in the
---             new window and exth is the event handler for external messages
---             that returns a list of widget reference/configuration pairs
---             which is applied after the handler in order to configure
---             some GUI widgets
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
runConfigControlledWidget :: String ->
             (TkWidget (IO ()),
              msg -> Port SP_Msg -> IO [(TkRefType,TkConfItem (IO ()))]) ->
             [msg] -> IO ()
runConfigControlledWidget title (tkw,exth) msgs = do
  wport <- openWish (escape_tcl title)
  initSchedule (>>) done tkConfig tkw wport
               (\msg configs wp -> exth msg wp >>= \confs ->
                                   doSolve (confs=:=configs))
               msgs (\_->done)

--- Runs a TkWidget in a new GUI window and process GUI events
--- after executing an initial comment on the GUI window.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- This operation is useful to run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param title - the title of the main window containing the widget
--- @param th - a pair (tkw,exth) where tkw is the widget shown in the
---             new window and exth is the event handler for external messages
--- @param initcmd - the initial command executed before starting the GUI
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
runControlledWidgetInit :: String ->
                           (TkWidget (IO ()), msg -> Port SP_Msg -> IO ()) ->
                           (Port SP_Msg -> IO ()) -> [msg] -> IO ()
runControlledWidgetInit title (tkw,exth) initcmd msgs =
 do wport <- openWish (escape_tcl title)
    initSchedule (>>) done tkConfig tkw wport (\msg [] wp -> exth msg wp)
                 msgs initcmd

--- Runs a TkWidget on a GUI window that was previously created
--- by the operation "openWish".
--- This is useful to execute a GUI in parallel to other concurrent
--- processes.
--- @param tkw - the widget shown in the new window
--- @param wport - the wish port for the GUI
runWidgetOnPort :: TkWidget Success -> Port SP_Msg -> Success
runWidgetOnPort tkw wport =
  initSchedule (&>) success tkCConfig tkw wport (\_ _ _ -> success) [] (\_->success)


--- Runs a TkWidget on a GUI window that was previously created
--- by the operation "openWish".
--- This is useful to execute a GUI in parallel to other concurrent
--- processes.
--- In addition, an event handler is provided that process
--- messages received from an external message stream.
--- Thus, one can run a GUI that should react on
--- user events as well as messages sent to an external port.
--- @param th - a pair (tkw,exth) where tkw is the widget shown in the
---             new window and exth is the event handler for external messages
--- @param msgs - the stream of external messages (usually coming from
---               an external port)
--- @param wport - the wish port for the GUI
runControlledWidgetOnPort :: (TkWidget Success, msg -> Port SP_Msg -> Success)
                             -> [msg] -> Port SP_Msg -> Success
runControlledWidgetOnPort (tkw,exth) msgs wport =
  initSchedule (&>) success tkCConfig tkw wport (\msg [] wp -> exth msg wp)
               msgs (\_->success)


--- Runs a TkWidget on a GUI window that was previously created
--- by the operation "openWish".
--- This is useful to execute a GUI in parallel to other concurrent
--- processes.
--- The GUI is actually started after executing an initial command for the GUI.
--- @param tkw - the widget shown in the new GUI window
--- @param wport - the wish port for the GUI
--- @param initcmd - the initial command executed before starting the GUI
runWidgetOnPortInit :: TkWidget Success -> Port SP_Msg
                                 -> (Port SP_Msg -> Success) -> Success
runWidgetOnPortInit tkw wport initcmd =
  initSchedule (&>) success tkCConfig tkw wport (\_ _ _ -> success) [] initcmd



-- start the scheduler (see below) with a given TkWidget on a wish port
-- and an initial command:
initSchedule :: (a -> a -> a) -> a ->
                (TkRefType -> TkConfItem a -> Port SP_Msg -> a) ->
                TkWidget a -> Port SP_Msg ->
                (msg -> [(TkRefType,TkConfItem a)] -> Port SP_Msg -> a) ->
                [msg] -> (Port SP_Msg -> a) -> a
initSchedule seqhnd stop config widget wport exth msgs initcmd
   | send (SP_Put tcl) wport
   = seqhnd (initcmd wport)
            (tkSchedule seqhnd stop config evs wport exth msgs)
   where (tcl,evs) = tkmain2tcl wport widget

-- Scheduler for Tcl/Tk events:
-- (the event handlers are IO actions or constraints)
--
-- Meaning of arguments:
-- seqhnd: operator for sequential composition of handlers (">>" or "&>")
-- stop: final operation w.r.t. seqhnd ("done" or "success")
-- config: the operation for processing configurations (tkConfig or tkCConfig)
-- evs: list of (String,handler) pairs
-- wport: port to a wish
-- exth: handler for external messages
-- msgs: list of external messages
tkSchedule seqhnd stop config evs wport exth msgs =
  tkScheduleChoice seqhnd stop config evs wport exth msgs
                   (choiceSPEP wport msgs)

tkScheduleChoice seqhnd stop config evs wport exth msgs (Left ans)
 | null ans     = stop
 | ans==":EXIT" = tkTerminateWish stop wport
 | (take 4 ans)==":EVT"
  = seqhnd (tkSelectEvent stop (drop 4 ans) evs wport configs)
           (seqhnd (foldr seqhnd stop (map (\(r,ci)->config r ci wport) configs))
                   (tkSchedule seqhnd stop config (configEventHandlers evs configs)
                               wport exth msgs))
 | otherwise = seqhnd (if tkShowErrors
                       then trace ("ERROR in tkSchedule: Received: "++ans++"\n") stop
                       else stop)    -- ignore other outputs:
                     (tkSchedule seqhnd stop config evs wport exth msgs)
  where configs free
tkScheduleChoice seqhnd stop config evs wport exth _ (Right (msg:msgs)) =
   seqhnd (exth msg configs wport)
          (seqhnd (foldr seqhnd stop
                         (map (\(r,ci)->config r ci wport) configs))
                  (tkSchedule seqhnd stop config (configEventHandlers evs configs) wport exth msgs))
  where configs free

configEventHandlers evs [] = evs
configEventHandlers evs ((ref,confitem):confitems) =
 let label = tkRef2Label ref in
 case confitem of
   TkHandler handler -> configEventHandlers ((label,handler) :
                        (filter (\le->fst le /= label) evs)) confitems
   _ -> configEventHandlers evs confitems


--- Changes the current configuration of a widget
--- (deprecated operation, only included for backward compatibility).
--- Warning: does not work for TkCmd options!
tkConfig :: TkRefType -> TkConfItem _ -> Port SP_Msg -> IO ()
tkConfig (TkRefLabel wpv var wtype) confitem wport
  | checkWishConsistency wpv wport &>
    send (SP_Put (tkConf2tcl wtype wpv (tkRefname2Label var) confitem)) wport
  = done

--- Changes the current configuration of a widget
--- (deprecated operation, only included for backward compatibility).
--- Warning: does not work for TkCmd options!
tkCConfig :: TkRefType -> TkConfItem _ -> Port SP_Msg -> Success
tkCConfig (TkRefLabel wpv var wtype) confitem wport
  | checkWishConsistency wpv wport &>
    send (SP_Put (tkConf2tcl wtype wpv (tkRefname2Label var) confitem)) wport
  = success

-- terminate the wish and close the port:
tkTerminateWish stop wport
  | send (SP_Put "exit") wport &>
    send SP_Close wport
  = stop

tkSelectEvent stop ans [] _ _ =
  trace ("Internal error in Tk/Curry: no handler for event: "++ans++"\n") stop
tkSelectEvent stop ans ((ev,handler):evs) wport configs =
  if ans==ev then handler configs wport
             else tkSelectEvent stop ans evs wport configs


-- get the current value of a widget <w>" by
-- 1. executing the Tcl procedure "putvar [getvar_<w>]"
-- 2. reading the message ":VAR<w>%<len>*<value>
--    (where <len> is the length of <value> which can be more than one line)
tkGetVar :: String -> Port SP_Msg -> String -> Success
tkGetVar var wport val
 | send (SP_Put ("putvar "++var++" [getvar"++var++"]")) wport
 = tkGetVarMsg var wport val

tkGetVarMsg var wport val
 | send (SP_GetLine varmsg) wport
 = if takeWhile (/='%') varmsg == ":VAR"++var
   then let (len,value) = break (=='*') (tail (dropWhile (/='%') varmsg))
         in val =:= tkGetVarValue (readNat len) (tail value) wport
   else (if tkShowErrors
         then trace ("ERROR in tkGetVar \""++var++"\": Received: "
                      ++varmsg++"\n") success
         else success) &>
        tkGetVarMsg var wport val -- ignore other messages and try again
        --&> send (SP_Put ("puts \""++escape_tcl varmsg++"\"")) wport
 where varmsg free

tkGetVarValue len valmsg wport
 = if length valmsg < len
   then valmsg ++ "\n" ++ tkGetVarRemValue (len - (length valmsg + 1)) wport
   else if length valmsg > len && tkShowErrors
        then trace ("ERROR in tkGetVar: answer too short\n") valmsg
        else valmsg

tkGetVarRemValue len wport | send (SP_GetLine valmsg) wport
 = if length valmsg < len
   then valmsg ++ "\n" ++ tkGetVarRemValue (len - (length valmsg + 1)) wport
   else if length valmsg > len && tkShowErrors
        then trace ("ERROR in tkGetVar: answer too short\n") valmsg
        else valmsg
 where valmsg free


-- check consistency of access to Tk variables via Tk ports:
checkWishConsistency wp1 wp2 =
 if wp1==wp2 then success else
  trace "Inconsistent use of Tk ports during access to Tk variables\n" failed


-- escape some Tcl special characters (brackets, dollars):
escape_tcl [] = []
escape_tcl (c:s) = if c=='[' || c==']' || c=='$' || c=='"' || c=='\\'
                   then '\\':c:escape_tcl s
                   else c:escape_tcl s


----------------------------------------------------------------------------
-- some useful functions for implementing event handlers...
----------------------------------------------------------------------------
-- ...as IO actions:

--- An event handler that does nothing.
tkVoid :: Port SP_Msg -> IO ()
tkVoid _ = done

--- An event handler for terminating the GUI.
tkExit :: Port SP_Msg -> IO ()
tkExit wport
 | send (SP_Put "puts \":EXIT\"") wport &> -- send :EXIT to terminate scheduler
   send (SP_Put "exit") wport              -- send "exit" to terminate the wish
 = done

--- Gets the (String) value of a variable in a GUI.
tkGetValue :: TkRefType -> Port SP_Msg -> IO String
tkGetValue (TkRefLabel wpv var _) wport
  | checkWishConsistency wpv wport &>
    tkGetVar var wport val
  = return val
  where val free

--- Sets the (String) value of a variable in a GUI.
tkSetValue :: TkRefType -> String -> Port SP_Msg -> IO ()
tkSetValue (TkRefLabel wpv var _) val wport
  | checkWishConsistency wpv wport &>
    send (SP_Put ("setvar"++var++" \""++escape_tcl val++"\"")) wport
  = done

--- Updates the (String) value of a variable w.r.t. to an update function.
tkUpdate :: (String->String) -> TkRefType -> Port SP_Msg -> IO ()
tkUpdate upd tkref wport
  = tkGetValue tkref wport >>= \val ->
    tkSetValue tkref (upd val) wport

--- Appends a String value to the contents of a TextEdit widget and
--- adjust the view to the end of the TextEdit widget.
tkAppendValue :: TkRefType -> String -> Port SP_Msg -> IO ()
tkAppendValue (TkRefLabel wpv var wtype) val wport
  | if wtype/="textedit"
    then trace ("WARNING: tkAppendValue ignored for widget type \""++
                wtype++"\"\n") success
    else checkWishConsistency wpv wport &>
         send (SP_Put
                 (tkRefname2Label var++" insert end \""++escape_tcl val++"\""))
              wport &>
         send (SP_Put (tkRefname2Label var++" see end")) wport
  = done

--- This is an experimental function and might be changed in the future.
--- Appends a String value with tags to the contents of a TextEdit widget and
--- adjust the view to the end of the TextEdit widget.
tkAppendTaggedValue :: TkRefType -> String -> String -> Port SP_Msg -> IO ()
tkAppendTaggedValue (TkRefLabel wpv var wtype) val tags wport
  | if wtype/="textedit"
    then trace ("WARNING: tkAppendTaggedValue ignored for widget type \""++
                wtype++"\"\n") success
    else checkWishConsistency wpv wport &>
         send (SP_Put
                 (tkRefname2Label var++" insert end \""++escape_tcl val++"\""
                  ++" "++tags))
              wport &>
         send (SP_Put (tkRefname2Label var++" see end")) wport
  = done


--- Sets the input focus of this GUI to the widget referred by the first
--- argument.
--- This is useful for automatically selecting input entries in an application.
tkFocus :: TkRefType -> Port SP_Msg -> IO ()
tkFocus (TkRefLabel wpv var _) wport
  | checkWishConsistency wpv wport &>
    send (SP_Put ("focus "++tkRefname2Label var)) wport
  = done

--- Adds a list of canvas items to a canvas referred by the first argument.
tkAddCanvas :: TkRefType -> [TkCanvasItem] -> Port SP_Msg -> IO ()
tkAddCanvas (TkRefLabel wpv var wtype) items wport
  | checkWishConsistency wpv wport &>
    send (SP_Put (tkConf2tcl wtype wpv (tkRefname2Label var) (TkItems items)))
         wport
  = done


----------------------------------------------------------------------------
-- ...as constraints:

--- An event handler that does nothing.
tkCVoid :: Port SP_Msg -> Success
tkCVoid _ = success

--- An event handler for terminating the GUI.
tkCExit :: Port SP_Msg -> Success
tkCExit wport
 = send (SP_Put "puts \":EXIT\"") wport  -- send :EXIT to terminate scheduler

--- Gets the (String) value of a variable in a GUI.
tkCGetValue :: TkRefType -> Port SP_Msg -> String -> Success
tkCGetValue (TkRefLabel wpv var _) wport val
  | checkWishConsistency wpv wport &>
    tkGetVar var wport val
  = success

--- Sets the (String) value of a variable in a GUI.
tkCSetValue :: TkRefType -> String -> Port SP_Msg -> Success
tkCSetValue (TkRefLabel wpv var _) val wport
  | checkWishConsistency wpv wport
  = send (SP_Put ("setvar"++var++" \""++escape_tcl val++"\"")) wport

--- Updates the (String) value of a variable w.r.t. to an update function.
tkCUpdate :: (String->String) -> TkRefType -> Port SP_Msg -> Success
tkCUpdate upd tkref wp = 
  let val free in tkCGetValue tkref wp val &>
                  tkCSetValue tkref (upd val) wp

--- Sets the input focus of this GUI to the widget referred by the first
--- argument.
--- This is useful for automatically selecting input entries in an application.
tkCFocus :: TkRefType -> Port SP_Msg -> Success
tkCFocus (TkRefLabel wpv var _) wport
  | checkWishConsistency wpv wport &>
    send (SP_Put ("focus "++tkRefname2Label var)) wport
  = success

--- Adds a list of canvas items to a canvas referred by the first argument.
tkCAddCanvas :: TkRefType -> [TkCanvasItem] -> Port SP_Msg -> Success
tkCAddCanvas (TkRefLabel wpv var wtype) items wport
  | checkWishConsistency wpv wport &>
    send (SP_Put (tkConf2tcl wtype wpv (tkRefname2Label var) (TkItems items)))
         wport
  = success


----------------------------------------------------------------------------
-- Example GUIs:
----------------------------------------------------------------------------

--- A simple popup message.
popup_message :: String -> IO ()
popup_message s = runWidget ""
                    (TkCol [] [TkLabel [TkText s],
                               TkButton tkExit [TkText "Dismiss"]])

--- A simple event handler that can be associated to a widget.
--- The event handler takes a GUI port as parameter in order to
--- read or write values from/into the GUI.
TkCmd :: (Port SP_Msg -> a) -> TkConfItem a
TkCmd cmd = TkHandler configcmd
  where configcmd [] wport = cmd wport

--- An event handler that can be associated to a widget.
--- The event handler takes a GUI port as parameter (in order to
--- read or write values from/into the GUI) and returns a list
--- of widget reference/configuration pairs
--- which is applied after the handler in order to configure some GUI widgets.
TkConfigCmd :: (Port SP_Msg -> IO [(TkRefType,TkConfItem (IO ()))])
               -> TkConfItem (IO ())
TkConfigCmd cmd =
   TkHandler (\configs wp -> cmd wp >>= \confs -> doSolve (confs=:=configs))

--- A button with an associated event handler which is activated
--- if the button is pressed.
TkButton :: (Port SP_Msg -> a) -> [TkConfItem a] -> TkWidget a
TkButton cmd confs = TkPlainButton (TkCmd cmd : confs)


--- A button with an associated event handler which is activated
--- if the button is pressed. The event handler is a configuration handler
--- (see TkConfigCmd) that allows the configuration of some widgets.
TkConfigButton :: (Port SP_Msg -> IO [(TkRefType,TkConfItem (IO ()))])
                  -> [TkConfItem (IO ())] -> TkWidget (IO ())
TkConfigButton cmd confs = TkPlainButton (TkConfigCmd cmd : confs)


--- A text edit widget with vertical and horizontal scrollbars.
--- The argument contains the configuration options for the text edit widget.
TkTextEditScroll :: [TkConfItem a] -> TkWidget a
TkTextEditScroll confs =
   TkMatrix []
     [[TkTextEdit ([TkRef txtref, TkFill]++confs),
       TkScrollV txtref [TkFillY]],
      [TkScrollH txtref [TkFillX]]]     where txtref free


--- A canvas widget with vertical and horizontal scrollbars.
--- The argument contains the configuration options for the text edit widget.
TkCanvasScroll :: [TkConfItem a] -> TkWidget a
TkCanvasScroll confs =
   TkCol []
     [TkRow []
       [TkCanvas ([TkRef cref, TkFill]++confs),
        TkScrollV cref [TkFillY]],
      TkScrollH cref [TkFillX]]     where cref free


--- An entry widget with a horizontal scrollbar.
--- The argument contains the configuration options for the entry widget.
TkEntryScroll :: [TkConfItem a] -> TkWidget a
TkEntryScroll confs =
   TkCol []
    [TkEntry ([TkRef entryref, TkFillX]++confs),
     TkScrollH entryref [TkWidth 10, TkFillX]]
  where entryref free


--- Pops up a GUI for selecting an existing file.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
tkGetOpenFile :: IO String
tkGetOpenFile = tkGetOpenFileWithTypes []

--- Pops up a GUI for selecting an existing file. The parameter is
--- a list of pairs of file types that could be selected.
--- A file type pair consists of a name and an extension for that file type.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
tkGetOpenFileWithTypes :: [(String,String)] -> IO String
tkGetOpenFileWithTypes filetypes = openWish "" >>= tkGOF filetypes
tkGOF filetypes wp
  | send (SP_Put
             ("wm withdraw .\nputs [tk_getOpenFile" ++
              (if null filetypes then "" else
               " -filetypes {"++
               concatMap (\(x,y)->"{{"++x++"} {"++y++"}} ") filetypes ++"}") ++
              "]\n"))
           wp &>
    send (SP_GetLine filename) wp
  = tkExit wp >> return filename
 where filename free


--- Pops up a GUI for choosing a file to save some data.
--- If the user chooses an existing file, she/he will asked to confirm
--- to overwrite it.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
tkGetSaveFile :: IO String
tkGetSaveFile = tkGetSaveFileWithTypes []

--- Pops up a GUI for choosing a file to save some data. The parameter is
--- a list of pairs of file types that could be selected.
--- A file type pair consists of a name and an extension for that file type.
--- If the user chooses an existing file, she/he will asked to confirm
--- to overwrite it.
--- The file with its full path name will be returned (or "" if the user
--- cancels the selection).
tkGetSaveFileWithTypes :: [(String,String)] -> IO String
tkGetSaveFileWithTypes filetypes = openWish "" >>= tkGSF filetypes
tkGSF filetypes wp
  | send (SP_Put
             ("wm withdraw .\nputs [tk_getSaveFile" ++
              (if null filetypes then "" else
               " -filetypes {"++
               concatMap (\(x,y)->"{{"++x++"} {"++y++"}} ") filetypes ++"}") ++
              "]\n"))
           wp &>
    send (SP_GetLine filename) wp
  = tkExit wp >> return filename
 where filename free


--- Pops up a GUI dialog box to select a color.
--- The name of the color will be returned (or "" if the user
--- cancels the selection).
tkChooseColor :: IO String
tkChooseColor = openWish "" >>= tkCC
tkCC wp | send (SP_Put "wm withdraw .\nputs [tk_chooseColor]") wp &>
          send (SP_GetLine color) wp
         = tkExit wp >> return color
 where color free


-- end of Tk library
