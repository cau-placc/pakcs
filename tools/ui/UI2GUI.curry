------------------------------------------------------------------------------
--- Library for creating tcl/tk applications from ui descriptions
--- based an the GUI library
--- @author Christof Kluss
--- @version July 2013
------------------------------------------------------------------------------

module UI2GUI (
  UIWidget, UIRef,
  Command(..),Ref,Handler(..),
  Widget(Widget),
  Event(..),WidgetKind(..),
  CanvasItem(..),
  -- for stils
  StyleClass(..),Position(..),Direction(..),
  Style(..),BorderStyle(..),FontStyle(..),Color(..),
  --
  addStyle,addStyles,setStyles,
  addHandler,addHandlers,setHandlers,
  setRef,getRef,
  -- IO Actions
  UI2GUI.runUI,UI2GUI.exitUI,
  UI2GUI.getValue,UI2GUI.setValue,UI2GUI.updateValue,UI2GUI.appendValue,
  UI2GUI.changeStyles,UI2GUI.setHandler,UI2GUI.setDisabled,
  UI2GUI.setVisible,
  UI2GUI.addCanvas,
  UI2GUI.showPopup,UI2GUI.showMessage,  
  -- Widgets
  colS,col,rowS,row,matrixS,matrix,
  entry,entryS,label,labelS,
  UI.button,buttonS,simpleButton,simpleButtonS,
  checkButton,checkButtonS,simpleCheckButton,simpleCheckButtonS,
  canvas,canvasS,
  textEdit,textEditS,
  scale,scaleS,message,messageS,
  menuBar,menuBarS,menu,menuS,menuSeparator,menuSeparatorS,
  menuItem,menuItemS,
  listBox,listBoxS,
  UI.selection,UI.selectionInitial,UI.selectionInitialS,
  UI.radio_main,UI.radio_other,
  --
  showBorderStyle,showColor,showPos,
  --  
  UI2GUI.UIEnv,--,seeText 
  setErrorBg
) where


import Maybe
import qualified GUI
import UI 
import IOExts

data State = State (GUI.GuiPort,[GUI.ReconfigureItem])
data UIEnv = UIEnv (IORef State)

type UIRef = UI.Ref GUI.WidgetRef 
type UIWidget = UI.Widget GUI.WidgetRef (UIEnv -> IO()) ()

ui2guievent :: Event -> GUI.Event
ui2guievent event = case event of
  MouseButton1 -> GUI.MouseButton1
  MouseButton2 -> GUI.MouseButton2
  MouseButton3 -> GUI.MouseButton3
  KeyPress     -> GUI.KeyPress
  Return       -> GUI.Return
  _            -> GUI.DefaultEvent


-- converts an UI event handler to an GUI event handler
-- the IORef value collects the GUI.ReconfigureItem
--
-- You can use "setHandler ref event cmd env" in ui handlers, 
-- but it is not possible to convert this with "GUI.setConfig ...",
-- due to implementation of the GUI library.
-- So such actions are collected in IORef value and 
-- are returned as list of ReconfigureItem values
ui2guicmd cmd gp = do stateref <- newIORef (State (gp,[])) 
	              cmd (UIEnv stateref) 
	              State (_,reconfigs) <- readIORef stateref
	              return reconfigs

-- converts text, reference and handlers of an ui widget
-- to corresponding values of type ConfItem
lrh2confitems :: Maybe String -> Maybe (UIRef) 
  -> [Handler (UIEnv -> IO ()) _] -> [GUI.ConfItem]
lrh2confitems mblabel mbref handlers =    
  text ++ wref ++ guihandlers  
  where
    text = map (\x -> GUI.Text x) (maybeToList mblabel)

    wref = case mbref of 
      Just ref -> [GUI.WRef (foo ref)]
        where 
	  foo r | r =:= UI.Ref cr = cr
            where cr free	
      Nothing  -> []

    guihandlers = foo handlers
      where 
        foo []     = []
        foo ((UI.Handler event (UI.Cmd cmd)):handlers1) =           
	  GUI.Handler (ui2guievent event) (ui2guicmd cmd): foo handlers1

-- converts an UI widget to a GUI widget 
widgetUI2GUI :: UIWidget -> GUI.Widget
widgetUI2GUI (UI.Widget name mblabel mbref handlers styleclasses widgets) =
  case name of
    UI.Col -> GUI.ColC collconfs confitems (map widgetUI2GUI widgets)
    UI.Row -> GUI.RowC collconfs confitems (map widgetUI2GUI widgets) 
    UI.Matrix wss -> GUI.Matrix collconfs (map (map widgetUI2GUI) wss) 
    UI.Label  -> GUI.Label confitems
    UI.Button -> GUI.PlainButton confitems
    UI.Entry  -> GUI.Entry confitems    
    -- Menu   
    UI.MenuBar    -> 
      GUI.MenuButton [GUI.Text label, GUI.Menu (map menu2menu widgets2)]
      where 
        (label,widgets2) = head (foo widgets)
          where             
            foo (UI.Widget name1 mblabel1 _ _ _ widgets1:_) = 
              case name1 of 
                UI.Menu -> [(fromJust mblabel1,widgets1)]  
		_       -> [("UI.Menu expected in UI2GUI.widgetUI2GUI",[])]
		
    UI.Canvas w h -> GUI.Canvas ([GUI.Width w, GUI.Height h] ++ confitems)
    UI.CheckButton checked -> GUI.CheckButton ((GUI.CheckInit init):confitems)           
      where init = if checked then "1" else "0"
    UI.ListBox h items sel -> 
      GUI.ListBoxScroll 
        ([GUI.Height h,GUI.Width 10,GUI.List items,GUI.CheckInit (show sel)] ++
        confitems)
    UI.Scale min max -> GUI.Scale min max confitems
    UI.TextEdit rows cols -> 
      GUI.TextEdit ([GUI.Height rows, GUI.Width cols] ++ confitems)
    x -> GUI.Label
            [GUI.Text (show x ++ " not implemented in UI2GUI.widgetUI2GUI")] 

  where
    collconfs = classes2guicollconfs styleclasses  

    confitems = lrh2confitems mblabel mbref handlers 
      ++ classes2guiconfitems styleclasses


menu2menu :: UIWidget -> GUI.MenuItem
menu2menu (UI.Widget name mblabel _ handlers _ widgets) =
  case name of    
    UI.Menu -> GUI.MMenuButton (fromJust mblabel) (map menu2menu widgets)
    UI.MenuSeparator -> GUI.MSeparator
    UI.MenuItem      -> GUI.MButton guihandler (fromJust mblabel)

      where 
        guihandler = head (foo handlers)
          where 
            foo [] = []
            foo ((UI.Handler _ (UI.Cmd cmd)):handlers1) = 
              (ui2guicmd cmd):(foo handlers1)

------------------------------------------------------------------------------

-- returns display informations from widgets
--
getdisplayconf :: UIWidget -> [(GUI.WidgetRef,GUI.ConfItem)]
getdisplayconf (UI.Widget name _ mbref _ styleclasses widgets) =
  case name of 
    UI.Matrix wss -> (concatMap (concatMap getdisplayconf) wss)
    _ -> (bar styleclasses) ++ (concatMap getdisplayconf widgets)    
  where 
    bar [] = []
    bar (UI.Class styles:cs) = (foo styles) ++ bar cs
     where   
      foo []             = []
      foo (style:styles') = case style of
        (UI.Display b) -> case mbref of 
          Just (UI.Ref ref) -> [(ref,GUI.Display b)] ++ foo styles'	            
	  _                 -> foo styles'
        _ -> foo styles'  
------------------------------------------------------------------------------

-- to hide/show a widget the in tcl/tk the commands 
-- "grid $widget" and "grid remove $widget" are used.
-- so the grid have to exists before 
-- 
runUI :: String -> UIWidget -> IO ()
runUI title widget =
  --GUI.runGUI title (widgetUI2GUI widget)
  GUI.runInitGUI title (widgetUI2GUI widget) initcmd
  where
    d = getdisplayconf widget
    initcmd gp = 
      mapIO_ (\ (ref,confitem) -> GUI.setConfig ref confitem gp) d

------------------------------------------------------------------------------

classes2guiconfitems :: [StyleClass] -> [GUI.ConfItem]
classes2guiconfitems [] = []
classes2guiconfitems (UI.Class styles:cs) =
  (styles2confitems styles) ++ classes2guiconfitems cs

  where 
    styles2confitems []             = []
    styles2confitems (style:styles') = case style of
      --Active Bool
      UI.Fg color -> [GUI.Foreground $ UI.showColor color]
      UI.TextColor color -> [GUI.Foreground $ UI.showColor color]
      UI.Bg color -> 
        case color of 
	  Default -> [GUI.TclOption "-background $defaultBgColor"]
	  _       -> [GUI.Background $ UI.showColor color]
      --UI.Height n -> [GUI.Height n]
      --UI.Width  n -> [GUI.Width n]
      UI.Display b -> [GUI.Display b]
      --Fill 
      --FillX 
      --FillY 
      _ -> []
     ++ styles2confitems styles'

classes2guicollconfs :: [StyleClass] -> [GUI.ConfCollection]
classes2guicollconfs []     = []
classes2guicollconfs (UI.Class styles:cs) =
  (styles2confs styles) ++ classes2guicollconfs cs

  where 
    styles2confs []             = []
    styles2confs (style:styles') =
      case style of
        UI.Align pos -> case pos of 
          UI.Center -> GUI.CenterAlign:styles2confs styles
          UI.Left   -> GUI.LeftAlign:styles2confs styles 
          UI.Right  -> GUI.RightAlign:styles2confs styles 
          UI.Top    -> GUI.TopAlign:styles2confs styles 
          UI.Bottom -> GUI.BottomAlign:styles2confs styles
        _                 -> styles2confs styles
     ++ styles2confs styles'

-------------------------------------------------------------------------------

canvasui2canvasgui :: [CanvasItem] -> [GUI.CanvasItem]
canvasui2canvasgui canvasitems = map conv canvasitems
  where
    conv (CLine points str)    = GUI.CLine points str 
    conv (CPolygon points str) = GUI.CPolygon points str
    conv (CRectangle x y str)  = GUI.CRectangle x y str
    conv (COval x y str)       = GUI.COval x y str
    conv (CText x str1 str2)   = GUI.CText x str1 str2
    
-------------------------------------------------------------------------------    

--- Sets the String value of a variable in an UI.
setValue :: UIRef -> String -> UIEnv -> IO ()
setValue (UI.Ref ref) val (UIEnv p) = do State (gp,_) <- readIORef p
                                         GUI.setValue ref val gp
					 
--- Gets the String value of a variable in an UI.
getValue :: UIRef -> UIEnv -> IO (String)
getValue (UI.Ref ref) (UIEnv p) = do State (gp,_) <- readIORef p
                                     GUI.getValue ref gp
				     
--- Updates the (String) value of a variable 
--- w.r.t. to an update function.
updateValue :: (String -> String) -> UIRef -> UIEnv -> IO ()
updateValue upd (UI.Ref ref) (UIEnv p) = do State (gp,_) <- readIORef p
                                            GUI.updateValue upd ref gp
--- Appends a String value to the contents of a widget.
appendValue :: UIRef -> String -> UIEnv -> IO ()
appendValue (UI.Ref ref) val (UIEnv p) = do State (gp,_) <- readIORef p
                                            GUI.appendValue ref val gp

--- Adds a list of canvas items to a canvas 
--- referred by the first argument.
addCanvas :: UIRef -> [CanvasItem] -> UIEnv -> IO ()
addCanvas (UI.Ref ref) items (UIEnv p) = do 
  State (gp,_) <- readIORef p
  GUI.addCanvas ref (canvasui2canvasgui items) gp

--- Runs a Widget in a new window.
showPopup :: String -> UIWidget -> _ -> IO ()
showPopup title ui _ = runUI title ui

--- Shows a String Message in a new window.
showMessage :: String -> UIEnv -> IO ()
showMessage message = showPopup "Info" (label message)

--- An event handler for terminating the GUI.
exitUI :: UIEnv -> IO ()
exitUI (UIEnv p) = do State (gp,_) <- readIORef p
                      GUI.exitGUI gp

--- Sets a new Handler to a Widget referred by the first argument.
--- An existing Handler for the same event type is overridden
setHandler :: UIRef -> Event -> (UIEnv -> IO _) -> UIEnv -> IO ()
setHandler (UI.Ref ref) event cmd (UIEnv p) = do
  State (gp,reconfigs) <- readIORef p
  writeIORef p 
    (State (gp,reconfigs ++ 
      [GUI.WidgetConf ref (GUI.Handler (ui2guievent event) (ui2guicmd cmd))]))
  
--- Sets the state of a widget to disabled (inactive)
--- or active (inactive widgets do not accept any events)
setDisabled :: UIRef -> Bool -> UIEnv -> IO ()
setDisabled (UI.Ref ref) value (UIEnv p) = do
  State (gp,_) <- readIORef p
  GUI.setConfig ref (GUI.Active (not value)) gp


setVisible :: UIRef -> Bool -> UIEnv -> IO ()
setVisible (UI.Ref ref) value (UIEnv p) = do
  State (gp,_) <- readIORef p
  GUI.setConfig ref (GUI.Display value) gp

--setVisible :: UIRef -> Bool -> UIEnv -> IO ()
--setVisible (UI.Ref ref) value (UIEnv p) = do
--  State (gp,reconfigs) <- readIORef p
--  writeIORef p (State (gp,reconfigs ++ [GUI.WidgetConf ref (GUI.Display value)]))
 

--- Changes the style of a widget
changeStyles :: UIRef -> [StyleClass] -> UIEnv -> IO ()
changeStyles (UI.Ref ref) styles (UIEnv env) = do
  State (gp,_) <- readIORef env
  mapIO_ (\ x -> GUI.setConfig ref x gp) (classes2guiconfitems styles)


setErrorBg ref b env = do  
  if b then changeStyles ref [Class [Bg Yellow]] env
       else changeStyles ref [Class [Bg Default]] env
