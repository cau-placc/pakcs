------------------------------------------------------------------------------
--- Interface for ui descriptions
--- @author Christof Kluss
--- @version September 2008
------------------------------------------------------------------------------

module UI(
  Command(..),Ref(..),Handler(..),
  Widget(Widget),
  Event(..),WidgetKind(..),
  CanvasItem(..),
  UIEnv,
  -- for stils
  StyleClass(..),Position(..),Direction(..),
  Style(..),BorderStyle(..),FontStyle(..),Color(..),
  --
  addStyle,addStyles,setStyles,
  addHandler,addHandlers,setHandlers,
  setRef,getRef,
  -- IO Actions
  runUI,exitUI,
  getValue,setValue,updateValue,appendValue,
  changeStyles,setHandler,setDisabled,
  addCanvas,
  showPopup,showMessage,  
  -- Widgets
  colS,col,rowS,row,matrixS,matrix,
  entry,entryS,label,labelS,button,buttonS,simpleButton,simpleButtonS,
  checkButton,checkButtonS,simpleCheckButton,simpleCheckButtonS,
  canvas,canvasS,
  textEdit,textEditS,
  scale,scaleS,message,messageS,
  menuBar,menuBarS,menu,menuS,menuSeparator,menuSeparatorS,
  menuItem,menuItemS,
  listBox,listBoxS,
  selection,selectionInitial,selectionInitialS,radio_main,radio_other,
  --
  showBorderStyle,showColor,showPos
) where


import Maybe

infixl 0 `setRef`
infixl 0 `setHandlers`
infixl 0 `addStyles`
infixl 0 `setStyles`

data Command act1 act2 
  = Cmd act1   
  | SpicyDoc act2 

--- The data type of references to widgets in a UI window.
data Ref r = Ref r

data Handler act1 act2
  = Handler Event (Command act1 act2)

--- The generic type of a widget in an UI.
data Widget r act1 act2 =
  Widget
    (WidgetKind r act1 act2)
    (Maybe String) (Maybe (Ref r)) [Handler act1 act2]
    [StyleClass]
    [Widget r act1 act2]

--- The data type of possible events on which handlers can react.
data Event = 
    DefaultEvent
  | FocusOut
  | FocusIn
  | MouseButton1
  | MouseButton2
  | MouseButton3
  | KeyPress
  | Return
  | Change  
  | Click
  | DoubleClick


data WidgetKind r act1 act2 =
    Col
  | Row
  | Matrix [[Widget r act1 act2]]
  | Label
  | Button
  | Entry  
  | TextEdit Int Int
  | Scale Int Int
  | CheckButton Bool
  | Menu | MenuSeparator | MenuBar | MenuItem  
  | Canvas Int Int 
  | ListBox Int [String] Int | ListBoxItem String Bool
  | Name String  
  | Link  
  | RadioButton Bool


defaultHandler :: a -> Handler a _
defaultHandler cmd = Handler DefaultEvent (Cmd cmd)

-------------------------------------------------------------------------------

addStyle :: Widget r a1 a2 -> StyleClass -> Widget r a1 a2
addStyle widget class = addStyles widget [class]

addStyles :: Widget r a1 a2 -> [StyleClass] -> Widget r a1 a2
addStyles (Widget str mblabel mbref handlers styleClasses ws) classes 
  = Widget str mblabel mbref handlers (styleClasses ++ classes) ws 

setStyles :: Widget r a1 a2 -> [StyleClass] -> Widget r a1 a2
setStyles (Widget str mblabel mbref handlers _ ws) classes 
  = Widget str mblabel mbref handlers classes ws 

addHandler :: Widget r a1 a2 -> Handler a1 a2 -> Widget r a1 a2
addHandler widget handler = addHandlers widget [handler]

addHandlers :: Widget r a1 a2 -> [Handler a1 a2] -> Widget r a1 a2
addHandlers  (Widget str mblabel mbref handlers styles ws) hs 
  = Widget str mblabel mbref (handlers ++ hs) styles ws

setHandlers :: Widget r a1 a2 -> [Handler a1 a2] -> Widget r a1 a2
setHandlers (Widget str mblabel mbref _ styles ws) handlers
  = Widget str mblabel mbref handlers styles ws

setRef :: Widget r a1 a2 -> Ref r -> Widget r a1 a2
setRef  (Widget str mblabel _ handlers styles ws) ref 
  = Widget str mblabel (Just ref) handlers styles ws

getRef :: Widget r a1 a2 -> (Ref r,Widget r a1 a2)
getRef w@(Widget str mblabel mbref handlers styles ws) = case mbref of
  Just r  -> (r,w) 
  Nothing -> (ref,Widget str mblabel (Just ref) handlers styles ws) 
    where ref free

-------------------------------------------------------------------------------

--- The data type of items in a canvas.
data CanvasItem = 
    CLine [(Int,Int)] String
  | CPolygon [(Int,Int)] String
  | CRectangle (Int,Int) (Int,Int) String
  | COval (Int,Int) (Int,Int) String
  | CText (Int,Int) String String


-- Styles
-------------------------------------------------------------------------------

data StyleClass = Class [Style]
data Position = Center | Left | Right | Top | Bottom
data Direction = X | Y | Both

--- The data type of possible styles.
data Style = 
   Align Position 
 | TextAlign Position 
 | TextColor Color
 | Fill Direction
 | Height Int
 | Width Int
 | Active Bool
 | Fg Color
 | Bg Color
 | Font FontStyle 
 | Border BorderStyle
 | Display Bool
 | NameValue String String 
 

data BorderStyle = Dotted | Dashed | Solid
data FontStyle   = Bold | Italic | Underline

--- The data type of possible colors.
data Color 
  = Black | Blue | Brown | Cyan | Gold | Gray | Green
  | Magenta | Navy | Orange | Pink | Purple | Red
  | Tomato| Turquoise | Violet | White | Yellow | Default

-------------------------------------------------------------------------------

type UIRef = Ref ()
type UIWidget = Widget () (UI.UIEnv -> IO ()) () 
data UIEnv = UIEnv 

--- Run a Widget in a new window.
runUI :: String -> UIWidget -> IO ()
runUI _ _ = error "UI:runUI not executable"

--- An event handler for terminating the GUI.
exitUI :: UIEnv -> IO ()
exitUI _ = error "UI:exitUI not executable" 

--- Gets the String value of a variable in an UI.
getValue :: UIRef -> UIEnv -> IO String
getValue _ _ = error "UI:getValue not executable"

--- Sets the String value of a variable in an UI.
setValue :: UIRef -> String -> UIEnv -> IO ()
setValue _ _ _ = error "UI:setValue not executable"
 
--- Updates the (String) value of a variable 
--- w.r.t. to an update function.
updateValue :: (String -> String) -> UIRef -> UIEnv -> IO ()
updateValue _ _ _ = error "UI:updateValue not executable"

--- Appends a String value to the contents of a widget.
appendValue :: UIRef -> String -> UIEnv -> IO ()
appendValue _ _ _ = error "UI:appendValue not executable"

--- Changes the style of a widget
changeStyles :: UIRef -> [StyleClass] -> UIEnv -> IO ()
changeStyles _ _ _ = error "UI:changeStyles not executable" 

--- Sets a new Handler to a Widget referred by the first argument.
--- An existing Handler for the same event type is overridden
setHandler :: UIRef -> Event -> (UIEnv -> IO ()) -> UIEnv -> IO ()
setHandler _ _ _ _ = error "UI:setHandler not executable" 

--- Sets the state of a widget to disabled (inactive)
--- or active (inactive widgets do not accept any events)
setDisabled :: UIRef -> Bool -> UIEnv -> IO ()
setDisabled _ _ _ = error "UI:setDisabled not executable" 

--- Adds a list of canvas items to a canvas 
--- referred by the first argument.
addCanvas :: UIRef -> [CanvasItem] -> UIEnv -> IO ()
addCanvas _ _ _ = error "UI:addCanvas not executable" 

--- Runs a Widget in a new window.
showPopup :: String -> UIWidget -> UIEnv -> IO ()
showPopup _ _ _ = error "UI:showPopup not executable"
 
--- Shows a String Message in a new window.
showMessage :: String -> UIEnv -> IO ()
showMessage _ _  = error "UI:showMessage not executable" 

 
---------------------------------------------------------

--- Vertical alignment of widgets.
colS :: [StyleClass] -> [Widget r a1 a2] -> Widget r a1 a2
colS styles ws = Widget Col Nothing Nothing [] styles ws
col :: [Widget r a1 a2] -> Widget r a1 a2
col = colS []

--- Horizontal alignment of widgets.
rowS :: [StyleClass] -> [Widget r a1 a2] -> Widget r a1 a2
rowS styles ws = Widget Row Nothing Nothing [] styles ws
row :: [Widget r a1 a2] -> Widget r a1 a2
row = rowS []

--- A 2-dimensional (matrix) alignment of widgets
matrixS :: [StyleClass] -> [[Widget r a1 a2]] -> Widget r a1 a2
matrixS styles wss = Widget (Matrix wss) Nothing Nothing [] styles []
matrix :: [[Widget r a1 a2]] -> Widget r a1 a2
matrix = matrixS []

--- An entry widget for entering single lines
entry :: Ref r -> String -> Widget r a1 a2
entry = entryS []
entryS :: [StyleClass] -> Ref r -> String -> Widget r a1 a2
entryS styles ref content = 
  Widget Entry  (Just content) (Just ref) [] styles []

--- A label for showing a text
label :: String -> Widget r a1 a2
label = labelS []
labelS :: [StyleClass] -> String -> Widget r a1 a2
labelS styles str = Widget Label (Just str) Nothing [] styles []

-- A button in a UI whose event handler is activated
-- if the user presses the button
button :: a1 -> String -> Widget r a1 a2
button = buttonS []
buttonS :: [StyleClass] -> a1 -> String -> Widget r a1 a2
buttonS styles cmd text =
  Widget Button (Just text) Nothing [defaultHandler cmd] styles []

--- A button without handler, but reference
simpleButton :: Ref r -> String -> Widget r a1 a2
simpleButton = simpleButtonS []
simpleButtonS :: [StyleClass] -> Ref r -> String -> Widget r a1 a2
simpleButtonS styles ref text =  
  Widget Button (Just text) (Just ref) [] styles []

--- A check button: 
--- it has value "0" if it is unchecked and
---        value "1" if it is checked
checkButton :: Ref r -> a1 -> String -> Bool -> Widget r a1 a2
checkButton = checkButtonS []
checkButtonS :: [StyleClass] -> Ref r -> a1 -> String -> Bool ->
                                                            Widget r a1 a2
checkButtonS styles ref cmd text checked =
  Widget (CheckButton checked) 
         (Just text) (Just ref) [defaultHandler cmd] styles []


-- A check button without a reference: 
-- it has value "0" if it is unchecked and
--        value "1" if it is checked
simpleCheckButton :: Ref r -> String -> Bool -> Widget r a1 a2
simpleCheckButton = simpleCheckButtonS []
simpleCheckButtonS :: [StyleClass] -> Ref r -> String -> Bool ->
                                                           Widget r a1 a2
simpleCheckButtonS styles ref text checked =
  Widget (CheckButton checked) (Just text) (Just ref) [] styles []


-- A canvas to draw pictures containing CanvasItems
canvas :: Ref r -> Int -> Int -> Widget r a1 a2
canvas = canvasS []
canvasS :: [StyleClass] -> Ref r -> Int -> Int -> Widget r a1 a2
canvasS styles ref h w = Widget (Canvas h w) Nothing (Just ref) [] styles []


-- A text editor widget to show and manipulate larger text paragraphs
textEdit :: Ref r -> String -> Int -> Int -> Widget r a1 a2
textEdit = textEditS [] 
textEditS :: [StyleClass] -> Ref r -> String -> Int -> Int -> Widget r a1 a2
textEditS styles  ref text rows cols =
  Widget (TextEdit rows cols) (Just text) (Just ref) [] styles []
 

--- A scale widget to input values by a slider
scale :: Ref r -> a1 -> Int -> Int -> Widget r a1 a2
scale = scaleS []
scaleS :: [StyleClass] -> Ref r -> a1 -> Int -> Int -> Widget r a1 a2
scaleS styles  ref cmd min max =
  Widget (Scale min max) Nothing (Just ref) [defaultHandler cmd] styles []

--- A message for showing simple string values
message :: Ref r -> Widget r a1 a2
message = messageS []
messageS :: [StyleClass] -> Ref r -> Widget r a1 a2
messageS styles ref = Widget Label Nothing (Just ref) [] styles []

--- A menubar contains a list of menus
menuBar :: [Widget r a1 a2] -> Widget r a1 a2
menuBar = menuBarS []
menuBarS :: [StyleClass] -> [Widget r a1 a2] -> Widget r a1 a2
menuBarS styles xs    = Widget MenuBar Nothing Nothing [] styles xs

--- A button with a pull-down menu for a menubar
menu :: String -> [Widget r a1 a2] -> Widget r a1 a2
menu = menuS []
menuS :: [StyleClass] -> String -> [Widget r a1 a2] -> Widget r a1 a2
menuS styles text xs = Widget Menu (Just text) Nothing [] styles xs 

--- A separator between menu entries
menuSeparator :: Widget r a1 a2
menuSeparator = menuSeparatorS []
menuSeparatorS :: [StyleClass] -> Widget r a1 a2
menuSeparatorS styles = 
  Widget MenuSeparator (Just "--------------------") Nothing [] styles []

--- A button with an associated command and a label string
menuItem :: a1 -> String -> Widget r a1 a2
menuItem = menuItemS []
menuItemS :: [StyleClass] -> a1 -> String -> Widget r a1 a2
menuItemS styles cmd text = 
  Widget MenuItem 
         (Just text) Nothing [defaultHandler cmd] styles []

--- A widget containing a list of items for selection
listBox :: Int -> [String] -> Ref r -> a1 -> Widget r a1 a2
listBox = listBoxS []
listBoxS :: [StyleClass] -> Int -> [String] -> Ref r -> a1 -> Widget r a1 a2
listBoxS styles size strs ref cmd =  
  Widget (ListBox size strs (-1))
         Nothing (Just ref) [defaultHandler cmd] styles []

-------------------------------------------------------------------------------

--- A selection button with a reference and a list of name/value pairs.
--- The names are shown in the selection and the value is returned
--- for the selected name.
selection :: Ref r -> [String] -> Widget r a1 a2
selection ref menue = selectionInitial ref menue (-1)

--- A selection button with a reference, a list of name/value pairs,
--- and a preselected item in this list.
--- The names are shown in the selection and the value is returned
--- for the selected name.
selectionInitial :: Ref r -> [String] -> Int -> Widget r a1 a2
selectionInitial = selectionInitialS [] 
selectionInitialS :: [StyleClass] -> Ref r -> [String] -> Int ->
                                                            Widget r a1 a2
selectionInitialS styles ref items sel = 
  Widget (ListBox 1 items sel) Nothing (Just ref) [] styles []

--- A main button of a radio (initially "on") with a reference and a value.
--- The value is returned of this button is on.
--- A complete radio button suite always consists of a main button
--- (radio_main) and some further buttons (radio_others) with the
--- same reference. Initially, the main button is selected
--- (or nothing is selected if one uses radio_main_off instead of radio_main).
--- The user can select another button but always at most one button
--- of the radio can be selected. The value corresponding to the
--- selected button is returned in the environment for this radio reference.
radio_main :: Ref r -> String -> Widget r a1 a2
radio_main   ref value =
  Widget (RadioButton True) (Just value) (Just ref) [] [] []

--- A further button of a radio (initially "off") with a reference (identical
--- to the main button of this radio) and a value.
--- The value is returned of this button is on.
radio_other :: Ref r -> String -> Widget r a1 a2
radio_other  ref value =
  Widget (RadioButton False) (Just value) (Just ref) [] [] []
-------------------------------------------------------------------------------

showBorderStyle :: BorderStyle -> String
showBorderStyle Dotted = "dotted"
showBorderStyle Dashed = "dashed"
showBorderStyle Solid = "solid"

--- Converts a style value into its textual representation.
showColor :: Color -> String
showColor Black     = "black"
showColor Blue      = "blue"
showColor Brown     = "brown"
showColor Cyan      = "cyan"
showColor Gold      = "gold"
showColor Gray      = "gray"
--showColor Green     = "forest green"
showColor Green     = "green"
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
showColor Default   = "white"

showPos :: Position -> String
showPos Center = "center"
showPos Left   = "left"
showPos Right  = "right"
showPos Top    = "top"
showPos Bottom = "bottom"
