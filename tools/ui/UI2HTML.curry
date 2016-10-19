------------------------------------------------------------------------------
--- Library for creating web applications from ui descriptions
--- based on the HTML library from PAKCS
---
--- The installation of a cgi script written with this library
--- can be done by the command
--- 
--- <code>makecurrycgi -m main -o /home/user/public_html/prog.cgi prog</code>
---
--- where <code>prog</code> is the name of the Curry program with
--- the cgi script, <code>/home/user/public_html/prog.cgi</code> is
--- the desired location of the
--- compiled cgi script, and <code>main</code> is a Curry expression
--- like <code>runGUI "Titel" uiwidget</code> 
--- (makecurrycgi is a shell script stored in <i>pakcshome</i>/bin).
---
--- @author Christof Kluss
--- @version July 2013
------------------------------------------------------------------------------

module UI2HTML (
  UIWidget, UIRef,
  Command(..),Ref,Handler(..),
  Widget(Widget),
  Event(..),WidgetKind(..),
  CanvasItem(..),
  -- for styles
  StyleClass(..),Position(..),Direction(..),
  Style(..),BorderStyle(..),FontStyle(..),Color(..),
  --
  addStyle,addStyles,setStyles,
  addHandler,addHandlers,setHandlers,
  setRef,getRef,
  -- IO Actions
  UI2HTML.runUI,UI2HTML.exitUI,
  UI2HTML.getValue,UI2HTML.setValue,UI2HTML.updateValue,UI2HTML.appendValue,
  UI2HTML.changeStyles,UI2HTML.setHandler,UI2HTML.setDisabled,
  UI2HTML.addCanvas,UI2HTML.setVisible,
  UI2HTML.showPopup,UI2HTML.showMessage,  
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
  UI2HTML.UIEnv,
  Reconfigure(..), -- exported for GUI2HTML
  ui2hexps,  
  ajaxForm,ajaxForm2,
  setConfig,seeText,
  --  
  setErrorBg,
  cgiRef2Ref,ref2cgiRef,  
  nextHtmlForm,
  includeHtmlForm,
  onClickSpicy, 
  changeChilds
) where

import HTML
import IOExts
import Maybe
import Json
import System
import UI
import SpicyWeb


maindir = "http://www.informatik.uni-kiel.de/~pakcs/UI/"
jsbibsspicy = ["prototype.js","Action.js"]
jsbibs      = ["ajaxrequest.js"]
css         = ["default.css"]


data State = State (CgiRef -> String,
                   ([(Ref CgiRef, Maybe String,[Reconfigure])],
                    [([(String,String)],[HtmlExp])]))

--- The (abstract) data type for representing environments
data UIEnv = UIEnv (IORef State)

type UIWidget = UI.Widget CgiRef (UIEnv -> IO()) (Doc (Ref CgiRef)) 
type UIRef    = Ref CgiRef

-- Data type for describing configurations that are applied
data Reconfigure 
  = List [String]
  | Disabled Bool 
  | Visible Bool
  | Style [StyleClass]
  | Pos (Int,Int)
  | ErrorBg Bool
 deriving Show

conf2str :: Reconfigure -> (String,Json)
conf2str val = case val of
  --Value str    -> ("text", String str)
  Disabled b   -> ("disabled",Bool b)
  Visible b    -> ("visible",Bool b)
  Style styles -> ("style",String (styleClasses2String styles))
  List strs    -> ("list",Array (map String strs))
  Pos (x,y)    -> ("pos",Array [Int x,Int y])
  ErrorBg b    -> ("errorbg",Bool b)
  x            -> ("error",String (show x))

state2json :: ([(Ref CgiRef,Maybe (String),[Reconfigure])],_) -> Json
state2json (xs,_) = Array (map toJson xs) 
  where
    toJson (ref,mbval,confs) =
      Object [("id",String (idOfRef  ref)),
              ("changes", Object ((map conf2str confs) ++ 
                (maybe [] 
                       (\val -> [("value",String val)]) 
                       mbval)))]

execCmdAndRespond :: (UIEnv -> IO()) -> IORef State -> IO HtmlForm
execCmdAndRespond cmd stateref = do
  cmd (UIEnv stateref)
  State (_,state) <- readIORef stateref
  let ps = snd state
  writeIORef stateref (State (const "",([],[]))) 
  return $ AjaxAnswer (state2json state) ps

-------------------------------------------------------------------------------

ref2cgiRef :: UIRef -> CgiRef
ref2cgiRef (Ref ref) = ref
cgiRef2Ref :: CgiRef -> Ref CgiRef
cgiRef2Ref ref = Ref ref

-- little modifications on the UI widgets for HTML output
widgetUI2widgetHTML :: UIWidget -> UIWidget
widgetUI2widgetHTML (UI.Widget name mbcont mbref handlers styles ws) =
  case name of
    Matrix wss -> Widget (Matrix (map (map widgetUI2widgetHTML) wss))
                       mbcont mbref handlers styles []

    MenuBar -> ((UI.Widget name mbcont mbref handlers styles 
               (map widgetUI2widgetHTML entrys)) 
                  `addStyles` [Class [Width 8]])
          where
            entrys = if null ws then ws else
                let (UI.Widget name' mbcontent' _ _ _ ws') = head ws in
                  case name' of 
                    Menu -> (UI.Widget (Name "option") mbcontent' Nothing [] 
                      [Class [Font Bold,Font Italic]] []):ws' ++ tail ws
                    _    -> ws

    Menu -> UI.Widget name mbcont mbref handlers styles
               (map widgetUI2widgetHTML ws)

    Scale min max ->
      UI.Widget (Name "select") mbcont mbref handlers styles
        (map (\x -> UI.Widget (ListBoxItem (show x) False)
                              (Just (show x)) Nothing [] [] [])
             [min .. max])

    CheckButton checked ->
      row [UI.Widget (CheckButton checked) mbcont mbref handlers styles [],
           label (fromMaybe "" mbcont)]

    ListBox size list sel -> 
      UI.Widget (ListBox size list sel) mbcont mbref handlers styles
        (selOption list 0)
      where
        selOption [] _ = []
        selOption (v:vs) i =
          (UI.Widget (ListBoxItem (show i) (if i==sel then True else False))
             (Just v) Nothing [] [] []) : selOption vs (i+1)  

    _ -> UI.Widget name mbcont mbref handlers styles 
           (map widgetUI2widgetHTML ws)

-- translates the kind of a widget into corresponding html tag
showWidgetKind :: WidgetKind _ _ _ -> String
showWidgetKind kind = case kind of 
  Matrix _ -> "span"
  Col      -> "div" 
  Row      -> "div" 
  Label    -> "div" -- span is no block element
  Button   -> "input" 
  Entry    -> "input" 
  Menu     -> "optgroup" 
  MenuSeparator -> "option"  
  MenuBar  -> "select" 
  MenuItem -> "option" 
  (Canvas _ _)  -> "canvas" 
  (TextEdit _ _)  -> "textarea" 
  (CheckButton _) -> "input" 
  (Scale _ _)     -> "scale"
  (ListBox _ _ _) -> "select" 
  ListBoxItem _ _ -> "option"   
  (Name str) -> str  
  --(Selection _ _)   -> "select"
  --(SelectionItem _ _) -> "option" 
  (RadioButton _ ) -> "input" 
  Link -> "a"
  _ -> "error in UI2HTML.showWidgetKind"


event2js :: Event -> String
event2js event = case event of 
  FocusOut     -> "onblur"
  FocusIn      -> "onfocus"
  MouseButton1 -> "onclick"
  MouseButton2 -> "onclick"
  MouseButton3 -> "onclick"
  KeyPress     -> "onkeyup" 
  Return       -> "onkeypress"
  Change       -> "onchange"
  DoubleClick  -> "ondbclick"
  Click        -> "onclick"
  DefaultEvent -> "default"
  _ -> "error in UI2HTML.event2js"


event2str :: WidgetKind _ _ _ -> Event -> String
event2str wkind event = case event of
  DefaultEvent -> case wkind of 
    Scale _ _     -> "onchange"
    Entry         -> "onkeypress" -- Return
    Name "select" -> "onchange"
    _             -> "onclick"
  _ -> event2js event


event2jsfctname :: WidgetKind _ _ _ -> Event -> String
event2jsfctname kind event = case event of
  Return   -> "handleKeypress"
  KeyPress -> "ajaxRequest" 
  DefaultEvent -> 
    case kind of 
      Entry -> "handleKeypress"
      _     -> "ajaxRequest" 
  _      -> "ajaxRequest" 

name2attrs :: WidgetKind _ _ _ -> [(String,String)]
name2attrs name = case name of
  TextEdit rows cols -> [("rows",show rows),("cols",show cols)] 
  Canvas h w -> [("height",show h),("width",show w)]
  Button     -> [("type","button")] 
  Entry      -> [("type","text")]   
  ListBox size _ _ -> [("size",show size)] 
  --SelectionItem _ b -> if b then [("selected","yes")] else []
  ListBoxItem _ b     -> if b then [("selected","yes")] else []
  CheckButton checked -> [("type","checkbox")] 
    ++ if checked then [("checked","checked")] else [] 
  RadioButton checked -> [("type","radio")] ++ 
                         (if checked then [("checked","yes")] else [])
  MenuBar    ->
     [("onchange",
        "ajaxRequest(event,window,this.options[this.selectedIndex].value); " 
         ++ "this.selectedIndex=0;")] 
  _ -> []


content2attrs :: Maybe (String) -> WidgetKind _ _ _ -> [(String,String)]
content2attrs mbcontent name = case mbcontent of
  Nothing     -> []
  Just label  -> case name of     
    Menu           -> [("label",htmlQuote label)]    
    Label          -> []
    TextEdit _ _   -> []
    MenuItem       -> []
    MenuSeparator  -> []
    Link           -> [("href","#")]
    ListBoxItem val _ -> [("value",htmlQuote val)]
    _              -> [("value",htmlQuote label)]

-- converts a Widget to a HtmlExp
widget2hexp :: Maybe (IORef State) -> Widget CgiRef (UIEnv -> IO ()) _ ->
                                                                     [HtmlExp]
widget2hexp mbstateref (UI.Widget kind mbcontent mbref handlers styles widgets) =
  handlerhexps ++ [case mbref of
    Nothing  -> hexp  
    Just ref -> case kind of 
      RadioButton False -> hexp -- only the main radio button has a reference
      _                 -> HtmlCRef hexp (ref2cgiRef ref)]      
  where

    hexp1 = 
      HtmlStruct 
        (showWidgetKind kind)                -- tag name
        -- set the tag attributes 
        ((name2attrs kind)                   -- type attribute
         ++ (content2attrs mbcontent kind)   -- value attribute
         ++ case mbref of                    -- name and id attribute
              Nothing -> []
              Just ref1 -> case kind of 
                -- span has no name attribute
                Label         -> [("id",rid)]
                Link          -> [("id",rid)]
                Row           -> [("id",rid)]
                Col           -> [("id",rid)]           
                -- id attr is unique, 
                -- so groups of radio buttons cannot have same id
                RadioButton _ -> [("name",rid)]
                _             -> [("id",rid),("name",rid)]
               where rid = idOfRef ref1
         ++ if null styles                   -- style attribute
              then [] else styleClasses2Attrs styles)
        childhexps                           -- child html expressions

    xs = case kind of 
      --Col -> map (\ w -> block (widget2hexp mbstateref w)) widgets
      Col -> [table' (map (\ w -> [widget2hexp mbstateref w]) widgets)]
      Row -> [table' [map (\ w -> widget2hexp mbstateref w) widgets]] 
      Matrix wss ->       
         [table' (map (\ ws -> map (\w -> widget2hexp mbstateref w) ws) wss) 
            `addAttrs` (if null styles then [] else styleClasses2Attrs styles)]
      _   -> concatMap (widget2hexp mbstateref) widgets

    -- some special handling for widgets without child widgets
    -- the content of these widgets are in their textnodes
    -- e.g. a widget of kind TextEdit: <textarea>content</textarea>
    childhexps = case kind of 
      Canvas _ _    -> [htxt "canvas tag not supported"] 
      Label         -> [htxtcont]  -- <span>content<span>     
      TextEdit _ _  -> [htxtcont]  -- <textarea>content<textareas>
      Name "option" -> [htxtcont] 
      Menu          -> xs
      MenuItem      -> [htxtcont]
      MenuSeparator -> [htxtcont]
      Link          -> [htxtcont] 
      ListBoxItem _ _ -> [htxtcont]
      --SelectionItem name _ -> [htxt $ htmlQuote name ++ " "]      
      _         -> xs
     where htxtcont = htxt $ fromMaybe "" mbcontent 
     
    handlerhexps = []
    hexp = handler2hexp handlers hexp1
      where 
        handler2hexp [] ahexp = ahexp
        handler2hexp (h:hs) ahexp = case h of 
          Handler event cmd -> case cmd of
            Cmd c ->
              handler2hexp hs 
               (case kind of
                 MenuItem -> (AjaxEvent2 ahexp handler "value" "")
                 _   -> AjaxEvent2 ahexp handler (event2str kind event) 
                                   (event2jsfctname kind event))

               where
                 handler env =
                   case mbstateref of 
                     Just stateref -> do                
                       State (_,state) <- readIORef stateref
                       writeIORef stateref (State (env,state))
                       execCmdAndRespond c stateref
                     Nothing -> do 
                      stateref2 <- newIORef (State (env,([],[])))
                      execCmdAndRespond c stateref2   
 
            SpicyDoc _ -> handler2hexp hs ahexp


-- there are problems with instantiation when you use this code 
{-
    (hexp,handlerhexps) = handler2hexp handlers hexp1 []
      where 
        handler2hexp [] nhexp nhandlerhexp = (nhexp,nhandlerhexp)
        handler2hexp (h:hs) nhexp nhandlerhexp = case h of 
          Handler event cmd -> case cmd of
            Cmd c -> case kind of
                 MenuItem -> handler2hexp hs 
                     (nhexp `addAttr` ("value","EVENT_" ++ string2urlencoded id))            
                     (AjaxEvent id handler:nhandlerhexp)
                     
                 _ -> handler2hexp hs 
                        (nhexp `addAttr` 
                          (event2str kind event,
                           event2jsfctname kind event ++ 
                           "(event,window,'EVENT_" ++ string2urlencoded id ++ "');"))
                        (AjaxEvent id handler:nhandlerhexp)             
               where
                 id free
                 handler env =
                   case mbstateref of 
                     Just stateref -> do                
                       State (_,state) <- readIORef stateref
                       writeIORef stateref (State (env,state))
                       execCmdAndRespond c stateref
                     Nothing -> do 
                      stateref2 <- newIORef (State (env,([],[])))
                      execCmdAndRespond c stateref2   
 
            Cmd2 _ -> handler2hexp hs nhexp nhandlerhexp
-}

------------------------------------------------------------------------------
------------------------------------------------------------------------------
ui2htmlexps :: Maybe (IORef State) -> UIWidget -> [HtmlExp]
ui2htmlexps mbstateref widget =   
  widget2hexp mbstateref (widgetUI2widgetHTML widget)    

ui2hexps :: UIWidget -> [HtmlExp]
ui2hexps widget =
  widget2hexp Nothing (widgetUI2widgetHTML widget)   

jsFormParams = map (\s -> FormJScript (maindir ++ s)) (jsbibs ++ jsbibsspicy)
  ++ map (\s -> FormCSS (maindir ++ s)) css 

ajaxForm :: String -> [HtmlExp] -> HtmlForm
ajaxForm title hexps = 
  HtmlForm title ([MultipleHandlers] ++ jsFormParams) hexps
  
ajaxForm2 :: String -> [FormParam] -> [HtmlExp] -> HtmlForm
ajaxForm2 title params hexps = 
  HtmlForm title (params ++ [MultipleHandlers] ++ jsFormParams) hexps  

runAjaxForm :: String -> [HtmlExp] -> IO HtmlForm
runAjaxForm title hexps = do    
    return $ ajaxForm title hexps 
------------------------------------------------------------------------------


runUI :: String -> UIWidget -> IO HtmlForm
runUI title widget = do
   stateref <- newIORef (State (const "",([],[])))
   let hexps = ui2htmlexps (Just stateref) widget 
   --let state = []

   let docs =  widget2docs widget    

   let acts = map (\ (OnLoad _ a) -> a) docs
   let optAct = elimNullActions $ foldr (*>) (yield Null) acts

   return $ HtmlForm title 
    ([MultipleHandlers] ++ jsFormParams ++ 
     [BodyAttr ("onload",showOnLoadAction optAct)])
    hexps
-------------------------------------------------------------------------------

styleClasses2String :: [StyleClass] -> String
styleClasses2String [] = [] 
styleClasses2String (Class styles:classes) = 
  styles2css styles ++ styleClasses2String classes 

-- http://www.w3.org/TR/CSS21

styles2css :: [Style] -> String
styles2css [] = ""
styles2css (style:styles) = case style of
    --Active b -> ("disabled","disabled")
    Display b -> if b then "display: inline" else "display: none"    
    Bg color  -> "background-color: " ++ showColor color 
    Fg color  -> "color: " ++ showColor color  
    Border s  -> "border-style: " ++ showBorderStyle s 
    Font s    -> case s of 
      Bold      -> "font-weight: bold"
      Italic    -> "font-style: italic"
      Underline -> "text-decoration: underline"
      _         -> ""
    TextAlign pos   -> "text-align: " ++ showPos pos
    TextColor color -> "color: " ++ showColor color
    Height n -> "height: " ++ show n ++ "em" --"px"
    Width n -> "width: " ++ show n ++ "em" --"px"   
    Align pos -> 
      case pos of
        Center   -> "text-align: " ++ showPos pos 
        UI.Left  -> "text-align: " ++ showPos pos 
        UI.Right -> "text-align: " ++ showPos pos 
        Top      -> "vertical-align: " ++ showPos pos 
        Bottom   -> "vertical-align: " ++ showPos pos 
        _        -> ""
 
    Fill dir -> 
      case dir of 
        X    -> "width: 100%"
        Y    -> "height: 100%"
        Both -> "width: 100%; height: 100%"     
        _      -> ""
    NameValue name value -> (name ++ ": " ++ value)    
    _         -> ""

  ++ "; " ++ styles2css styles 

------------------------------------------------------------------------------

styleClasses2Attrs :: [StyleClass] -> [(String,String)]
styleClasses2Attrs xs = 
  let str = styleClasses2String xs in 
    if null str 
      then []
      else [("style",styleClasses2String xs)]
   ++ s2attrs xs

  where s2attrs [] = [] 
        s2attrs (Class styles:classes) = 
           styles2attr styles ++ s2attrs classes

styles2attr :: [Style] -> [(String,String)]
styles2attr [] = []
styles2attr (style:styles) = case style of             
    Active b -> if not b then [("disabled","disabled")] else []
    _        -> []
  ++ styles2attr styles 


------------------------------------------------------------------------------


idOfRef :: Ref CgiRef -> String
idOfRef (Ref (CgiRef r)) =  r

--- Gets the (String) value of a variable in a GUI.
getValue :: Ref CgiRef -> UIEnv -> IO String
getValue cref (UIEnv stateref) = do
  State (env,state) <- readIORef stateref
  let value = findValue cref state
  case value of 
    Just v  -> return v
    --Nothing -> return $ fromMaybe "" (env (ref2cgiRef cref))    
    Nothing -> return $ env (ref2cgiRef cref)

--- Sets the (String) value of a variable in a GUI.
setValue :: UIRef -> String -> UIEnv -> IO ()
setValue cref value (UIEnv stateref) = do
  State (env,state) <- readIORef stateref
  let newstate = changeValue state cref value
  writeIORef stateref (State (env,newstate)) 

--- Changes the current configuration of a widget
setConfig :: UIRef -> Reconfigure -> UIEnv -> IO ()
setConfig cref conf (UIEnv stateref) = do
  State (env,state) <- readIORef stateref
  let newstate = changeConfig state cref conf
  writeIORef stateref (State (env,newstate))

--- Updates the (String) value of a variable w.r.t. to an update function.
updateValue :: (String -> String) -> UIRef -> UIEnv -> IO ()
updateValue upd cref x = do
  val <- getValue cref x
  setValue cref (upd val) x  

--- Appends a String value to the contents of a widget.
appendValue :: UIRef -> String -> UIEnv -> IO ()
appendValue cref str x =
  updateValue (\s -> s ++ str) cref x

--- Runs a Widget in a new window.
showPopup :: String -> UIWidget -> UIEnv -> IO ()
showPopup title ui (UIEnv stateref) = do
  State (env,(xs,ps)) <- readIORef stateref
  let newstate = (xs,ps ++ [([("type","popup"),("title",title)],
                             ui2htmlexps (Just stateref) ui)])
  writeIORef stateref  (State (env,newstate))

--- Shows a String Message in a new window.
showMessage :: String -> UIEnv -> IO ()
showMessage message = showPopup "Info" (label message) 

includeHtmlForm :: UIRef -> HtmlForm -> UIEnv -> IO ()
includeHtmlForm ref (HtmlForm _ _ hexps) (UIEnv stateref) = do
  State (env,(xs,ps)) <- readIORef stateref
  let newstate = (xs,ps ++ [([("type","changeChilds"),("ref",idOfRef ref)],
                             hexps)])
  writeIORef stateref (State (env,newstate))


changeChilds :: UIRef -> UIWidget -> UIEnv -> IO ()
changeChilds ref childs (UIEnv stateref) = do
  State (env,(xs,ps)) <- readIORef stateref
  let newstate = (xs,ps ++ [([("type","changeChilds"),("ref",idOfRef ref)],
                             ui2htmlexps (Just stateref) childs)])
  writeIORef stateref (State (env,newstate)) 

nextHtmlForm :: HtmlForm -> UIEnv -> IO ()
nextHtmlForm (HtmlForm title _ hexps) (UIEnv stateref) = do  
  State (env,(xs,ps)) <- readIORef stateref
  let newstate = (xs,ps ++ [([("type","htmlsite"),("title",title)],hexps)])
  writeIORef stateref (State (env,newstate))  


--- Sets a new Handler to a Widget referred by the first argument.
--- An existing Handler for the same event type is overridden
setHandler :: UIRef -> Event -> (UIEnv -> IO ()) -> UIEnv -> IO ()
setHandler cref event cmd (UIEnv stateref) = do
     State (env,(xs,ps)) <- readIORef stateref

     let newstate = (xs,ps ++ 
             [([("type","changeevent"), ("id",idOfRef cref),
                ("event",event2js event), 
                ("eventid","EVENT_" ++ string2urlencoded id)],
                [nhexp])])  
          where
            id free
            nhexp = AjaxEvent id handler 
              where 
                handler env1 = do
                  State (_,state) <- readIORef stateref
                  writeIORef stateref (State (env1,state))      
                  execCmdAndRespond cmd stateref

     writeIORef stateref (State (env,newstate))  

--- An event handler for terminating the GUI.
exitUI :: UIEnv -> IO ()
exitUI (UIEnv stateref) = do
  State (env,state) <- readIORef stateref
  let newstate = changeValue state (Ref (CgiRef "close")) ""
  writeIORef stateref (State (env,newstate)) 

------------------------------------------------------

--- Adds a list of canvas items to a canvas 
--- referred by the first argument.
addCanvas :: UIRef -> [CanvasItem] -> UIEnv -> IO ()
addCanvas _ [] _ = done
addCanvas cref (item:items) x = do
  case item of
    CLine line _ -> do 
      appendValue cref (";CLine" ++ show line) x
    _ -> done
  addCanvas cref items x

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--- Sets the state of a widget to disabled (inactive)
--- or active (inactive widgets do not accept any events)
setDisabled :: UIRef -> Bool -> UIEnv -> IO ()
setDisabled cref value = setConfig cref (Disabled value)

setVisible :: UIRef -> Bool -> UIEnv -> IO ()
setVisible cref value = setConfig cref (Visible value)

--- Changes the style of a widget
changeStyles :: UIRef -> [StyleClass] -> UIEnv -> IO ()
changeStyles cref styles = setConfig cref (Style styles) 

setErrorBg :: UIRef -> Bool -> UIEnv -> IO ()
setErrorBg cref isError = setConfig cref (ErrorBg isError)

seeText :: UIRef -> (Int,Int) -> UIEnv -> IO ()
seeText cref (line,column) env = do 
  setConfig cref (Pos (line,column)) env
  showMessage "UI2HTML:seeText not implemented" env  

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

findValue :: Eq a => a -> ([(a,Maybe b,_)],_) -> Maybe b
findValue _ ([],_) = Nothing
findValue ref ((r,mbval,_):xs,ps) =
  if (ref == r) 
    then mbval
    else findValue ref (xs,ps)

changeValue :: Eq a => ([(a,Maybe b,[c])],d) -> a -> b -> ([(a,Maybe b,[c])],d)
changeValue (xs,ps) ref nvalue = (change xs,ps)
  where 
    change [] = [(ref,Just nvalue,[])]
    change ((r,mbval,confs):xs1) = 
      if (ref == r) 
        then ((r,Just nvalue,confs):xs1)
        else ((r,mbval,confs):change xs1)

changeConfig :: Eq a => ([(a,Maybe b,[Reconfigure])],c) -> a -> Reconfigure
                     -> ([(a,Maybe b,[Reconfigure])],c)
changeConfig (xs,ps) ref nconf = (change xs,ps)
  where
    change [] = [(ref,Nothing,[nconf])]
    change ((r,mbval,confs):xs1) =       
      if (ref == r)
        then ((r,mbval,change' confs):xs1)
        else ((r,mbval,confs):change xs1) 
      where 
        change' [] = [nconf] 
        change' (c:oconfs) =
          case nconf of 
            List _ -> case c of
              List _ -> (nconf:oconfs)
              _ -> (c:change' oconfs)
            Disabled _ -> case c of 
              Disabled _ -> (nconf:oconfs)
              _ -> (c:change' oconfs)
            Visible _ -> case c of 
              Visible _ -> (nconf:oconfs)
              _ -> (c:change' oconfs)
            Pos _ -> case c of 
              Pos _ -> (nconf:oconfs)
              _ -> (c:change' oconfs)   
            Style n -> case c of 
              Style o -> (Style (o++n):oconfs)
              _ -> (c:change' oconfs)
            ErrorBg _ -> case c of 
              ErrorBg _ -> (nconf:oconfs)
              _ -> (c:change' oconfs)  

------------------------------------------------------------------------------

--- Table with a matrix of items where each item is a list of HTML expressions.
table' :: [[[HtmlExp]]] -> HtmlExp
table' items = HtmlStruct "table" [] --[("style","width: 100%;")]
 (map (\row->HtmlStruct "tr" []
         (map (\item -> 
           HtmlStruct "td" [("style","vertical-align: top;")] item) row)) items)

------------------------------------------------------------------------------
-- for spicy web

infixl 0 `onClickSpicy`

-- filter spicy docs from handlers
widget2docs :: Widget _ _ a2 -> [a2]
widget2docs (UI.Widget _ _ _ handlers _ widgets) =
  (handlers2docs handlers []) ++ (concatMap widget2docs widgets)
    where
      handlers2docs [] ds = ds
      handlers2docs (h:hs) ds = case h of
        Handler _ (SpicyDoc doc) -> handlers2docs hs (doc:ds)
        _                        -> handlers2docs hs ds


-- adds a spicyweb onclick action to a widget
onClickSpicy :: UIWidget -> Action _ UIRef -> UIWidget
onClickSpicy widget action = 
  let (ref,widget1) = getRef widget
  in  widget1 `addHandlers` [Handler Click (SpicyDoc (Doc ref `onClick` action))] 
