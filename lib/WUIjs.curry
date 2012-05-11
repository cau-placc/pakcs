------------------------------------------------------------------------------
--- A library to support the type-oriented construction of Web User Interfaces
--- (WUIs).
---
--- The ideas behind the application and implementation of WUIs are
--- described in a paper that is available via
--- [this web page](http://www.informatik.uni-kiel.de/~pakcs/WUI).
---
--- In addition to the original library, this version provides also support
--- for JavaScript.
---
--- @author Michael Hanus
--- @version February, 2009
------------------------------------------------------------------------------

module WUIjs(--WuiState,cgiRef2state,state2cgiRef,value2state,state2value,
           --states2state,state2states,altstate2state,state2altstate,
           Rendering,WuiSpec,
           withRendering,withError,withCondition,withConditionJS,
           adaptWSpec,transformWSpec,
           wHidden,wConstant,wInt,
           wString,wStringSize,wRequiredString,wRequiredStringSize,wTextArea,
           wSelect,wSelectInt,wSelectBool,wRadioSelect,wRadioBool,wCheckBool,
           wMultiCheckSelect,
           wPair,wTriple,w4Tuple,w5Tuple,w6Tuple,w7Tuple,w8Tuple,
           w9Tuple,w10Tuple,w11Tuple,w12Tuple,
           wCons2,wCons3,wCons4,wCons5,wCons6,wCons7,
           wCons8,wCons9,wCons10,wCons11,wCons12,wJoinTuple,
           wMaybe,wCheckMaybe,wRadioMaybe,
           wList,wListWithHeadings,wHList,wMatrix,wEither,
           WTree(..),wTree,
           WuiHandler,wuiHandler2button,
           renderTuple,renderTaggedTuple,renderList,
           mainWUI,wui2html,wuiInForm,wuiWithErrorForm,wuiHandler2button,
           wCons2JS,wCons3JS,wCons4JS,wCons5JS,wCons6JS,wCons7JS, -- internal...
           wCons8JS,wCons9JS,wCons10JS,wCons11JS,wCons12JS,        -- internal...
           withConditionJSName                                    -- internal...
           )
 where

import HTML
import Read(readNat)
import List(elemIndex,intersperse)
import Maybe
import Char(isDigit,isSpace)
import ReadShowTerm
import JavaScript

infixl 0 `withRendering`
infixl 0 `withError`
infixl 0 `withCondition`
infixl 0 `withConditionJS`

------------------------------------------------------------------------------
--- An internal WUI state is used to maintain the cgi references of the input
--- fields as a structure that corresponds to the structure of the edit data.
--- The last argument is always a JavaScript expression to access the input data
--- stored in the form elements.
data WuiState =
      -- reference to elementary input field:
     Ref CgiRef             (Maybe JSExp)
      -- string representation of a hidden value:
   | Hidden String          (Maybe JSExp)
      -- composition of trees (substructures):
   | CompNode [WuiState]    (Maybe JSExp)
      -- alternative of trees (union of substructures):
   | AltNode (Int,WuiState) (Maybe JSExp)

cgiRef2state :: CgiRef -> Maybe JSExp -> WuiState
cgiRef2state cr js = Ref cr js

state2cgiRef :: WuiState -> CgiRef
state2cgiRef (Ref cr _) = cr

value2state :: _ -> WuiState
value2state v = Hidden (showQTerm v) Nothing

state2value :: WuiState -> _
state2value (Hidden s _) = readQTerm s

-- Combine several WUI states into a single state.
-- The second argument is a string representation of the constructor
-- used in the JavaScript processing the of data or Nothing if processing
-- by JavaScript is not possible.
states2state :: [WuiState] -> Maybe ([JSExp]->JSExp) -> WuiState
states2state sts Nothing = CompNode sts Nothing
states2state sts (Just jscomb) =
  CompNode sts
           (if Nothing `elem` jsOfElems
            then Nothing
            else Just (jscomb (map fromJust jsOfElems)))
 where
   jsOfElems = map jsAccessToState sts

state2states :: WuiState -> [WuiState]
state2states (CompNode sts _) = sts

altstate2state :: (Int,WuiState) -> WuiState
altstate2state alt@(_,st) = AltNode alt (jsAccessToState st)

state2altstate :: WuiState -> (Int,WuiState)
state2altstate (AltNode alt _) = alt

-- Get the JavaScript function to access the current input values.
jsAccessToState :: WuiState -> Maybe JSExp
jsAccessToState (Ref      _ js) = js
jsAccessToState (Hidden   _ js) = js
jsAccessToState (CompNode _ js) = js
jsAccessToState (AltNode  _ js) = js

-- Set the JavaScript function to access the current input values to Nothing:
setNoJSAccessInWuiState :: WuiState -> WuiState
setNoJSAccessInWuiState (Ref      cref _) = Ref      cref Nothing
setNoJSAccessInWuiState (Hidden   s    _) = Hidden   s    Nothing
setNoJSAccessInWuiState (CompNode sts  _) = CompNode sts  Nothing
setNoJSAccessInWuiState (AltNode  alts _) = AltNode  alts Nothing

-- Compute a unique name for a state (used to identify error message tables).
state2refname :: WuiState -> String
state2refname (Ref      cref _) = idOfCgiRef cref
state2refname (Hidden   _ _) = ""
state2refname (CompNode ws _) =
  "comp" ++ concatMap (("_"++) . state2refname) ws
state2refname (AltNode (i,s) _) =
  "alt_" ++ show i ++ "_" ++ state2refname s

------------------------------------------------------------------------------
--- A rendering is a function that combines the visualization of components
--- of a data structure into some HTML expression.
type Rendering = [HtmlExp] -> HtmlExp

--- WuiParams specify the parameters of an individual Wui component type:
--- * the standard rendering
--- * an error message shown in case of illegal inputs
--- * a condition to specify legal input values
--- * optionally a JavaScript function name implementing the condition
type WuiParams a = (Rendering, String, a->Bool, Maybe String)

renderOf (render,_,_,_) = render

errorOf (_,err,_,_) = err

conditionOf (_,_,c,_) = c

jsConditionOf (_,_,_,jsc) = jsc

------------------------------------------------------------------------------
--- The type HtmlState describes a value consisting of an HTML expression
--- (usually containing some input elements), possibly a JavaScript expression
--- that checks the validity of the value, and a WUI state containing
--- references to input elements in the HTML expression.

type HtmlState = (HtmlExp, Maybe JSExp, WuiState)

------------------------------------------------------------------------------
--- A handler for a WUI is an event handler for HTML forms possibly with some
--- specific JavaScript code attached.
data WuiHandler = WHandler HtmlHandler (Maybe JSExp)

--- Transform a WUI handler into a submit button with a given label string.
wuiHandler2button :: String -> WuiHandler -> HtmlExp
wuiHandler2button title (WHandler handler mbjs) =
  let bt = button title handler in
  maybe bt
        (\jse->bt `addAttr` ("onclick","AllowSubmission = " ++ showJSExp jse))
        mbjs

------------------------------------------------------------------------------
--- The type of WUI specifications.
--- The first component are parameters specifying the behavior of this WUI type
--- (rendering, error message, and constraints on inputs).
--- The second component is a "show" function returning an HTML expression for
--- the edit fields and a WUI state containing the CgiRefs to extract
--- the values from the edit fields.
--- The third component is "read" function to extract the values from
--- the edit fields for a given cgi environment (returned as (Just v)).
--- If the value is not legal, Nothing is returned. The second component
--- of the result contains an HTML edit expression
--- together with a WUI state to edit the value again.
data WuiSpec a =
  WuiSpec (WuiParams a)
          (WuiParams a -> a -> HtmlState)
          (WuiParams a -> CgiEnv -> WuiState -> (Maybe a,HtmlState))

--- Puts a new rendering function into a WUI specification.
withRendering :: WuiSpec a -> Rendering -> WuiSpec a
withRendering (WuiSpec (_,errmsg,legal,jsck) showhtml readvalue) render =
  WuiSpec (render,errmsg,legal,jsck) showhtml readvalue


--- Puts a new error message into a WUI specification.
withError :: WuiSpec a -> String -> WuiSpec a
withError (WuiSpec (render,_,legal,jsck) showhtml readvalue) errmsg =
  WuiSpec (render,errmsg,legal,jsck) showhtml readvalue

--- Puts a new condition into a WUI specification.
withCondition :: WuiSpec a -> (a -> Bool) -> WuiSpec a
withCondition (WuiSpec (render,errmsg,_,_) showhtml readvalue) legal =
  WuiSpec (render,errmsg,legal,Nothing) showhtml readvalue

--- Puts a new JavaScript implementation of the condition
--- into a WUI specification.
withConditionJS :: WuiSpec a -> (a->Bool) -> WuiSpec a
withConditionJS (WuiSpec (render,errmsg,_,_) showhtml readvalue) legal =
  WuiSpec (render,errmsg,legal,Nothing) showhtml readvalue

--- Puts a new JavaScript implementation of the condition
--- into a WUI specification.
withConditionJSName :: WuiSpec a -> (a->Bool,String) -> WuiSpec a
withConditionJSName (WuiSpec (render,errmsg,_,_) showhtml readvalue)
                    (legal,jsck) =
  WuiSpec (render,errmsg,legal,if null jsck then Nothing else Just jsck)
          showhtml readvalue

--- Transforms a WUI specification from one type to another.
transformWSpec :: (a->b,b->a) -> WuiSpec a -> WuiSpec b
transformWSpec (a2b,b2a) (WuiSpec wparamsa showhtmla readvaluea) =
  WuiSpec (transParamA2B wparamsa)
          (\wparamsb b -> setNoJSAccessInHtmlState
                             (showhtmla (transParamB2A wparamsb) (b2a b)))
          (\wparamsb env wst ->
            let (mba,errstate) = readvaluea (transParamB2A wparamsb) env wst
             in (maybe Nothing (Just . a2b) mba,
                 setNoJSAccessInHtmlState errstate))
 where
  transParamA2B :: WuiParams a -> WuiParams b
  transParamA2B (render,errmsg,legal,_) =
    -- since we can't transform JS check code for type a into b, we ignore it:
    (render, errmsg, legal . b2a, Nothing)

  transParamB2A :: WuiParams b -> WuiParams a
  transParamB2A (render,errmsg,legal,_) =
    (render, errmsg, legal . a2b, jsConditionOf wparamsa)

  -- transform an HtmlState so that the JavaScript access function in
  -- the corresponding WuiState is set to Nothing (this is necessary since
  -- the JavaScript access function accesses values of type a and cannot
  -- be transformed in general to an access function for b values)
  setNoJSAccessInHtmlState (hexp,jsexp,ws) =
    (hexp, jsexp, setNoJSAccessInWuiState ws)

--- Adapt a WUI specification to a new type. For this purpose,
--- the first argument must be a transformation mapping values
--- from the old type to the new type. This function must be bijective
--- and operationally invertible (i.e., the inverse must be computable
--- by narrowing). Otherwise, use <code>transformWSpec</code>!
adaptWSpec :: (a->b) -> WuiSpec a -> WuiSpec b
adaptWSpec a2b = transformWSpec (a2b,invert a2b)

-- Compute the inverse of a function by exploiting function patterns:
invert :: (a->b) -> b -> a
invert f = f_invert
 where
  local_f x = f x
  --f_invert (local_f x) = x  -- here we use a function pattern
  f_invert y | (local_f x) =:<= y = x  where x free -- the same without fun.pat.


------------------------------------------------------------------------------
-- A collection of basic WUIs and WUI combinators:

--- A hidden widget for a value that is not shown in the WUI.
--- Usually, this is used in components of larger
--- structures, e.g., internal identifiers, data base keys.
wHidden :: WuiSpec a
wHidden =
  WuiSpec (head,"?",const True,Nothing) -- dummy values, not used
          (\_ v -> (hempty, Nothing, value2state v))
          (\_ _ s -> (Just (state2value s), (hempty,Nothing,s)))

--- A widget for values that are shown but cannot be modified.
--- The first argument is a mapping of the value into a HTML expression
--- to show this value.
wConstant :: (a->HtmlExp) -> WuiSpec a
wConstant showhtml =
  WuiSpec (head,"?",const True,Nothing)
          (\wparams v ->
                    ((renderOf wparams) [showhtml v], Nothing, value2state v))
          (\wparams _ s -> let v = state2value s in
                    (Just v, ((renderOf wparams) [showhtml v], Nothing, s)))

--- A widget for editing integer values.
wInt :: WuiSpec Int
wInt = WuiSpec
  (head,"Illegal integer:",const True,Nothing)
  (\(render,errmsg,_,jsck) v -> intWidget errmsg jsck render (showInt v))
  (\(render,errmsg,legal,jsck) env s ->
      let input = env (state2cgiRef s)
          renderr hexps = addErrMsg False False False errmsg "" (render hexps)
       in maybe (Nothing, intWidget errmsg jsck renderr input)
                (\v -> if legal v
                       then (Just v,  intWidget errmsg jsck render input)
                       else (Nothing, intWidget errmsg jsck renderr input))
                (readMaybeInt (stripSpaces input)))
 where
  showInt i = if i<0 then '-' : show (-i) else show i

  intWidget errmsg mbjs render s =
    (addErrMsg True True False errmsg refname
         (render [textfield ref s `addAttr` ("size","6")
                                  `addAttr` ("onblur",showJSExp jsCheckCall)]),
     Just jsCheckCall,
     cgiRef2state ref (Just jsaccess))
    where
      ref free
      refname = idOfCgiRef ref
      jsaccess = JSFCall "intValueOf" [JSString refname]
      parseIntCheckCall = JSFCall "parseIntCheck" [JSString refname]
      intCheckCall = maybe parseIntCheckCall
                           (\jsf->JSOp "&&" parseIntCheckCall
                                            (JSFCall jsf [jsaccess]))
                           mbjs
      jsCheckCall = JSFCall "setErrorClassName" [JSString refname,intCheckCall]

-- Remove leading and ending spaces in a string.
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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


checkLegalInput :: WuiParams a
                -> (String -> Maybe String -> Rendering -> a -> HtmlState)
                -> Bool
                -> a
                -> (Maybe a,HtmlState)
checkLegalInput (render,errmsg,legal,jsck) value2widget errorastable value =
  if legal value
  then (Just value, value2widget errmsg jsck render value)
  else (Nothing,
        value2widget errmsg jsck
                     (\hes -> addErrMsg False False errorastable errmsg ""
                                        (render hes))
                     value)


--- A predefined filter for processing string inputs.
--- Here, we replace \r\n by \n:
filterStringInput :: String -> String
filterStringInput = removeCRs

--- Replace all \r\n by \n:
removeCRs :: String -> String
removeCRs [] = []
removeCRs [c] = [c]
removeCRs (c1:c2:cs) =
  if c1=='\r' && c2=='\n' then '\n' : removeCRs cs
                          else c1 : removeCRs (c2:cs)

--- A widget for editing string values.
wString :: WuiSpec String
wString = wStringAttrs []

--- A widget for editing string values with a size attribute.
wStringSize :: Int -> WuiSpec String
wStringSize size = wStringAttrs [("size",show size)]

--- A widget for editing string values with some attributes for the
--- text field.
wStringAttrs :: [(String,String)] -> WuiSpec String
wStringAttrs attrs =
  WuiSpec (head, "?", const True, Nothing)
          (\(render,errmsg,_,jsck) v -> stringWidget errmsg jsck render v)
          (\wparams env s ->
                checkLegalInput wparams stringWidget False
                                (filterStringInput (env (state2cgiRef s))))
 where
  stringWidget errmsg mbjs render v =
     (addErrMsg (isJust jsCheckCall) True False errmsg refname
        (render [foldr (flip addAttr) (textfield ref v) (attrs++onblurAttr)]),
      jsCheckCall,
      cgiRef2state ref (Just jsaccess))
    where
      ref free
      refname = idOfCgiRef ref
      jsaccess = JSFCall "stringValueOf" [JSString refname]
      jsCheckCall = maybeJSFun2checkCall refname jsaccess mbjs
      onblurAttr = maybe [] (\jsc->[("onblur",showJSExp jsc)]) jsCheckCall

--- A widget for editing string values that are required to be non-empty.
wRequiredString :: WuiSpec String
wRequiredString =
  withConditionJSName (wString `withError` "Missing input:")
                      (not . null, "notEmpty")

--- A widget with a size attribute for editing string values
--- that are required to be non-empty.
wRequiredStringSize :: Int -> WuiSpec String
wRequiredStringSize size =
  withConditionJSName (wStringSize size `withError` "Missing input:")
                      (not . null, "notEmpty")

--- A widget for editing string values in a text area.
--- The argument specifies the height and width of the text area.
wTextArea :: (Int,Int) -> WuiSpec String
wTextArea dims =
  WuiSpec (head, "?", const True, Nothing)
          (\ (render,errmsg,_,jsck) v -> textareaWidget errmsg jsck render v)
          (\wparams env s ->
             checkLegalInput wparams textareaWidget False
                             (filterStringInput (env (state2cgiRef s))))
 where
  textareaWidget errmsg mbjs render v =
     (addErrMsg (isJust jsCheckCall) True False errmsg refname
           (render [textarea ref dims v `addAttrs` onblurAttr]),
      jsCheckCall,
      cgiRef2state ref (Just jsaccess))
    where
      ref free
      refname = idOfCgiRef ref
      jsaccess = JSFCall "stringValueOf" [JSString refname]
      jsCheckCall = maybeJSFun2checkCall refname jsaccess mbjs
      onblurAttr = maybe [] (\jsc->[("onblur",showJSExp jsc)]) jsCheckCall


--- A widget to select a value from a given list of values.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into strings to be shown
--- in the selection widget.
wSelect :: (a->String) -> [a] -> WuiSpec a
wSelect showelem selset =
  WuiSpec (head,"?",const True, Nothing)
          (\ (render,errmsg,_,jsck) v -> selWidget errmsg jsck render v)
          (\wparams env s ->
             checkLegalInput wparams selWidget False
                             (selset !! readNat (env (state2cgiRef s))))
 where
  selWidget _ _ render v =
     (render [maybe (selection ref namevalues)
                    (\i -> selectionInitial ref namevalues i)
                    idx],
      Nothing,
      cgiRef2state ref Nothing)
    where
      ref free
      idx = elemIndex v selset
      namevalues = zip (map showelem selset) (map show [0..])


--- A widget to select a value from a given list of values that are
--- representable in JavaScript. In contrast to the more general combinator
--- wSelect, this combinator supports the client-side checking of
--- conditions by JavaScipt.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into strings to be shown
--- in the selection widget.
--- The second argument maps value into the corresponding JavaScript
--- representation.
wSelectJS :: (a->String) -> (a->JSExp) -> [a] -> WuiSpec a
wSelectJS showelem showjselem selset =
  WuiSpec (head,"?",const True, Nothing)
          (\ (render,errmsg,_,jsck) v -> selWidget errmsg jsck render v)
          (\wparams env s ->
             checkLegalInput wparams selWidget False
                             (selset !! readNat (env (state2cgiRef s))))
 where
  selWidget errmsg mbjs render v =
     (addErrMsg (isJust jsCheckCall) True False errmsg refname
         (render [maybe (selection ref namevalues)
                        (\i -> selectionInitial ref namevalues i)
                        idx]),
      jsCheckCall,
      cgiRef2state ref (Just jsaccess))
    where
      ref free
      refname = idOfCgiRef ref
      jsaccess = JSFCall "selectValueOf"
                          [JSString refname,
                           JSFCall "new Array" (map showjselem selset)]
      jsCheckCall = maybeJSFun2checkCall refname jsaccess mbjs
      idx = elemIndex v selset
      namevalues = zip (map showelem selset) (map show [0..])


--- A widget to select a value from a given list of integers (provided as
--- the argument).
--- The current value should be contained in the value list and is preselected.
wSelectInt :: [Int] -> WuiSpec Int
wSelectInt = wSelectJS show JSInt


--- A widget to select a Boolean value via a selection box.
--- The arguments are the strings that are shown for the values
--- True and False in the selection box, respectively.
--- @param true - string for selection of True
--- @param false - string for selection of False
--- @return a WUI specification for a Boolean selection widget
wSelectBool :: String -> String -> WuiSpec Bool
wSelectBool true false =
  wSelectJS (\b->if b then true else false) JSBool [True,False]


--- A widget to select a Boolean value via a check box.
--- The first argument are HTML expressions that are shown after the
--- check box.  The result is True if the box is checked.
wCheckBool :: [HtmlExp] -> WuiSpec Bool
wCheckBool hexps =
  WuiSpec (head, "?", const True, Nothing)
          (\ (render,errmsg,_,jsck) v -> checkWidget errmsg jsck render v)
          (\wparams env s ->
            checkLegalInput wparams checkWidget False
                            (env (state2cgiRef s)=="True"))
 where
  checkWidget errmsg mbjs render v =
    (addErrMsg (isJust jsCheckCall) True False errmsg refname
        (render [inline ((if v then checkedbox else checkbox) ref "True"
                             : hexps)]),
     jsCheckCall,
     cgiRef2state ref (Just jsaccess))
    where
      ref free
      refname = idOfCgiRef ref
      jsaccess = JSFCall "checkBoxValueOf" [JSString refname]
      jsCheckCall = maybeJSFun2checkCall refname jsaccess mbjs


--- A widget to select a list of values from a given list of values
--- via check boxes.
--- The current values should be contained in the value list
--- and are preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the check box.
wMultiCheckSelect :: (a->[HtmlExp]) -> [a] -> WuiSpec [a]
wMultiCheckSelect showelem selset =
  WuiSpec (renderTuple, tupleError, const True, Nothing)
          (\ (render,errmsg,_,jsck) vs -> checkWidget errmsg jsck render vs)
          (\wparams env st ->
             checkLegalInput wparams checkWidget True
                 (concatMap (\ (ref,s) -> if env ref=="True" then [s] else [])
                            (zip (map state2cgiRef (state2states st)) selset)))
 where
  checkWidget _ _ render vs = -- TODO: add JavaScript code
    let refs = take (length selset) newVars
        numsetitems = zip refs selset
        showItem (ref,s) =
           inline ((if s `elem` vs then checkedbox else checkbox)
                                                       ref "True" : showelem s)
     in (render (map showItem numsetitems), Nothing,
         states2state (map (\cref->cgiRef2state cref Nothing) refs) Nothing)

newVars = unknown : newVars


--- A widget to select a value from a given list of values via a radio button.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the radio button.
wRadioSelect :: (a->[HtmlExp]) -> [a] -> WuiSpec a
wRadioSelect showelem selset =
  WuiSpec (renderTuple, tupleError, const True, Nothing)
          (\ (render,errmsg,_,jsck) v -> radioWidget errmsg jsck render v)
          (\wparams env s ->
             checkLegalInput wparams radioWidget True
                             (selset !! readNat (env (state2cgiRef s))))
 where
  radioWidget _ _ render v = -- TODO: add JavaScript code
     (render (map showItem numhitems),
      Nothing,
      cgiRef2state ref Nothing)
    where
      ref free
      idx = maybe 0 id (elemIndex v selset)
      numhitems = zip [0..] (map showelem selset)
      showItem (i,s) = table [[[(if i==idx then radio_main else radio_other)
                                      ref (show i)],s]]

--- A widget to select a Boolean value via a radio button.
--- The arguments are the lists of HTML expressions that are shown after
--- the True and False radio buttons, respectively.
--- @param true - HTML expressions for True radio button
--- @param false - HTML expressions for False radio button
--- @return a WUI specification for a Boolean selection widget
wRadioBool :: [HtmlExp] -> [HtmlExp] -> WuiSpec Bool
wRadioBool truehexps falsehexps =
  wRadioSelect (\b->if b then truehexps else falsehexps) [True,False]


-----------------------------------------------------------------------
-- Auxliary functions used in WUI combinators:

--- Translate a (maybe) JavaScript check function into a JavaScript call:
maybeJSFun2checkCall _ _ Nothing = Nothing
maybeJSFun2checkCall refname jsaccessvalue (Just jsf) =
  Just (JSFCall "setErrorClassName" [JSString refname,
                                     JSFCall jsf [jsaccessvalue]])


--- Conditionally (if first argument is true) adds an error message
--- that is initially hidden (if second argument is false).
--- If the third argument is true, the error message is framed into a table
--- around the HTML expression and the message is shown as the first row of
--- the a table, otherwise the error message is shown in front of the
--- around an HTML expression.
addErrMsg :: Bool -> Bool -> Bool -> String -> String -> HtmlExp -> HtmlExp
addErrMsg hasjsck hide astable errmsg refname hexp =
  if not hide || hasjsck
  then
   if astable
   then table [[[style errmsgclass [htxt errmsg] `addAttrs` msgidattr]],[[hexp]]]
          `addAttrs` ([("class",tableclass)] ++ tblidattr)
   else inline [style errmsgclass [htxt errmsg] `addAttrs` msgidattr, hexp]
  else hexp
 where
  msgidattr   = if null refname then [] else [("id","MSG_"++refname)]
  tblidattr   = if null refname then [] else [("id",refname)]
  tableclass  = if hide then "wuipassiveerrmsg" else "wuiactiveerrmsg"
  errmsgclass = if hide then "wuihide"          else "wuinohide"


-- Tuple constructor of arity n in JavaScript:
jsTupleCons n = jsConsTerm ('(' : replicate (n - 1) ',' ++ ")")


-- Join a list of input fields (each consisting of HTML expressions,
-- JavaScript checks, WUI states) into a single structure accordding
-- to the WUI parameters. If the second argument is True, the error message
-- is hidden until explicitly displayed by the JavaScript checks.
-- The third argument (jscomb) is a JavaScript combinator that combines
-- the expressions to access the values of the subfields into an expression
-- to access the complete combined value (or Nothing if such a combination
-- is not possible)
joinSubFields :: WuiParams _ -> Bool -> [(HtmlExp,Maybe JSExp,WuiState)]
              -> Maybe ([JSExp]->JSExp) -> (HtmlExp,Maybe JSExp,WuiState)
joinSubFields (render,errmsg,_,mbjsck) hide subfields jscomb =
  let (subhexps,subjscks,substates) = unzip3 subfields
      consstate = states2state substates jscomb
      refname = state2refname consstate
      jscall = jsCheckCallFromState consstate mbjsck
      jscks = catMaybes subjscks
   in (addErrMsg (isJust jscall) hide True errmsg refname (render subhexps),
       if null jscks
       then jscall
       else Just
         (maybe (parAndJS jscks)
                (\jsc->JSOp "&&"
                            (JSOp "&&" (JSFCall "unsetErrorClassName"
                                                [JSString refname])
                                       (parAndJS jscks))
                            jsc)
                jscall),
       consstate)
 where
  -- generate strict conjunction of non-empty list of JavaScript expressions:
  parAndJS [j] = j
  parAndJS (j1:j2:js) = JSFCall "And" [j1,parAndJS (j2:js)]

-- Check and join values of subelements.
-- If there is an error is some subfield (indicated by the fourth argument),
-- return Nothing, otherwise check the combined value.
checkAndJoinSubFields wparams subfields jscons errorinsubfields joinvalue =
  let combine hide = joinSubFields wparams hide subfields jscons
   in if errorinsubfields
      then (Nothing, combine True)
      else if (conditionOf wparams) joinvalue
           then (Just joinvalue, combine True) 
           else (Nothing,        combine False)

-- Generate a single structure that is a given alternative of an input field
-- (each consisting of an HTML expression, JavaScript check, WUI state)
-- accordding to the WUI parameters.
-- If the second argument is True, the error message
-- is hidden until explicitly displayed by the JavaScript checks.
altSubField :: WuiParams _ -> Bool -> HtmlExp -> Maybe JSExp -> (Int,WuiState)
               -> (HtmlExp,Maybe JSExp,WuiState)
altSubField (render,errmsg,_,mbjsck) hide subhexp subjsck altstate =
  let consstate = altstate2state altstate
      refname = state2refname consstate
      jscons = jsCheckCallFromState consstate mbjsck
   in (addErrMsg (isJust jscons) hide False errmsg refname (render [subhexp]),
       maybe jscons
             (\subjs -> Just
                (maybe subjs
                       (\jsc->JSOp "&&"
                                   (JSOp "&&" (JSFCall "unsetErrorClassName"
                                                       [JSString refname])
                                              subjs)
                                   jsc)
                       jscons))
             subjsck,
       consstate)

jsCheckCallFromState _ Nothing = Nothing
jsCheckCallFromState wstate (Just jsck) =
  maybe Nothing
        (\jsstateaccess -> Just (JSFCall  "setErrorClassName"
                                          [JSString (state2refname wstate),
                                           JSFCall jsck [jsstateaccess]]))
        (jsAccessToState wstate)

-------------------------------------------------------------------------

--- WUI combinator to combine two tuples into a joint tuple.
--- It is similar to wPair but renders both components as a single
--- tuple provided that the components are already rendered as tuples,
--- i.e., by the rendering function <code>renderTuple</code>.
--- This combinator is useful to define combinators for large tuples.
wJoinTuple :: WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
wJoinTuple (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb) =
  WuiSpec (renderTuple, tupleError, const True, Nothing) showc readc
 where
  jscons = Just (jsTupleCons 2)

  render2joinrender render [h1,h2] =
    let h1s = unRenderTuple h1
        h2s = unRenderTuple h2
     in render (h1s++h2s)

  addJoinRender (render,errmsg,legal,jsck) =
    (render2joinrender render,errmsg,legal,jsck)

  showc wparams vc | (va,vb) =:<= vc =
    joinSubFields (addJoinRender wparams) True
                  [showa wparama va, showb wparamb vb] jscons
   where va,vb free

  readc wparams env s =
    let [ra,rb] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
     in checkAndJoinSubFields (addJoinRender wparams) [fielda,fieldb] jscons
            (rav==Nothing || rbv==Nothing)
            (fromJust rav, fromJust rbv)


--- WUI combinator for pairs.
wPair :: WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
wPair = wCons2JS (Just (jsTupleCons 2)) (\a b -> (a,b))

--- WUI combinator for constructors of arity 2.
--- The first argument is the binary constructor.
--- The second and third arguments are the WUI specifications
--- for the argument types.
wCons2 :: (a->b->c) -> WuiSpec a -> WuiSpec b -> WuiSpec c
wCons2 = wCons2JS Nothing

wCons2JS jscons cons
         (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb) =
  WuiSpec (renderTuple, tupleError, const True, Nothing) showc readc
 where
  showc wparams vc | cons va vb =:<= vc =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb] jscons
   where va,vb free

  readc wparams env s =
    let [ra,rb] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
     in checkAndJoinSubFields wparams [fielda,fieldb] jscons
            (rav==Nothing || rbv==Nothing)
            (cons (fromJust rav) (fromJust rbv))

--- WUI combinator for triples.
wTriple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec (a,b,c)
wTriple = wCons3JS (Just (jsTupleCons 3)) (\a b c -> (a,b,c))

--- WUI combinator for constructors of arity 3.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons3 :: (a->b->c->d) -> WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d
wCons3 = wCons3JS Nothing

wCons3JS jscons cons
         (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
         (WuiSpec wparamc showc readc) =
  WuiSpec (renderTuple, tupleError, const True, Nothing) showd readd
 where
  showd wparams vd | cons va vb vc =:<= vd =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc] jscons
   where va,vb,vc free

  readd wparams env s =
    let [ra,rb,rc] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv))


--- WUI combinator for tuples of arity 4.
w4Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec (a,b,c,d)
w4Tuple = wCons4JS (Just (jsTupleCons 4)) (\a b c d -> (a,b,c,d))

--- WUI combinator for constructors of arity 4.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons4  :: (a->b->c->d->e) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e
wCons4 = wCons4JS Nothing

wCons4JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showe reade
 where
  showe wparams ve | cons va vb vc vd =:<= ve =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd] jscons
    where va,vb,vc,vd free

  reade wparams env s =
    let [ra,rb,rc,rd] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
     in checkAndJoinSubFields wparams
            [fielda,fieldb,fieldc,fieldd] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv))


--- WUI combinator for tuples of arity 5.
w5Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec (a,b,c,d,e)
w5Tuple = wCons5JS (Just (jsTupleCons 5)) (\a b c d e -> (a,b,c,d,e))

--- WUI combinator for constructors of arity 5.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons5  :: (a->b->c->d->e->f) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f
wCons5 = wCons5JS Nothing

wCons5JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showf readf
 where
  showf wparams vl | cons va vb vc vd ve =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve] jscons
    where va,vb,vc,vd,ve free

  readf wparams env s =
    let [ra,rb,rc,rd,re] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev))


--- WUI combinator for tuples of arity 6.
w6Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec (a,b,c,d,e,f)
w6Tuple = wCons6JS (Just (jsTupleCons 6)) (\a b c d e f -> (a,b,c,d,e,f))

--- WUI combinator for constructors of arity 6.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons6  :: (a->b->c->d->e->f->g) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g
wCons6 = wCons6JS Nothing

wCons6JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showg readg
 where
  showg wparams vl | cons va vb vc vd ve vf =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf] jscons
    where va,vb,vc,vd,ve,vf free

  readg wparams env s =
    let [ra,rb,rc,rd,re,rf] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv))


--- WUI combinator for tuples of arity 7.
w7Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec (a,b,c,d,e,f,g)
w7Tuple = wCons7JS (Just (jsTupleCons 7)) (\a b c d e f g -> (a,b,c,d,e,f,g))

--- WUI combinator for constructors of arity 7.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons7  :: (a->b->c->d->e->f->g->h) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h
wCons7 = wCons7JS Nothing

wCons7JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf)
        (WuiSpec wparamg showg readg) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showh readh
 where
  showh wparams vl | cons va vb vc vd ve vf vg =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf,
                    showg wparamg vg] jscons
    where va,vb,vc,vd,ve,vf,vg free

  readh wparams env s =
    let [ra,rb,rc,rd,re,rf,rg] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
        (rgv,fieldg) = readg wparamg env rg
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf,fieldg] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing || rgv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv) (fromJust rgv))


--- WUI combinator for tuples of arity 8.
w8Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec (a,b,c,d,e,f,g,h)
w8Tuple = wCons8JS (Just (jsTupleCons 8))
                   (\a b c d e f g h -> (a,b,c,d,e,f,g,h))

--- WUI combinator for constructors of arity 8.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons8  :: (a->b->c->d->e->f->g->h->i) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i
wCons8 = wCons8JS Nothing

wCons8JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf)
        (WuiSpec wparamg showg readg) (WuiSpec wparamh showh readh) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showi readi
 where
  showi wparams vl | cons va vb vc vd ve vf vg vh =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf,
                    showg wparamg vg,
                    showh wparamh vh] jscons
    where va,vb,vc,vd,ve,vf,vg,vh free

  readi wparams env s =
    let [ra,rb,rc,rd,re,rf,rg,rh] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
        (rgv,fieldg) = readg wparamg env rg
        (rhv,fieldh) = readh wparamh env rh
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf,fieldg,fieldh] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv) (fromJust rgv) (fromJust rhv))


--- WUI combinator for tuples of arity 9.
w9Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i ->
           WuiSpec (a,b,c,d,e,f,g,h,i)
w9Tuple = wCons9JS (Just (jsTupleCons 9))
                   (\a b c d e f g h i -> (a,b,c,d,e,f,g,h,i))

--- WUI combinator for constructors of arity 9.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons9  :: (a->b->c->d->e->f->g->h->i->j) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j
wCons9 = wCons9JS Nothing

wCons9JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf)
        (WuiSpec wparamg showg readg) (WuiSpec wparamh showh readh)
        (WuiSpec wparami showi readi) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showj readj
 where
  showj wparams vl | cons va vb vc vd ve vf vg vh vi =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf,
                    showg wparamg vg,
                    showh wparamh vh,
                    showi wparami vi] jscons
    where va,vb,vc,vd,ve,vf,vg,vh,vi free

  readj wparams env s =
    let [ra,rb,rc,rd,re,rf,rg,rh,ri] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
        (rgv,fieldg) = readg wparamg env rg
        (rhv,fieldh) = readh wparamh env rh
        (riv,fieldi) = readi wparami env ri
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf,fieldg,fieldh,
                                       fieldi] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing ||
             riv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv) (fromJust rgv) (fromJust rhv)
                  (fromJust riv))


--- WUI combinator for tuples of arity 10.
w10Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec (a,b,c,d,e,f,g,h,i,j)
w10Tuple = wCons10JS (Just (jsTupleCons 10))
                     (\a b c d e f g h i j -> (a,b,c,d,e,f,g,h,i,j))

--- WUI combinator for constructors of arity 10.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons10  :: (a->b->c->d->e->f->g->h->i->j->k) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k
wCons10 = wCons10JS Nothing

wCons10JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf)
        (WuiSpec wparamg showg readg) (WuiSpec wparamh showh readh)
        (WuiSpec wparami showi readi) (WuiSpec wparamj showj readj) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showk readk
 where
  showk wparams vl | cons va vb vc vd ve vf vg vh vi vj =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf,
                    showg wparamg vg,
                    showh wparamh vh,
                    showi wparami vi,
                    showj wparamj vj] jscons
    where va,vb,vc,vd,ve,vf,vg,vh,vi,vj free

  readk wparams env s =
    let [ra,rb,rc,rd,re,rf,rg,rh,ri,rj] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
        (rgv,fieldg) = readg wparamg env rg
        (rhv,fieldh) = readh wparamh env rh
        (riv,fieldi) = readi wparami env ri
        (rjv,fieldj) = readj wparamj env rj
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf,fieldg,fieldh,
                                       fieldi,fieldj] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing ||
             riv==Nothing || rjv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv) (fromJust rgv) (fromJust rhv)
                  (fromJust riv) (fromJust rjv))


--- WUI combinator for tuples of arity 11.
w11Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec (a,b,c,d,e,f,g,h,i,j,k)
w11Tuple = wCons11JS (Just (jsTupleCons 11))
                     (\a b c d e f g h i j k -> (a,b,c,d,e,f,g,h,i,j,k))

--- WUI combinator for constructors of arity 11.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons11  :: (a->b->c->d->e->f->g->h->i->j->k->l) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l
wCons11 = wCons11JS Nothing

wCons11JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf)
        (WuiSpec wparamg showg readg) (WuiSpec wparamh showh readh)
        (WuiSpec wparami showi readi) (WuiSpec wparamj showj readj)
        (WuiSpec wparamk showk readk) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showl readl
 where
  showl wparams vl | cons va vb vc vd ve vf vg vh vi vj vk =:<= vl =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf,
                    showg wparamg vg,
                    showh wparamh vh,
                    showi wparami vi,
                    showj wparamj vj,
                    showk wparamk vk] jscons
    where va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk free

  readl wparams env s =
    let [ra,rb,rc,rd,re,rf,rg,rh,ri,rj,rk] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
        (rgv,fieldg) = readg wparamg env rg
        (rhv,fieldh) = readh wparamh env rh
        (riv,fieldi) = readi wparami env ri
        (rjv,fieldj) = readj wparamj env rj
        (rkv,fieldk) = readk wparamk env rk
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf,fieldg,fieldh,
                                       fieldi,fieldj,fieldk] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing ||
             riv==Nothing || rjv==Nothing || rkv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv) (fromJust rgv) (fromJust rhv)
                  (fromJust riv) (fromJust rjv) (fromJust rkv))


--- WUI combinator for tuples of arity 12.
w12Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec (a,b,c,d,e,f,g,h,i,j,k,l)
w12Tuple = wCons12JS (Just (jsTupleCons 12))
                     (\a b c d e f g h i j k l -> (a,b,c,d,e,f,g,h,i,j,k,l))

--- WUI combinator for constructors of arity 12.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons12  :: (a->b->c->d->e->f->g->h->i->j->k->l->m) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m
wCons12 = wCons12JS Nothing

wCons12JS jscons cons
        (WuiSpec wparama showa reada) (WuiSpec wparamb showb readb)
        (WuiSpec wparamc showc readc) (WuiSpec wparamd showd readd)
        (WuiSpec wparame showe reade) (WuiSpec wparamf showf readf)
        (WuiSpec wparamg showg readg) (WuiSpec wparamh showh readh)
        (WuiSpec wparami showi readi) (WuiSpec wparamj showj readj)
        (WuiSpec wparamk showk readk) (WuiSpec wparaml showl readl) =
  WuiSpec (renderTuple,tupleError, const True, Nothing) showm readm
 where
  showm wparams vm | cons va vb vc vd ve vf vg vh vi vj vk vl =:<= vm =
    joinSubFields wparams True
                   [showa wparama va,
                    showb wparamb vb,
                    showc wparamc vc,
                    showd wparamd vd,
                    showe wparame ve,
                    showf wparamf vf,
                    showg wparamg vg,
                    showh wparamh vh,
                    showi wparami vi,
                    showj wparamj vj,
                    showk wparamk vk,
                    showl wparaml vl] jscons
    where va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl free

  readm wparams env s =
    let [ra,rb,rc,rd,re,rf,rg,rh,ri,rj,rk,rl] = state2states s
        (rav,fielda) = reada wparama env ra
        (rbv,fieldb) = readb wparamb env rb
        (rcv,fieldc) = readc wparamc env rc
        (rdv,fieldd) = readd wparamd env rd
        (rev,fielde) = reade wparame env re
        (rfv,fieldf) = readf wparamf env rf
        (rgv,fieldg) = readg wparamg env rg
        (rhv,fieldh) = readh wparamh env rh
        (riv,fieldi) = readi wparami env ri
        (rjv,fieldj) = readj wparamj env rj
        (rkv,fieldk) = readk wparamk env rk
        (rlv,fieldl) = readl wparaml env rl
     in checkAndJoinSubFields wparams [fielda,fieldb,fieldc,fieldd,
                                       fielde,fieldf,fieldg,fieldh,
                                       fieldi,fieldj,fieldk,fieldl] jscons
            (rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
             rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing ||
             riv==Nothing || rjv==Nothing || rkv==Nothing || rlv==Nothing)
            (cons (fromJust rav) (fromJust rbv) (fromJust rcv) (fromJust rdv)
                  (fromJust rev) (fromJust rfv) (fromJust rgv) (fromJust rhv)
                  (fromJust riv) (fromJust rjv) (fromJust rkv) (fromJust rlv))


--- WUI combinator for list structures where the list elements are vertically
--- aligned in a table.
wList :: WuiSpec a -> WuiSpec [a]
wList (WuiSpec rendera showa reada) = WuiSpec
  (renderList,"Illegal list:",const True, Nothing)
  (\wparams vas ->
     joinSubFields wparams True (map (showa rendera) vas) (Just jsListComb))
  (\wparams env s ->
     let rvs = map (reada rendera env) (state2states s)
         combine hide = joinSubFields wparams hide (map snd rvs)
                                                   (Just jsListComb)
      in if Nothing `elem` (map fst rvs)
         then (Nothing, combine True)
         else let value = map (fromJust . fst) rvs in
              if (conditionOf wparams) value
              then (Just value, combine True)
              else (Nothing,    combine False) )
 where
  jsListComb args = JSFCall "array2list" [JSFCall "new Array" args]

--- Add headings to a standard WUI for list structures:
wListWithHeadings :: [String] -> WuiSpec a -> WuiSpec [a]
wListWithHeadings headings wspec =
  wList wspec `withRendering` renderHeadings
 where
  renderHeadings hs = addHeadings (renderList hs) (map (\s->[htxt s]) headings)

--- WUI combinator for list structures where the list elements are horizontally
--- aligned in a table.
wHList :: WuiSpec a -> WuiSpec [a]
wHList wspec = wList wspec `withRendering` renderTuple


--- WUI for matrices, i.e., list of list of elements
--- visualized as a matrix.
wMatrix :: WuiSpec a -> WuiSpec [[a]]
wMatrix wspec = wList (wHList wspec)


--- WUI for Maybe values. It is constructed from a WUI for
--- Booleans and a WUI for the potential values. Nothing corresponds
--- to a selection of False in the Boolean WUI.
--- The value WUI is shown after the Boolean WUI.
--- @param wspecb - a WUI specification for Boolean values
--- @param wspeca - a WUI specification for the type of potential values
--- @param def - a default value that is used if the current value is Nothing
wMaybe :: WuiSpec Bool -> WuiSpec a -> a -> WuiSpec (Maybe a)
wMaybe (WuiSpec paramb showb readb) (WuiSpec parama showa reada) def =
 WuiSpec
   (renderTuple, tupleError, const True, Nothing)
   (\wparams mbs ->
         joinSubFields wparams True
                       [showb paramb (mbs/=Nothing),
                        showa parama (maybe def id mbs)] Nothing)
   (\wparams env s ->
     let [rb,ra] = state2states s
         (rbv,fieldb) = readb paramb env rb
         (rav,fielda) = reada parama env ra
         combine hide = joinSubFields wparams hide [fieldb,fielda] Nothing
      in if rbv==Nothing || rav==Nothing
         then (Nothing, combine True)
         else let value = if fromJust rbv
                          then Just (fromJust rav)
                          else Nothing in
              if (conditionOf wparams) value
              then (Just value, combine True)
              else (Nothing,    combine False))


--- A WUI for Maybe values where a check box is used to select Just.
--- The value WUI is shown after the check box.
--- @param wspec - a WUI specification for the type of potential values
--- @param hexps - a list of HTML expressions shown after the check box
--- @param def - a default value if the current value is Nothing
wCheckMaybe :: WuiSpec a -> [HtmlExp] -> a -> WuiSpec (Maybe a)
wCheckMaybe wspec exps = wMaybe (wCheckBool exps) wspec

--- A WUI for Maybe values where radio buttons are used to switch
--- between Nothing and Just.
--- The value WUI is shown after the radio button WUI.
--- @param wspec - a WUI specification for the type of potential values
--- @param hexps - a list of HTML expressions shown after the Nothing button
--- @param hexps - a list of HTML expressions shown after the Just button
--- @param def - a default value if the current value is Nothing
wRadioMaybe :: WuiSpec a -> [HtmlExp] -> [HtmlExp] -> a -> WuiSpec (Maybe a)
wRadioMaybe wspec hnothing hjust = wMaybe wBool wspec
 where
  wBool = wRadioSelect (\b->if b then hjust else hnothing) [False,True]

--- WUI for union types.
--- Here we provide only the implementation for Either types
--- since other types with more alternatives can be easily reduced to this case.
wEither :: WuiSpec a -> WuiSpec b -> WuiSpec (Either a b)
wEither (WuiSpec rendera showa reada) (WuiSpec renderb showb readb) =
 WuiSpec (head, "?", const True, Nothing) showEither readEither
 where
  showEither wparams (Left va) =
    let (hea,jsa,rta) = showa rendera va
     in altSubField wparams True hea jsa (1,rta)
  showEither wparams (Right vb) =
    let (heb,jsb,rtb) = showb renderb vb
     in altSubField wparams True heb jsb (2,rtb)

  readEither wparams env s =
    let (altindex,rab) = state2altstate s
     in case altindex of
         1 -> let (rv,(he,jsck,rst)) = reada rendera env rab
               in checkValue (rv==Nothing) (Left  (fromJust rv)) he jsck (1,rst)
         2 -> let (rv,(he,jsck,rst)) = readb renderb env rab
               in checkValue (rv==Nothing) (Right (fromJust rv)) he jsck (2,rst)
   where
     checkValue isnothing value hexp jsck altstate =
      let combine hide = altSubField wparams hide hexp jsck altstate
       in if isnothing
          then (Nothing, combine True)
          else if (conditionOf wparams) value
               then (Just value, combine True)
               else (Nothing,    combine False)


--- A simple tree structure to demonstrate the construction of WUIs for tree
--- types.
data WTree a = WLeaf a | WNode [WTree a]

--- WUI for tree types.
--- The rendering specifies the rendering of inner nodes.
--- Leaves are shown with their default rendering.
wTree :: WuiSpec a -> WuiSpec (WTree a)
wTree (WuiSpec rendera showa reada) =
 WuiSpec (renderList, "Illegal tree:", const True, Nothing) showTree readTree
 where
  showTree _ (WLeaf va) =
    let (hea,jsa,rta) = showa rendera va
     in altSubField rendera True hea jsa (1,rta)
  showTree wparams (WNode ns) =
    let (hes,jscks,sts) = joinSubFields wparams True
                                        (map (showTree wparams) ns) Nothing
     in (hes,jscks,altstate2state (2,sts))

  readTree wpar env s = let (altindex,rab) = state2altstate s in
    case altindex of
     1 -> let (rv,(he,jsck,rst)) = reada rendera env rab
              combine hide = altSubField rendera hide he jsck (1,rst)
           in checkValue combine (rv==Nothing) (WLeaf (fromJust rv))
     2 -> let rvs = map (readTree wpar env) (state2states rab)
              combine hide =
                let (hes,jscks,sts) = joinSubFields wpar hide
                                                    (map snd rvs) Nothing
                 in (hes,jscks,altstate2state (2,sts))
           in checkValue combine (Nothing `elem`  (map fst rvs))
                         (WNode (map (fromJust . fst) rvs))

   where
     checkValue combine isnothing value =
        if isnothing
        then (Nothing, combine True)
        else if conditionOf wpar value
             then (Just value, combine True)
             else (Nothing,    combine False)


-------------------------------------------------------------------------------
-- Definition of standard rendering functions

--- Standard rendering of tuples as a table with a single row.
--- Thus, the elements are horizontally aligned.
renderTuple :: Rendering
renderTuple hexps = table [map (\h->[h]) hexps]

--- Inverse operation of renderTuple. If the argument has not the
--- shape of the renderTuple output, it is returned unchanged.
--- In future versions, this operation is better implemented using
--- functional logic features, but currently the encapsulated search
--- is a bit weak for this purpose.
unRenderTuple :: HtmlExp -> [HtmlExp]
unRenderTuple hexp =
  if isTupleTable hexp
  then getTupleTableElems hexp
  else [hexp]
 where
  isTupleTable he = case he of
    HtmlStruct "table" [] [HtmlStruct "tr" [] tds] -> all isSingleElem tds
    _ -> False

  isSingleElem he = case he of
    HtmlStruct "td" _ [_] -> True
    _ -> False

  getTupleTableElems (HtmlStruct "table" [] [HtmlStruct "tr" [] tds]) =
    map (\ (HtmlStruct "td" _ [e]) -> e) tds

-- Standard error message for tuples:
tupleError = "Illegal combination:"

--- Standard rendering of tuples with a tag for each element.
--- Thus, each is preceded by a tag, that is set in bold, and all
--- elements are vertically aligned.
renderTaggedTuple :: [String] -> Rendering
renderTaggedTuple tags hexps =
  table (map (\(t,h)->[[bold [htxt t]],[h]]) (zip tags hexps))

--- Standard rendering of lists as a table with a row for each item:
--- Thus, the elements are vertically aligned.
renderList :: Rendering
renderList hexps = mergeTableOfTable (table (map (\h->[[h]]) hexps))
                                                `addAttr` ("border","1")

mergeTableOfTable :: HtmlExp -> HtmlExp
mergeTableOfTable (HtmlStruct "table" attrs rows) =
  HtmlStruct "table" attrs
             (if all isRowWithSingleTableData rows
              then map mergeRowWithSingleTableData rows
              else rows )

isRowWithSingleTableData row = case row of
   (HtmlStruct "tr" []
        [HtmlStruct "td" []
            [HtmlStruct "table" _ [HtmlStruct "tr" _ _]]]) -> True
   _ -> False

mergeRowWithSingleTableData 
  (HtmlStruct "tr" [] [HtmlStruct "td" [] [HtmlStruct "table" _ [row]]]) = row


-------------------------------------------------------------------------------
-- Main operations to generate HTML structures and handlers from
-- WUI specifications:

--- Generates an HTML form from a WUI data specification,
--- an initial value and an update form.
mainWUI :: WuiSpec a -> a -> (a -> IO HtmlForm) -> IO HtmlForm
mainWUI wuispec val store = do
  let (hexp,whandler) = wui2html wuispec val store
  return $ form "WUI" [hexp, breakline, wuiHandler2button "Submit" whandler]


--- Generates HTML editors and a handler from a WUI data specification,
--- an initial value and an update form.
wui2html :: WuiSpec a -> a -> (a -> IO HtmlForm) -> (HtmlExp,WuiHandler)
wui2html wspec val store = wuiWithErrorForm wspec val store standardErrorForm

--- A standard error form for WUIs.
standardErrorForm :: HtmlExp -> WuiHandler -> IO HtmlForm
standardErrorForm hexp whandler =
  return $ standardForm "Input error"
                        [hexp, wuiHandler2button "Submit" whandler]

--- Puts a WUI into a HTML form containing "holes" for the WUI and the
--- handler.
wuiInForm :: WuiSpec a -> a -> (a -> IO HtmlForm)
             -> (HtmlExp -> WuiHandler -> IO HtmlForm) -> IO HtmlForm
wuiInForm wspec val store userform =
  answerForm (wuiWithErrorForm wspec val store userform)
 where
  answerForm (hexp,whandler) = userform hexp whandler

--- Generates HTML editors and a handler from a WUI data specification,
--- an initial value and an update form. In addition to wui2html,
--- we can provide a skeleton form used to show illegal inputs.
wuiWithErrorForm :: WuiSpec a -> a -> (a -> IO HtmlForm)
                    -> (HtmlExp -> WuiHandler -> IO HtmlForm)
                    -> (HtmlExp,WuiHandler)
wuiWithErrorForm wspec val store errorform =
        showAndReadWUI wspec store errorform (generateWUI wspec val)

generateWUI :: WuiSpec a -> a
               -> (HtmlExp,Maybe JSExp, CgiEnv -> (Maybe a,HtmlState))
generateWUI (WuiSpec wparams showhtml readval) val =
    hst2result (showhtml wparams val)
  where
    hst2result (htmledits,jsfs,wstate) =
      (htmledits, jsfs, \env -> readval wparams env wstate)

showAndReadWUI :: WuiSpec a -> (a -> IO HtmlForm)
                            -> (HtmlExp -> WuiHandler -> IO HtmlForm)
                            -> (HtmlExp,Maybe JSExp,CgiEnv->(Maybe a,HtmlState))
                            -> (HtmlExp,WuiHandler)
showAndReadWUI wspec store errorform (htmledits,jsfs,readenv) =
  (inline [wuiStyleSheet, htmledits], WHandler (htmlhandler wspec) jsfs)
 where
  htmlhandler wui@(WuiSpec wparams _ readval) env =
    let (mbnewval, (htmlerrform,htmlerrjsfs,errwstate)) = readenv env
     in maybe (let (errhexp,errwhdl) =
                      showAndReadWUI wui
                                     store
                                     errorform
                                     (htmlerrform,htmlerrjsfs,
                                      \errenv -> readval wparams errenv errwstate)
               in errorform errhexp errwhdl)
              (\newval -> seq (normalForm newval) -- to strip off unused lvars
                              (store newval))
              mbnewval


--------------------------------------------------------------------------
-- The style sheet used in WUIs:

wuiStyleSheet = styleSheet $
  "\n.wuihide { display: none; }\n" ++
  ".wuinohide { display: inline; color: red; font-weight: bold ; background-color: yellow; }\n" ++
  ".wuipassiveerrmsg { padding: 0px; border-spacing: 0px; }\n"++
  ".wuiactiveerrmsg { padding: 0px; border-spacing: 0px; background-color: yellow; border-width: 2px; border-style: solid; border-color: red; }\n"

--------------------------------------------------------------------------
