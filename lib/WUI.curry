------------------------------------------------------------------------------
--- A library to support the type-oriented construction of Web User Interfaces
--- (WUIs).
---
--- The ideas behind the application and implementation of WUIs are
--- described in a paper that is available via
--- [this web page](http://www.informatik.uni-kiel.de/~pakcs/WUI).
---
--- @author Michael Hanus
--- @version February 2009
------------------------------------------------------------------------------

module WUI(--WuiState,cgiRef2state,state2cgiRef,value2state,state2value,
           --states2state,state2states,altstate2state,state2altstate,
           Rendering,WuiSpec,
           withRendering,withError,withCondition,adaptWSpec,transformWSpec,
           wHidden,wConstant,wInt,
           wString,wStringSize,wRequiredString,wRequiredStringSize,wTextArea,
           wSelect,wSelectInt,wSelectBool,wRadioSelect,wRadioBool,wCheckBool,
           wMultiCheckSelect,
           wPair,wTriple,w4Tuple,w5Tuple,w6Tuple,w7Tuple,w8Tuple,
           w9Tuple,w10Tuple,w11Tuple,w12Tuple,
           wCons2,wCons3,wCons4,wCons5,wCons6,wCons7,wCons8,
           wCons9,wCons10,wCons11,wCons12,wJoinTuple,
           wMaybe,wCheckMaybe,wRadioMaybe,
           wList,wListWithHeadings,wHList,wMatrix,wEither,
           WTree(..),wTree,
           WuiHandler,wuiHandler2button,
           renderTuple,renderTaggedTuple,renderList,
           mainWUI,wui2html,wuiInForm,wuiWithErrorForm)
 where

import HTML
import Read(readNat)
import List(elemIndex)
import Maybe
import Char(isDigit,isSpace)
import ReadShowTerm

infixl 0 `withRendering`
infixl 0 `withError`
infixl 0 `withCondition`

------------------------------------------------------------------------------
--- An internal WUI state is used to maintain the cgi references of the input
--- fields as a structure that corresponds to the structure of the edit data.
data WuiState =
     Ref CgiRef             -- reference to elementary input field
   | Hidden String          -- string representation of a hidden value
   | CompNode [WuiState]    -- composition of trees (substructures)
   | AltNode (Int,WuiState) -- alternative of trees (union of substructures)

cgiRef2state :: CgiRef -> WuiState
cgiRef2state cr = Ref cr

state2cgiRef :: WuiState -> CgiRef
state2cgiRef (Ref cr) = cr

value2state :: _ -> WuiState
value2state v = Hidden (showQTerm v)

state2value :: WuiState -> _
state2value (Hidden s) = readQTerm s

states2state :: [WuiState] -> WuiState
states2state sts = CompNode sts

state2states :: WuiState -> [WuiState]
state2states (CompNode sts) = sts

altstate2state :: (Int,WuiState) -> WuiState
altstate2state alt = AltNode alt

state2altstate :: WuiState -> (Int,WuiState)
state2altstate (AltNode alt) = alt

------------------------------------------------------------------------------
--- A rendering is a function that combines the visualization of components
--- of a data structure into some HTML expression.
type Rendering = [HtmlExp] -> HtmlExp

--- WuiParams specify the parameters of an individual Wui component type:
--- * the standard rendering
--- * an error message shown in case of illegal inputs
--- * a condition to specify legal input values
type WuiParams a = (Rendering, String, a->Bool)

renderOf (render,_,_) = render

errorOf (_,err,_) = err

conditionOf (_,_,c) = c

------------------------------------------------------------------------------
--- The type HtmlSate are values consisting of an HTML expression
--- (usually containing some input elements) and a WUI state containing
--- references to input elements in the HTML expression.

type HtmlState = (HtmlExp,WuiState)

------------------------------------------------------------------------------
--- A handler for a WUI is an event handler for HTML forms possibly with some
--- specific code attached (for future extensions).
data WuiHandler = WHandler HtmlHandler

--- Transform a WUI handler into a submit button with a given label string.
wuiHandler2button :: String -> WuiHandler -> HtmlExp
wuiHandler2button title (WHandler handler) = button title handler

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
withRendering (WuiSpec (_,errmsg,legal) showhtml readvalue) render =
  WuiSpec (render,errmsg,legal) showhtml readvalue


--- Puts a new error message into a WUI specification.
withError :: WuiSpec a -> String -> WuiSpec a
withError (WuiSpec (render,_,legal) showhtml readvalue) errmsg =
  WuiSpec (render,errmsg,legal) showhtml readvalue

--- Puts a new condition into a WUI specification.
withCondition :: WuiSpec a -> (a -> Bool) -> WuiSpec a
withCondition (WuiSpec (render,errmsg,_) showhtml readvalue) legal =
              (WuiSpec (render,errmsg,legal) showhtml readvalue)

--- Transforms a WUI specification from one type to another.
transformWSpec :: (a->b,b->a) -> WuiSpec a -> WuiSpec b
transformWSpec (a2b,b2a) (WuiSpec wparamsa showhtmla readvaluea) =
  WuiSpec (transParam b2a wparamsa)
          (\wparamsb b -> showhtmla (transParam a2b wparamsb) (b2a b))
          (\wparamsb env wst ->
            let (mba,errv) = readvaluea (transParam a2b wparamsb) env wst
             in (maybe Nothing (Just . a2b) mba, errv))
 where
  transParam :: (b->a) -> WuiParams a -> WuiParams b
  transParam toa (render,errmsg,legal) = (render,errmsg,legal . toa)

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
  WuiSpec (head,"?",const True) -- dummy values, not used
          (\_ v -> (hempty, value2state v))
          (\_ _ s -> (Just (state2value s), (hempty,s)))

--- A widget for values that are shown but cannot be modified.
--- The first argument is a mapping of the value into a HTML expression
--- to show this value.
wConstant :: (a->HtmlExp) -> WuiSpec a
wConstant showhtml =
  WuiSpec (head,"?",const True)
          (\wparams v -> ((renderOf wparams) [showhtml v], value2state v))
          (\(render,_,_) _ s -> let v = state2value s in
                                (Just v, (render [showhtml v],s)))

--- A widget for editing integer values.
wInt :: WuiSpec Int
wInt =
  WuiSpec (head,"Illegal integer:",const True)
          (\wparams v -> intWidget (renderOf wparams) (show v))
          (\(render,errmsg,legal) env s ->
            let input = env (state2cgiRef s)
                renderr = renderError render errmsg
             in maybe (Nothing, intWidget renderr input)
                      (\v -> if legal v
                             then (Just v,  intWidget render input)
                             else (Nothing, intWidget renderr input))
                      (readMaybeInt (stripSpaces input)))
 where
  intWidget render s = let ref free in
    (render [textfield ref s `addAttr` ("size","6")], cgiRef2state ref)

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


checkLegalInput :: WuiParams a -> (Rendering -> a -> HtmlState) -> a
                   -> (Maybe a,HtmlState)
checkLegalInput (render,errmsg,legal) value2widget value =
  if legal value
  then (Just value, value2widget render value)
  else (Nothing,    value2widget (renderError render errmsg) value)


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
  WuiSpec (head, "?", const True)
          (\wparams v -> stringWidget (renderOf wparams) v)
          (\wparams env s ->
                checkLegalInput wparams stringWidget
                                (filterStringInput (env (state2cgiRef s))))
 where
  stringWidget render v =
    let ref free in
    (render [foldr (flip addAttr) (textfield ref v) attrs], cgiRef2state ref)

--- A widget for editing string values that are required to be non-empty.
wRequiredString :: WuiSpec String
wRequiredString =
  wString `withError`     "Missing input:"
          `withCondition` (not . null)

--- A widget with a size attribute for editing string values
--- that are required to be non-empty.
wRequiredStringSize :: Int -> WuiSpec String
wRequiredStringSize size =
  wStringSize size `withError`     "Missing input:"
                   `withCondition` (not . null)

--- A widget for editing string values in a text area.
--- The argument specifies the height and width of the text area.
wTextArea :: (Int,Int) -> WuiSpec String
wTextArea dims =
  WuiSpec (head, "?", const True)
          (\wparams v -> textareaWidget (renderOf wparams) v)
          (\wparams env s ->
               checkLegalInput wparams textareaWidget
                                       (filterStringInput (env (state2cgiRef s))))
 where
  textareaWidget render v = let ref free in
                            (render [textarea ref dims v], cgiRef2state ref)


--- A widget to select a value from a given list of values.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into strings to be shown
--- in the selection widget.
wSelect :: (a->String) -> [a] -> WuiSpec a
wSelect showelem selset =
  WuiSpec (head,"?",const True)
          (\wparams v -> selWidget (renderOf wparams) v)
          (\wparams env s ->
             checkLegalInput wparams selWidget
                             (selset !! readNat (env (state2cgiRef s))))
 where
  selWidget render v =
    let ref free
        idx = elemIndex v selset
        namevalues = zip (map showelem selset) (map show [0..])
     in (render [maybe (selection ref namevalues)
                       (\i -> selectionInitial ref namevalues i)
                       idx],
         cgiRef2state ref)

--- A widget to select a value from a given list of integers (provided as
--- the argument).
--- The current value should be contained in the value list and is preselected.
wSelectInt :: [Int] -> WuiSpec Int
wSelectInt = wSelect show

--- A widget to select a Boolean value via a selection box.
--- The arguments are the strings that are shown for the values
--- True and False in the selection box, respectively.
--- @param true - string for selection of True
--- @param false - string for selection of False
--- @return a WUI specification for a Boolean selection widget
wSelectBool :: String -> String -> WuiSpec Bool
wSelectBool true false = wSelect (\b->if b then true else false) [True,False]

--- A widget to select a Boolean value via a check box.
--- The first argument are HTML expressions that are shown after the
--- check box.  The result is True if the box is checked.
wCheckBool :: [HtmlExp] -> WuiSpec Bool
wCheckBool hexps =
  WuiSpec (head, "?", const True)
          (\wparams v -> checkWidget (renderOf wparams) v)
          (\wparams env s ->
             checkLegalInput wparams checkWidget (env (state2cgiRef s)=="True"))
 where
  checkWidget render v = let ref free in
    (render [inline ((if v then checkedbox else checkbox) ref "True" : hexps)],
     cgiRef2state ref)

--- A widget to select a list of values from a given list of values
--- via check boxes.
--- The current values should be contained in the value list and are preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the check box.
wMultiCheckSelect :: (a->[HtmlExp]) -> [a] -> WuiSpec [a]
wMultiCheckSelect showelem selset =
  WuiSpec (renderTuple, tupleError, const True)
          (\wparams vs -> checkWidget (renderOf wparams) vs)
          (\wparams env st ->
             checkLegalInput wparams checkWidget
                   (concatMap (\ (ref,s) -> if env ref=="True" then [s] else [])
                              (zip (map state2cgiRef (state2states st)) selset)))
 where
  checkWidget render vs =
    let refs = take (length selset) newVars
        numsetitems = zip refs selset
        showItem (ref,s) =
           inline ((if s `elem` vs then checkedbox else checkbox)
                                                       ref "True" : showelem s)
     in (render (map showItem numsetitems),
         states2state (map cgiRef2state refs))

newVars = unknown : newVars

--- A widget to select a value from a given list of values via a radio button.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the radio button.
wRadioSelect :: (a->[HtmlExp]) -> [a] -> WuiSpec a
wRadioSelect showelem selset =
  WuiSpec (renderTuple, tupleError, const True)
          (\wparams v -> radioWidget (renderOf wparams) v)
          (\wparams env s ->
             checkLegalInput wparams radioWidget
                             (selset !! readNat (env (state2cgiRef s))))
 where
  radioWidget render v =
    let ref free
        idx = maybe 0 id (elemIndex v selset)
        numhitems = zip [0..] (map showelem selset)
        showItem (i,s) = table [[[(if i==idx then radio_main else radio_other)
                                        ref (show i)],s]]
     in (render (map showItem numhitems),
         cgiRef2state ref)

--- A widget to select a Boolean value via a radio button.
--- The arguments are the lists of HTML expressions that are shown after
--- the True and False radio buttons, respectively.
--- @param true - HTML expressions for True radio button
--- @param false - HTML expressions for False radio button
--- @return a WUI specification for a Boolean selection widget
wRadioBool :: [HtmlExp] -> [HtmlExp] -> WuiSpec Bool
wRadioBool truehexps falsehexps =
  wRadioSelect (\b->if b then truehexps else falsehexps) [True,False]


--- WUI combinator for pairs.
wPair :: WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
wPair = wCons2 (\a b -> (a,b))

--- WUI combinator for constructors of arity 2.
--- The first argument is the binary constructor.
--- The second and third arguments are the WUI specifications
--- for the argument types.
wCons2 :: (a->b->c) -> WuiSpec a -> WuiSpec b -> WuiSpec c
wCons2 cons (WuiSpec rendera showa reada) (WuiSpec renderb showb readb) =
  WuiSpec (renderTuple, tupleError, const True) showc readc
 where
  showc wparams vc | cons va vb =:<= vc =
    let (hea,rta) = showa rendera va
        (heb,rtb) = showb renderb vb
     in ((renderOf wparams) [hea,heb], states2state [rta,rtb])
   where va,vb free

  readc (render,errmsg,legal) env s =
    let [ra,rb] = state2states s
        (rav,(hea,rta)) = reada rendera env ra
        (rbv,(heb,rtb)) = readb renderb env rb
        errhexps = [hea,heb]
        errstate = states2state [rta,rtb]
     in if rav==Nothing || rbv==Nothing
        then (Nothing, (render errhexps, errstate))
        else let value = cons (fromJust rav) (fromJust rbv) in
             if legal value
             then (Just value, (render errhexps, errstate))
             else (Nothing,    (renderError render errmsg errhexps, errstate))


--- WUI combinator for triples.
wTriple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec (a,b,c)
wTriple = wCons3 (\a b c -> (a,b,c))

--- WUI combinator for constructors of arity 3.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons3 :: (a->b->c->d) -> WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d
wCons3 cons (WuiSpec rendera showa reada) (WuiSpec renderb showb readb)
            (WuiSpec renderc showc readc) =
  WuiSpec (renderTuple, tupleError, const True) showd readd
 where
  showd wparams vd | cons va vb vc =:<= vd =
    let (hea,rta) = showa rendera va
        (heb,rtb) = showb renderb vb
        (hec,rtc) = showc renderc vc
     in ((renderOf wparams) [hea,heb,hec], states2state [rta,rtb,rtc])
   where va,vb,vc free

  readd (render,errmsg,legal) env s =
    let [ra,rb,rc] = state2states s
        (rav,(hea,rta)) = reada rendera env ra
        (rbv,(heb,rtb)) = readb renderb env rb
        (rcv,(hec,rtc)) = readc renderc env rc
        errhexps = [hea,heb,hec]
        errstate = states2state [rta,rtb,rtc]
     in if rav==Nothing || rbv==Nothing || rcv==Nothing
        then (Nothing, (render errhexps, errstate))
        else let value = cons (fromJust rav) (fromJust rbv) (fromJust rcv) in
             if legal value
             then (Just value, (render errhexps, errstate))
             else (Nothing,    (renderError render errmsg errhexps, errstate))


--- WUI combinator for tuples of arity 4.
w4Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec (a,b,c,d)
w4Tuple = wCons4 (\a b c d -> (a,b,c,d))

--- WUI combinator for constructors of arity 4.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons4  :: (a->b->c->d->e) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e
wCons4 cons wa wb wc wd =
  adaptWSpec (\ ((a,b),(c,d)) -> cons a b c d)
             (wJoinTuple (wPair wa wb) (wPair wc wd))


--- WUI combinator for tuples of arity 5.
w5Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec (a,b,c,d,e)
w5Tuple = wCons5 (\a b c d e -> (a,b,c,d,e))

--- WUI combinator for constructors of arity 5.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons5  :: (a->b->c->d->e->f) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f
wCons5 cons wa wb wc wd we =
  adaptWSpec (\ ((a,b,c),(d,e)) -> cons a b c d e)
             (wJoinTuple (wTriple wa wb wc) (wPair wd we))


--- WUI combinator for tuples of arity 6.
w6Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec (a,b,c,d,e,f)
w6Tuple = wCons6 (\a b c d e f -> (a,b,c,d,e,f))

--- WUI combinator for constructors of arity 6.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons6  :: (a->b->c->d->e->f->g) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g
wCons6 cons wa wb wc wd we wf =
  adaptWSpec (\ ((a,b,c),(d,e,f)) -> cons a b c d e f)
             (wJoinTuple (wTriple wa wb wc) (wTriple wd we wf))


--- WUI combinator for tuples of arity 7.
w7Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec (a,b,c,d,e,f,g)
w7Tuple = wCons7 (\a b c d e f g -> (a,b,c,d,e,f,g))

--- WUI combinator for constructors of arity 7.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons7  :: (a->b->c->d->e->f->g->h) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h
wCons7 cons wa wb wc wd we wf wg =
  adaptWSpec (\ ((a,b,c,d),(e,f,g)) -> cons a b c d e f g)
             (wJoinTuple (w4Tuple wa wb wc wd) (wTriple we wf wg))


--- WUI combinator for tuples of arity 8.
w8Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec (a,b,c,d,e,f,g,h)
w8Tuple = wCons8 (\a b c d e f g h -> (a,b,c,d,e,f,g,h))

--- WUI combinator for constructors of arity 8.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons8  :: (a->b->c->d->e->f->g->h->i) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i
wCons8 cons wa wb wc wd we wf wg wh =
  adaptWSpec (\ ((a,b,c,d),(e,f,g,h)) -> cons a b c d e f g h)
             (wJoinTuple (w4Tuple wa wb wc wd) (w4Tuple we wf wg wh))


--- WUI combinator for tuples of arity 9.
w9Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i ->
           WuiSpec (a,b,c,d,e,f,g,h,i)
w9Tuple = wCons9 (\a b c d e f g h i -> (a,b,c,d,e,f,g,h,i))

--- WUI combinator for constructors of arity 9.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons9  :: (a->b->c->d->e->f->g->h->i->j) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j
wCons9 cons wa wb wc wd we wf wg wh wi =
  adaptWSpec (\ ((a,b,c,d,e),(f,g,h,i)) -> cons a b c d e f g h i)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w4Tuple wf wg wh wi))


--- WUI combinator for tuples of arity 10.
w10Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec (a,b,c,d,e,f,g,h,i,j)
w10Tuple = wCons10 (\a b c d e f g h i j -> (a,b,c,d,e,f,g,h,i,j))

--- WUI combinator for constructors of arity 10.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons10  :: (a->b->c->d->e->f->g->h->i->j->k) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k
wCons10 cons wa wb wc wd we wf wg wh wi wj =
  adaptWSpec (\ ((a,b,c,d,e),(f,g,h,i,j)) -> cons a b c d e f g h i j)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w5Tuple wf wg wh wi wj))


--- WUI combinator for tuples of arity 11.
w11Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec (a,b,c,d,e,f,g,h,i,j,k)
w11Tuple = wCons11 (\a b c d e f g h i j k -> (a,b,c,d,e,f,g,h,i,j,k))

--- WUI combinator for constructors of arity 11.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons11  :: (a->b->c->d->e->f->g->h->i->j->k->l) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l
wCons11 cons wa wb wc wd we wf wg wh wi wj wk =
  adaptWSpec (\ ((a,b,c,d,e),(f,g,h,i,j,k)) -> cons a b c d e f g h i j k)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w6Tuple wf wg wh wi wj wk))


--- WUI combinator for tuples of arity 12.
w12Tuple :: WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec (a,b,c,d,e,f,g,h,i,j,k,l)
w12Tuple = wCons12 (\a b c d e f g h i j k l -> (a,b,c,d,e,f,g,h,i,j,k,l))

--- WUI combinator for constructors of arity 12.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons12  :: (a->b->c->d->e->f->g->h->i->j->k->l->m) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m
wCons12 cons wa wb wc wd we wf wg wh wi wj wk wl =
  adaptWSpec (\ ((a,b,c,d,e,f),(g,h,i,j,k,l)) -> cons a b c d e f g h i j k l)
       (wJoinTuple (w6Tuple wa wb wc wd we wf) (w6Tuple wg wh wi wj wk wl))


--- WUI combinator to combine two tuples into a joint tuple.
--- It is similar to wPair but renders both components as a single
--- tuple provided that the components are already rendered as tuples,
--- i.e., by the rendering function <code>renderTuple</code>.
--- This combinator is useful to define combinators for large tuples.
wJoinTuple :: WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
wJoinTuple (WuiSpec rendera showa reada) (WuiSpec renderb showb readb) =
  WuiSpec (renderTuple, tupleError, const True) showc readc
 where
  render2joinrender render [h1,h2] =
    let h1s = unRenderTuple h1
        h2s = unRenderTuple h2
     in render (h1s++h2s)

  showc wparams vc | (va,vb) =:<= vc =
    let (hea,rta) = showa rendera va
        (heb,rtb) = showb renderb vb
     in (render2joinrender (renderOf wparams) [hea,heb],states2state [rta,rtb])
   where va,vb free

  readc (orgrender,errmsg,legal) env s =
    let [ra,rb] = state2states s
        (rav,(hea,rta)) = reada rendera env ra
        (rbv,(heb,rtb)) = readb renderb env rb
        errhexps = [hea,heb]
        errstate = states2state [rta,rtb]
        render = render2joinrender orgrender
     in if rav==Nothing || rbv==Nothing
        then (Nothing, (render errhexps, errstate))
        else let value = (fromJust rav, fromJust rbv) in
             if legal value
             then (Just value, (render errhexps, errstate))
             else (Nothing,    (renderError render errmsg errhexps, errstate))


--- WUI combinator for list structures where the list elements are vertically
--- aligned in a table.
wList :: WuiSpec a -> WuiSpec [a]
wList (WuiSpec rendera showa reada) =
  WuiSpec (renderList,"Illegal list:",const True)
          (\wparams vas ->
              listWidget (renderOf wparams) (unzip (map (showa rendera) vas)))
          (\ (render,errmsg,legal) env s ->
            let rvs = map (reada rendera env) (state2states s)
             in if Nothing `elem` (map fst rvs)
                then (Nothing, listWidget render (unzip (map snd rvs)))
                else let value = map (fromJust . fst) rvs in
                     if legal value
                     then (Just value, listWidget render (unzip (map snd rvs)))
                     else (Nothing, listWidget (renderError render errmsg)
                                               (unzip (map snd rvs))) )
 where
  listWidget render (hes,refs) = (render hes, states2state refs)

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
   (renderTuple, tupleError, const True)
   (\wparams mbs ->
     let (heb,rtb) = showb paramb (mbs/=Nothing)
         (hea,rta) = showa parama (maybe def id mbs)
      in ((renderOf wparams) [heb,hea], states2state [rtb,rta]))
   (\ (render,errmsg,legal) env s ->
     let [rb,ra] = state2states s
         (rbv,(heb,rtb)) = readb paramb env rb
         (rav,(hea,rta)) = reada parama env ra
         errhexps = [heb,hea]
         errstate = states2state [rtb,rta]
      in if rbv==Nothing || rav==Nothing
         then (Nothing, (render errhexps, errstate))
         else let value = if fromJust rbv
                          then Just (fromJust rav)
                          else Nothing in
              if legal value
              then (Just value, (render errhexps, errstate))
              else (Nothing,    (renderError render errmsg errhexps, errstate)))

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
 WuiSpec (head, "?", const True) showEither readEither
 where
  showEither wparams (Left va) =
    let (hea,rta) = showa rendera va
     in ((renderOf wparams) [hea], altstate2state (1,rta))
  showEither wparams (Right vb) =
    let (heb,rtb) = showb renderb vb
     in ((renderOf wparams) [heb], altstate2state (2,rtb))

  readEither (render,errmsg,legal) env s =
    let (altindex,rab) = state2altstate s
     in case altindex of
         1 -> let (rv,(he,rst)) = reada rendera env rab
               in checkValue (rv==Nothing) (Left (fromJust rv))
                             he (altstate2state(1,rst))
         2 -> let (rv,(he,rst)) = readb renderb env rab
               in checkValue (rv==Nothing) (Right (fromJust rv))
                             he (altstate2state(2,rst))
   where
     checkValue isnothing value hexp altstate =
        if isnothing
        then (Nothing, (render [hexp], altstate))
        else if legal value
             then (Just value, (render [hexp], altstate))
             else (Nothing,    (renderError render errmsg [hexp], altstate))

--- A simple tree structure to demonstrate the construction of WUIs for tree
--- types.
data WTree a = WLeaf a | WNode [WTree a]

--- WUI for tree types.
--- The rendering specifies the rendering of inner nodes.
--- Leaves are shown with their default rendering.
wTree :: WuiSpec a -> WuiSpec (WTree a)
wTree (WuiSpec rendera showa reada) =
 WuiSpec (renderList, "Illegal tree:", const True) showTree readTree
 where
  showTree _ (WLeaf va) =
    let (hea,rta) = showa rendera va
     in (hea, altstate2state (1,rta))
  showTree wparams (WNode ns) =
    let (hes,sts) = unzip (map (showTree wparams) ns)
     in ((renderOf wparams) hes, altstate2state (2,states2state sts))

  readTree wpar env s =
    let (altindex,rab) = state2altstate s
     in case altindex of
         1 -> let (rv,(he,rst)) = reada rendera env rab
               in checkValue (rv==Nothing) (WLeaf (fromJust rv)) head
                             [he] (altstate2state(1,rst))
         2 -> let rvs = map (readTree wpar env) (state2states rab)
               in checkValue (Nothing `elem`  (map fst rvs))
                             (WNode (map (fromJust . fst) rvs)) (renderOf wpar)
                             (map (fst . snd) rvs)
                          (altstate2state(2,states2state (map (snd . snd) rvs)))
   where
     checkValue isnothing value rendertree hexps altstate =
        if isnothing
        then (Nothing, (rendertree hexps, altstate))
        else if conditionOf wpar value
             then (Just value, (rendertree hexps, altstate))
             else (Nothing,    (renderError rendertree (errorOf wpar) hexps,
                                altstate))


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

-- Combine a rendering with an error message.
-- The error message is put as the first row of a table with background color
-- yellow.
renderError :: Rendering -> String -> Rendering
renderError render errmsg hexps =
  table [[[boldRedTxt errmsg]], [[render hexps]]] 
                  `addAttr` ("bgcolor","#ffff00") -- background color: yellow

boldRedTxt s = HtmlStruct "font" [("color","#ff0000")] [bold [htxt s]]


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
  let (hexp,handler) = wui2html wuispec val store
  return $ form "WUI" [hexp, breakline, wuiHandler2button "Submit" handler]

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

generateWUI :: WuiSpec a -> a -> (HtmlExp, CgiEnv -> (Maybe a,HtmlState))
generateWUI (WuiSpec wparams showhtml readval) val = hst2result (showhtml wparams val)
  where
    hst2result (htmledits,wstate) = (htmledits, \env -> readval wparams env wstate)

showAndReadWUI :: WuiSpec a -> (a -> IO HtmlForm)
                            -> (HtmlExp -> WuiHandler -> IO HtmlForm)
                            -> (HtmlExp,CgiEnv -> (Maybe a,HtmlState))
                            -> (HtmlExp,WuiHandler)
showAndReadWUI wspec store errorform (htmledits,readenv) =
  (htmledits, WHandler (htmlhandler wspec))
 where
  htmlhandler wui@(WuiSpec wparams _ readval) env =
    let (mbnewval, (htmlerrform,errwstate)) = readenv env
     in maybe (let (errhexp,errhdl) =
                      showAndReadWUI wui
                                     store
                                     errorform
                                     (htmlerrform,
                                      \errenv -> readval wparams errenv errwstate)
               in errorform errhexp errhdl)
              (\newval -> seq (normalForm newval) -- to strip off unused lvars
                              (store newval))
              mbnewval


--------------------------------------------------------------------------
