------------------------------------------------------------------------------
--- Library for creating tcl/tk applications from ui descriptions
--- with combinators you can create your own widgets
--- Ideas and parts of documentation from PAKCS WUI library  
--- @author Christof Kluss
--- @version September 2008
------------------------------------------------------------------------------


module TypedUI2GUI (
  Rendering,UIParams,UISpec(UISpec),  
  renderOf,errorOf,conditionOf,
  withRendering,withError,withConditionIO,withCondition,
  --filterStringInput,removeCRs,
  transformWSpec,adaptWSpec,--invert,
  wInt,--stripSpaces,readMaybeInt,
  wConstant,wHidden,
  wStringStyles,wString,wStringSize,wRequiredString,wRequiredStringSize,
  errorStyle,showError,renderError,
  wPair,wTriple,w4Tuple,w5Tuple,w6Tuple,w7Tuple,w8Tuple,w11Tuple,
  wCons2,wCons3,wCons4,wCons5,wCons6,wCons7,wCons8,wCons11,
  wTextArea,wList,renderList,wMultiCheckSelect,newVars,
  wSelect,wSelectInt,wSelectBool,renderTuple,tupleError,
  typedui2ui,typeduistore2ui,runUISpec,
  module UI2GUI  
) where

import Read(readNat)
import List(elemIndex)
import Maybe
import Char(isDigit,isSpace)
import ReadShowTerm
 
import UI2GUI
import IOExts

infixl 0 `withRendering`
infixl 0 `withError`
infixl 0 `withCondition`
infixl 0 `withConditionIO`

------------------------------------------------------------------------------
--- A rendering is a function that combines the visualization of components
--- of a data structure into some HTML expression.
type Rendering = [UIWidget] -> UIWidget

--- UIParams specify the parameters of an individual UI component type:
--- * the standard rendering
--- * an error message shown in case of illegal inputs
--- * a condition to specify legal input values
type UIParams a = (Rendering, String, a -> IO Bool)

renderOf :: (a,_,_) -> a
renderOf (render,_,_) = render

errorOf :: (_,a,_) -> a
errorOf (_,err,_) = err

conditionOf :: (_,_,a) -> a
conditionOf (_,_,c) = c

------------------------------------------------------------------------------
------------------------------------------------------------------------------

--- The type of UI specifications.
--- The first component are parameters specifying the behavior of this UI type
--- (rendering, error message, and constraints on inputs).
--- The second component is a "show" function returning an UI Widget,
--- a "read" function and a "set" function
--- The "read" function extract the values from the Widget.
--- If the value is not legal, Nothing is returned. 
data UISpec a =
  UISpec (UIParams a)
         (UIParams a -> a -> (UIWidget,
	                      UIEnv -> IO (Maybe a),
			      a -> UIEnv -> IO ())) 
	  
------------------------------------------------------------------------------

--- Puts a new rendering function into a UI specification.
withRendering :: UISpec a -> Rendering -> UISpec a
withRendering (UISpec (_,errmsg,legal) show) render =
  UISpec (render,errmsg,legal) show

--- Puts a new error message into a UI specification.
withError :: UISpec a -> String -> UISpec a
withError (UISpec (render,_,legal) show) errmsg =
  UISpec (render,errmsg,legal) show

--- Puts a new condition into a UI specification.
withConditionIO :: UISpec a -> (a -> IO Bool) -> UISpec a
withConditionIO (UISpec (render,errmsg,_) show) legal =
              (UISpec (render,errmsg,legal) show)

--- Puts a new condition into a UI specification.
withCondition :: UISpec a -> (a -> Bool) -> UISpec a
withCondition wspec legal  = 
  withConditionIO wspec (\a -> return $ legal a)

-- A collection of basic UIs and UI combinators:

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

--- Transforms a UI specification from one type to another.
transformWSpec :: (a->b,b->a) -> UISpec a -> UISpec b
transformWSpec (a2b,b2a) (UISpec wparamsa showuia) =
  UISpec (transParam b2a wparamsa)
          (\wparamsb b -> showWidget wparamsb b)         
 where
  showWidget wparamsb b = 
    let (widgetb,reada,seta) = showuia (transParam a2b wparamsb) (b2a b)

        readb env = do mba <- reada env
                       return $ maybe Nothing (Just . a2b) mba	     
        setb val env = seta (b2a val) env 

    in (widgetb,readb,setb)

  transParam :: (b->a) -> UIParams a -> UIParams b
  transParam toa (render,errmsg,legal) = (render,errmsg,legal . toa)

--- Adapt a UI specification to a new type. For this purpose,
--- the first argument must be a transformation mapping values
--- from the old type to the new type. This function must be bijective
--- and operationally invertible (i.e., the inverse must be computable
--- by narrowing). Otherwise, use <code>transformWSpec</code>!
adaptWSpec :: (a->b) -> UISpec a -> UISpec b
adaptWSpec a2b = transformWSpec (a2b,invert a2b)

-- Compute the inverse of a function by exploiting function patterns:
invert :: (a->b) -> b -> a
invert f = f_invert
 where
  local_f x = f x
  --f_invert (local_f x) = x  -- here we use a function pattern
  f_invert y | (local_f x) =:<= y = x where x free -- the same without fun.pat.

-------------------------------------------------------------------------------
hideErrorLabel lref env     = do setValue lref "" env
                                 setVisible lref False env
showErrorLabel lref msg env = do setValue lref msg env
                                 setVisible lref True env 

--- A widget for editing integer values.
wInt :: UISpec Int
wInt =
  UISpec (head,"?",const $ return True)
          (\wparams v -> intWidget wparams v)
 where
  intWidget (render,errmsg,legal) v = (render [widget],readval,setval)
    where
      ref, errorref free

      widget = col [
          labelS [errorStyle, Class [Display False]] "" `setRef` errorref,
          entryS [Class [NameValue "size" "6"]] ref (show v) 
          --`addHandlers` 
          --  [Handler FocusOut (Cmd (\env -> readval env >> done))]
        ]

      readval env = do
        val <- getValue ref env
        let mbn = readMaybeInt (stripSpaces val)
        if mbn == Nothing 
          then do showErrorLabel errorref "Illegal integer:" env 
                  return Nothing
          else do
            b <- legal (fromJust mbn)
            if b then do hideErrorLabel errorref env
                         return mbn
                 else do showErrorLabel errorref errmsg env
                         return Nothing 	

      setval val env = do hideErrorLabel errorref env
                          setValue ref (show val) env

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

--- A widget for values that are shown but cannot be modified.
--- The first argument is a mapping of the value into a UI Widget 
--- to show this value.
wConstant :: (a->UIWidget) -> UISpec a
wConstant showui =
  UISpec (head,"?",const $ return True)
          (\wparams v -> ((renderOf wparams) [showui v],
                          const $ return (Just v),
                          \ _ _ -> done ))

--- A hidden widget for a value that is not shown in the UI.
--- Usually, this is used in components of larger
--- structures, e.g., internal identifiers, data base keys.
wHidden :: UISpec a
wHidden =
  UISpec (head,"?",const $ return True) -- dummy values, not used
          (\_ v -> (label " ",const $ return (Just v),\ _ _ -> done))

--- A widget for editing string values 
wStringStyles :: [StyleClass] -> UISpec (String)
wStringStyles styles =
  UISpec (head, "?", const $ return True)
          (\wparams v -> stringWidget wparams v) 
 where
  stringWidget (render,errmsg,legal) v = (render [widget],readval,setval)
    where    
      ref, errorref free

      widget = col [
          labelS [errorStyle] "" `setRef` errorref,
          entryS styles ref v 
          --`addHandlers` 
	  --  [Handler FocusOut (Cmd (\env -> readval env >> done))]
        ]

      readval env = do
        val <- getValue ref env 
        b <- legal val
        if b then do hideErrorLabel errorref env
                     return (Just val)
             else do showErrorLabel errorref errmsg env
                     return Nothing

      setval val env = do hideErrorLabel errorref env
                          setValue ref val env

--- A widget for editing string values 
wString :: UISpec (String)
wString = wStringStyles []

--- A widget for editing string values 
wStringSize :: Int -> UISpec (String)
wStringSize size = wStringStyles [Class [NameValue "size" (show size)]]


--- A widget for editing string values that are required to be non-empty.
wRequiredString :: UISpec String
wRequiredString =
  wString `withError`     "Missing input:"
          `withCondition` (\s -> (not . null) s)

--- A widget for editing string values that are required to be non-empty.
wRequiredStringSize :: Int -> UISpec (String)
wRequiredStringSize size =
  wStringSize size `withError`     "Missing input:"
                   `withCondition` (\s -> (not . null) s)

-------------------------------------------------------------------------------

data ErrorRefs = ErrorRefs (UIRef,UIRef)

errorStyle :: StyleClass
errorStyle = Class [Fg Red, Font Bold]

showError :: ErrorRefs -> Maybe String -> UIEnv -> IO ()
showError errorref mberrmsg env | 
  ErrorRefs (labelref,colref) =:= errorref =
  case mberrmsg of 
    Just errmsg -> do showErrorLabel labelref errmsg env
                      setErrorBg labelref True env
                      setErrorBg colref True env
    Nothing     -> do hideErrorLabel labelref env
                      setErrorBg labelref False env
                      setErrorBg colref False env 
  where labelref,colref free

renderError :: UIWidget -> ErrorRefs -> UIWidget
renderError widget errorref | ErrorRefs (labelref,colref) =:= errorref =
        col [labelS  [Class [Fg Red, Font Bold, Display False]] ""
                `setRef` labelref,
             widget] `setRef` colref
  where labelref,colref free

-------------------------------------------------------------------------------

--- WUI combinator for pairs.
wPair :: (Eq a, Eq b) => UISpec a -> UISpec b -> UISpec ((a,b))
wPair = wCons2 (\a b -> (a,b))

--- WUI combinator for triples.
wTriple :: (Eq a, Eq b, Eq c) =>
           UISpec a -> UISpec b -> UISpec c -> UISpec (a,b,c)
wTriple = wCons3 (\a b c -> (a,b,c))

--- WUI combinator for tuples of arity 4.
w4Tuple :: (Eq a, Eq b, Eq c, Eq d) =>
           UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec (a,b,c,d)
w4Tuple = wCons4 (\a b c d -> (a,b,c,d))

--- WUI combinator for tuples of arity 5.
w5Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
           UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e ->
           UISpec (a,b,c,d,e)
w5Tuple = wCons5 (\a b c d e -> (a,b,c,d,e))

--- WUI combinator for tuples of arity 6.
w6Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
           UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e ->
           UISpec f -> UISpec (a,b,c,d,e,f)
w6Tuple = wCons6 (\a b c d e f -> (a,b,c,d,e,f))

--- WUI combinator for tuples of arity 7.
w7Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
           UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e ->
           UISpec f -> UISpec g -> UISpec (a,b,c,d,e,f,g)
w7Tuple = wCons7 (\a b c d e f g -> (a,b,c,d,e,f,g))

--- WUI combinator for tuples of arity 8.
w8Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
           UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e ->
           UISpec f -> UISpec g -> UISpec h -> UISpec (a,b,c,d,e,f,g,h)
w8Tuple = wCons8 (\a b c d e f g h -> (a,b,c,d,e,f,g,h))

--- WUI combinator for tuples of arity 11.
w11Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
            UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e ->
            UISpec f -> UISpec g -> UISpec h -> UISpec i -> UISpec j ->
            UISpec k -> UISpec (a,b,c,d,e,f,g,h,i,j,k)
w11Tuple = wCons11 (\a b c d e f g h i j k -> (a,b,c,d,e,f,g,h,i,j,k))


--- UI combinator for constructors of arity 2.
--- The first argument is the binary constructor.
--- The second and third arguments are the UI specifications
--- for the argument types.

wCons2 :: (Eq a, Eq b) => (a -> b -> c) -> UISpec a -> UISpec b -> UISpec c
wCons2 cons (UISpec rendera showa ) (UISpec renderb showb ) =  
  UISpec (renderTuple, tupleError, const $ return True) showc 
 where
  showc  (render,errmsg,legal) vc | cons va vb =:<= vc =
     (renderError (render [hea,heb]) errorref,readc,setc)
   where
     va,vb,errorref free
     (hea,reada,seta) = showa rendera va
     (heb,readb,setb) = showb renderb vb

     readc env = do
       rav <- reada env
       rbv <- readb env

       if rav==Nothing || rbv==Nothing
         then do showError errorref Nothing env
                 return Nothing
         else do
           let val = cons (fromJust rav) (fromJust rbv)
           b <- legal val
           if b
            then do showError errorref Nothing env
                    return (Just val)
            else do showError errorref (Just errmsg) env
                    return Nothing

     setc nvc env | cons nva nvb =:<= nvc = do
         showError errorref Nothing env
         seta nva env
         setb nvb env
       where nva, nvb free

--- A widget for editing string values in a text area.
--- The argument specifies the height and width of the text area.
wTextArea :: (Int,Int) -> UISpec String
wTextArea (rows,cols) = UISpec (head, "?", const $ return True)
                               (\wparams v -> textareaWidget wparams v)
 where
   textareaWidget (render,errmsg,legal) v = ((render [widget]),readval,setval)
    where
      ref, errorlabel free 
      widget = row [labelS [errorStyle] "" `setRef` errorlabel,
                    textEdit ref v rows cols 
                      `addHandlers` [Handler FocusOut (Cmd cmd)]]
        where cmd env = readval env >> done

      readval env = do
        val <- getValue ref env
        b <- legal val
        if b
          then do setValue errorlabel "" env
                  return (Just val)
          else do setValue errorlabel errmsg env 
                  return Nothing

      setval val env = do setValue errorlabel "" env
                          setValue ref val env


wList :: Eq a => UISpec a -> UISpec [a]
wList (UISpec rendera showa) =
  UISpec (renderList,"Illegal list:",const $ return True)
          (\wparams vas ->
              (listWidget wparams (unzip3 (map (showa rendera) vas))))
    where
      listWidget (render,errmsg,legal) (hes,readvals,setvals) = 
        (renderError (render hes) errorref, readval, setval)
        where
          errorref free

          readval env = do
            mbvals <- mapIO (\r -> r env) readvals

            if foldl (||) False (map (\v -> v == Nothing) mbvals) 	
              then do showError errorref Nothing env
                      return Nothing
              else do let value = (map fromJust mbvals) 
                      b <- legal value
                      if b
                        then do showError errorref Nothing env 
                                return (Just value)
                        else do showError errorref (Just errmsg) env
                                return Nothing

          setval vals env = do 
            showError errorref Nothing env	
            mapIO_ (\ (set,val) -> set val env)  (zip setvals vals)

-- standard rendering of lists 
renderList :: Rendering
renderList = col

--- A widget to select a list of values from a given list of values
--- via check boxes.
--- The current values should be contained in the value list and are 
--- preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the check box.
wMultiCheckSelect :: Eq a => (a->[UIWidget]) -> [a] -> UISpec [a]
wMultiCheckSelect showelem selset =
  UISpec (renderTuple, tupleError, const $ return True)
          (\wparams vs -> checkWidget wparams vs)
 where
  refs = take (length selset) newVars

  checkWidget (render,errmsg,legal) vs = 
     (renderError (render (map showItem numsetitems))
                  errorref,readval,setval)
    where
      errorref free

      numsetitems = zip refs selset
      showItem (ref,s) = 
        row ((simpleCheckButton ref "" (s `elem` vs)):(showelem s))

      readval env = do
        vals <- mapIO (\r -> getValue r env) refs
        let selected = concatMap (\ (sel,s) -> if sel == "1" then [s] else []) 
                        (zip vals selset) 

        b <- legal selected
        if b then do showError errorref Nothing env 
                     return (Just selected)
             else do showError errorref (Just errmsg) env 
                     return Nothing

      setval vals env = do
        showError errorref Nothing env 
        mapIO_ (\ref -> setValue ref "0" env) refs
        mapIO_ (\val -> do let mbidx = elemIndex val selset
                           maybe (done) 
                                 (\n -> setValue (refs!!n) "1" env) 
                                 mbidx) vals

newVars :: [_]
newVars = unknown : newVars


--- A widget to select a value from a given list of values.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into strings to be shown
--- in the selection widget.
wSelect :: Eq a => (a->String) -> [a] -> UISpec a
wSelect showelem selset =
  UISpec (head,"?",const $ return True)
         (\wparams v -> selWidget wparams v)
 where
  selWidget (render,_,_) v = ((render [maybe (UI2GUI.selection ref items)
                               (\i -> UI2GUI.selectionInitial ref items i)
                               idx]),readval,setval)
    where
      ref free
      idx = elemIndex v selset      
      items = (map showelem selset) 

      readval env = do val <- getValue ref env
                       return (Just (selset!!(readNat val)))


      setval val env = do let midx = elemIndex val selset
                          maybe (done) (\n -> setValue ref (show n) env) midx

--- A widget to select a value from a given list of integers (provided as
--- the argument).
--- The current value should be contained in the value list and is preselected.
wSelectInt :: [Int] -> UISpec Int
wSelectInt = wSelect show


--- A widget to select a Boolean value via a selection box.
--- The arguments are the strings that are shown for the values
--- True and False in the selection box, respectively.
--- @param true - string for selection of True
--- @param false - string for selection of False
--- @return a UI specification for a Boolean selection widget
wSelectBool :: String -> String -> UISpec Bool
wSelectBool true false = wSelect (\b->if b then true else false) [True,False]

-------------------------------------------------------------------------------

-- Definition of standard rendering functions

--- standard rendering of tuples as a row:
renderTuple :: Rendering
renderTuple = row

-- Standard error message for tuples:
tupleError :: String
tupleError = "Illegal combination:"

-------------------------------------------------------------------------------
-- Main operations to generate UI structures and handlers from
-- UI specifications:

--- Generates an UI Widget and a handlers from a UI data specification,
--- an initial value.
typedui2ui :: UISpec a -> a -> 
  (UIWidget,UIEnv -> IO (Maybe a),a -> UIEnv -> IO (),(a -> a) -> UIEnv ->
                                                                         IO ())
typedui2ui (UISpec wparams show) val = (widget,getval,setval,updval)
  where
    (widget,getval,setval) = show wparams val
    updval upd env = do mbn <- getval env
                        maybe (done) (\n -> setval (upd n) env) mbn 

--- Generates HTML editors and a handler from a WUI data specification,
--- an initial value and an update form.
typeduistore2ui :: UISpec a -> a -> (a -> UIEnv -> IO ()) -> 
                                                    (UIWidget,UIEnv -> IO ())
typeduistore2ui (UISpec wparams show) val store = (ui,handler)
  where
    (ui,getval,_) = show wparams val 
    handler env = do mbval <- getval env
                     maybe (done) (\v -> store v env) mbval

--runUISpec :: UISpec a -> a -> (a -> UIEnv -> IO ()) -> IO ()
runUISpec uispec val store = do
  let (ui,read) = typeduistore2ui uispec val store
  runUI "WUI" (col [ui, button read "Submit"])

------------------------------------------------------------------------------

wCons3 :: (Eq a, Eq b, Eq c) =>
          (a->b->c->d) -> UISpec a -> UISpec b -> UISpec c -> UISpec d
wCons3 cons (UISpec rendera showa) (UISpec renderb showb)
            (UISpec renderc showc) =
  UISpec (renderTuple, tupleError, const $ return True) showd 
 where
  showd (render,errmsg,legal) vd | cons va vb vc =:<= vd =
    (renderError (render [hea,heb,hec]) errorref,readd,setd)
   where
     va,vb,vc free
     errorref free

     (hea,reada,seta) = showa rendera va
     (heb,readb,setb) = showb renderb vb
     (hec,readc,setc) = showc renderc vc   

     readd env = do
        rav <- reada env 
        rbv <- readb env 
        rcv <- readc env 

        if rav==Nothing || rbv==Nothing || rcv==Nothing 
          then do showError errorref Nothing env
                  return Nothing
          else do
            let value = cons (fromJust rav) (fromJust rbv) (fromJust rcv)
            b <- legal value
            if b
              then do showError errorref Nothing env 
                      return (Just value)
              else do showError errorref (Just errmsg) env 
                      return Nothing 

     setd nvd env | cons nva nvb nvc =:<= nvd = do
         showError errorref Nothing env
         seta nva env
         setb nvb env
         setc nvc env
       where nva, nvb, nvc free


wCons4  :: (Eq a, Eq b, Eq c, Eq d) =>
           (a->b->c->d->e) ->
            UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e
wCons4 cons
        (UISpec wparama showa) (UISpec wparamb showb)
        (UISpec wparamc showc) (UISpec wparamd showd) =
  UISpec (renderTuple,tupleError, const $ return True) showe
 where
  showe (render,errmsg,legal) ve | cons va vb vc vd =:<= ve =
     (renderError (render [hea,heb,hec,hed]) errorref,reade,sete)
    where 
      va,vb,vc,vd free
      errorref free
      (hea,reada,seta) = showa wparama va
      (heb,readb,setb) = showb wparamb vb
      (hec,readc,setc) = showc wparamc vc
      (hed,readd,setd) = showd wparamd vd

      reade env = do
        rav <- reada env
        rbv <- readb env
        rcv <- readc env
        rdv <- readd env

        if rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing
         then do showError errorref Nothing env
                 return Nothing
         else do
           let value = cons (fromJust rav) (fromJust rbv) 
                            (fromJust rcv) (fromJust rdv) 
           b <- legal value
           if b then do showError errorref Nothing env
                        return (Just value)
                else do showError errorref (Just errmsg) env
                        return Nothing

      sete nve env | cons nva nvb nvc nvd =:<= nve = do
         showError errorref Nothing env  
         seta nva env
         setb nvb env
         setc nvc env
         setd nvd env
       where nva, nvb, nvc, nvd free 

wCons5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
          (a -> b -> c -> d -> e -> f) -> UISpec a -> UISpec b -> UISpec c ->
             UISpec d -> UISpec e -> UISpec f
wCons5 cons (UISpec wparama showa) (UISpec wparamb showb)
            (UISpec wparamc showc) (UISpec wparamd showd)
            (UISpec wparame showe) 
 = UISpec (renderTuple,tupleError, const $ return True) showh
 where
  showh (render,errmsg,legal) vf | cons va vb vc vd ve =:<= vf =
     (renderError (render [hea,heb,hec,hed,hee]) errorref,readf,setf)
    where 
      va,vb,vc,vd,ve free
      errorref free
      (hea,reada,seta) = showa wparama va
      (heb,readb,setb) = showb wparamb vb
      (hec,readc,setc) = showc wparamc vc
      (hed,readd,setd) = showd wparamd vd
      (hee,reade,sete) = showe wparame ve

      readf env = do
        rav <- reada env
        rbv <- readb env
        rcv <- readc env
        rdv <- readd env
        rev <- reade env

        if rav==Nothing || rbv==Nothing || rcv==Nothing || 
           rdv==Nothing || rev==Nothing 
          then do showError errorref Nothing env
                  return Nothing
          else do
            let value = cons (fromJust rav) (fromJust rbv) (fromJust rcv)
                             (fromJust rdv) (fromJust rev)
            b <- legal value
            if b then do showError errorref Nothing env 
                         return (Just value)
                 else do showError errorref (Just errmsg) env 
                         return Nothing

      setf nvf env | cons nva nvb nvc nvd nve =:<= nvf = do
         showError errorref Nothing env
         seta nva env
         setb nvb env
         setc nvc env
         setd nvd env
         sete nve env
       where nva, nvb, nvc, nvd, nve free 

wCons6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
          (a -> b -> c -> d -> e -> f -> g) -> UISpec a -> UISpec b ->
              UISpec c -> UISpec d -> UISpec e -> UISpec f -> UISpec g
wCons6 cons
        (UISpec wparama showa) (UISpec wparamb showb)
        (UISpec wparamc showc) (UISpec wparamd showd)
        (UISpec wparame showe) (UISpec wparamf showf) =
  UISpec (renderTuple,tupleError, const $ return True) showh
 where
  showh (render,errmsg,legal) vg | cons va vb vc vd ve vf =:<= vg =
     (renderError (render [hea,heb,hec,hed,hee,hef]) errorref,readg,setg)
    where 
      va,vb,vc,vd,ve,vf free
      errorref free
      (hea,reada,seta) = showa wparama va
      (heb,readb,setb) = showb wparamb vb
      (hec,readc,setc) = showc wparamc vc
      (hed,readd,setd) = showd wparamd vd
      (hee,reade,sete) = showe wparame ve
      (hef,readf,setf) = showf wparamf vf

      readg env = do
        rav <- reada env
        rbv <- readb env
        rcv <- readc env
        rdv <- readd env
        rev <- reade env
        rfv <- readf env

        if rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
           rev==Nothing || rfv==Nothing  

         then do showError errorref Nothing env
                 return Nothing
         else do
           let value = cons (fromJust rav) (fromJust rbv)
                            (fromJust rcv) (fromJust rdv)
                            (fromJust rev) (fromJust rfv)
           b <- legal value
           if b then do showError errorref Nothing env
                        return (Just value)
                else do showError errorref (Just errmsg) env 
                        return Nothing

      setg nvg env | cons nva nvb nvc nvd nve nvf =:<= nvg = do
         showError errorref Nothing env  
         seta nva env
         setb nvb env
         setc nvc env
         setd nvd env
         sete nve env
         setf nvf env
       where nva, nvb, nvc, nvd, nve, nvf free 

wCons7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
          (a -> b -> c -> d -> e -> f -> g -> h) -> UISpec a -> UISpec b ->
        UISpec c -> UISpec d -> UISpec e -> UISpec f -> UISpec g -> UISpec h
wCons7 cons
        (UISpec wparama showa) (UISpec wparamb showb)
        (UISpec wparamc showc) (UISpec wparamd showd) 
        (UISpec wparame showe) (UISpec wparamf showf) 
        (UISpec wparamg showg) 
 = UISpec (renderTuple,tupleError, const $ return True) showh
 where
  showh (render,errmsg,legal) vh | cons va vb vc vd ve vf vg =:<= vh =
     (renderError (render [hea,heb,hec,hed,hee,hef,heg]) errorref,readh,seth)
    where 
      va,vb,vc,vd,ve,vf,vg free
      errorref free
      (hea,reada,seta) = showa wparama va
      (heb,readb,setb) = showb wparamb vb
      (hec,readc,setc) = showc wparamc vc
      (hed,readd,setd) = showd wparamd vd
      (hee,reade,sete) = showe wparame ve
      (hef,readf,setf) = showf wparamf vf
      (heg,readg,setg) = showg wparamg vg

      readh env = do
        rav <- reada env
        rbv <- readb env
        rcv <- readc env
        rdv <- readd env
        rev <- reade env
        rfv <- readf env
        rgv <- readg env

        if rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
           rev==Nothing || rfv==Nothing || rgv==Nothing 
          then do showError errorref Nothing env
                  return Nothing
          else do
            let value = cons (fromJust rav) (fromJust rbv) (fromJust rcv)
                             (fromJust rdv) (fromJust rev) (fromJust rfv)
                             (fromJust rgv)
            b <- legal value
            if b then do showError errorref Nothing env
                         return (Just value)
                 else do showError errorref (Just errmsg) env 
                         return Nothing

      seth nvh env | cons nva nvb nvc nvd nve nvf nvg =:<= nvh = do
         showError errorref Nothing env
         seta nva env
         setb nvb env
         setc nvc env
         setd nvd env
         sete nve env
         setf nvf env
         setg nvg env
       where nva, nvb, nvc, nvd, nve, nvf, nvg free 


wCons8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
          (a -> b -> c -> d -> e -> f -> g -> h -> i) -> UISpec a -> 
   UISpec b -> UISpec c -> UISpec d -> UISpec e -> UISpec f -> UISpec g ->
   UISpec h -> UISpec i
wCons8 cons
        (UISpec wparama showa) (UISpec wparamb showb)
        (UISpec wparamc showc) (UISpec wparamd showd)
        (UISpec wparame showe) (UISpec wparamf showf)
        (UISpec wparamg showg) (UISpec wparamh showh)
      =
  UISpec (renderTuple,tupleError, const $ return True) showi
 where
  showi (render,errmsg,legal) vi | cons va vb vc vd ve vf vg vh =:<= vi =
     (renderError (render [hea,heb,hec,hed,hee,hef,heg,heh])
      errorref,readi,seti)
    where 
      va,vb,vc,vd,ve,vf,vg,vh free
      errorref free
      (hea,reada,seta) = showa wparama va
      (heb,readb,setb) = showb wparamb vb
      (hec,readc,setc) = showc wparamc vc
      (hed,readd,setd) = showd wparamd vd
      (hee,reade,sete) = showe wparame ve
      (hef,readf,setf) = showf wparamf vf
      (heg,readg,setg) = showg wparamg vg
      (heh,readh,seth) = showh wparamh vh

      readi env = do
        rav <- reada env
        rbv <- readb env
        rcv <- readc env
        rdv <- readd env
        rev <- reade env
        rfv <- readf env
        rgv <- readg env
        rhv <- readh env

        if rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
           rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing 
          then do showError errorref Nothing env
                  return Nothing
          else do
            let value = cons (fromJust rav) (fromJust rbv) (fromJust rcv)
                             (fromJust rdv) (fromJust rev) (fromJust rfv)
                             (fromJust rgv) (fromJust rhv)
            b <- legal value
            if b then do showError errorref Nothing env
                         return (Just value)
                 else do showError errorref (Just errmsg) env
                         return Nothing

      seti nvi env | cons nva nvb nvc nvd nve nvf nvg nvh =:<= nvi = do
         showError errorref Nothing env
         seta nva env
         setb nvb env
         setc nvc env
         setd nvd env
         sete nve env
         setf nvf env
         setg nvg env
         seth nvh env
       where nva, nvb, nvc, nvd, nve, nvf, nvg, nvh free 


wCons11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
           (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) ->
            UISpec a -> UISpec b -> UISpec c -> UISpec d -> UISpec e ->
            UISpec f -> UISpec g -> UISpec h -> UISpec i -> UISpec j ->
            UISpec k -> UISpec l
wCons11 cons
        (UISpec wparama showa) (UISpec wparamb showb) (UISpec wparamc showc)
        (UISpec wparamd showd) (UISpec wparame showe) (UISpec wparamf showf)
        (UISpec wparamg showg) (UISpec wparamh showh) (UISpec wparami showi)
        (UISpec wparamj showj) (UISpec wparamk showk)
  = UISpec (renderTuple,tupleError, const $ return True) showl
 where
  showl (render,errmsg,legal) vl |
         cons va vb vc vd ve vf vg vh vi vj vk =:<= vl =
     (renderError (render [hea,heb,hec,hed,hee,hef,heg,heh,hei,hej,hek])
                  errorref,readl,setl)
    where 
      va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk free
      errorref free
      (hea,reada,seta) = showa wparama va
      (heb,readb,setb) = showb wparamb vb
      (hec,readc,setc) = showc wparamc vc
      (hed,readd,setd) = showd wparamd vd
      (hee,reade,sete) = showe wparame ve
      (hef,readf,setf) = showf wparamf vf
      (heg,readg,setg) = showg wparamg vg
      (heh,readh,seth) = showh wparamh vh
      (hei,readi,seti) = showi wparami vi
      (hej,readj,setj) = showj wparamj vj
      (hek,readk,setk) = showk wparamk vk

      readl env = do
        rav <- reada env
        rbv <- readb env
        rcv <- readc env
        rdv <- readd env
        rev <- reade env
        rfv <- readf env
        rgv <- readg env
        rhv <- readh env
        riv <- readi env
        rjv <- readj env
        rkv <- readk env

        if rav==Nothing || rbv==Nothing || rcv==Nothing || rdv==Nothing ||
           rev==Nothing || rfv==Nothing || rgv==Nothing || rhv==Nothing ||
           riv==Nothing || rjv==Nothing || rkv==Nothing 
         then do showError errorref Nothing env
                 return Nothing
         else do
           let value = cons (fromJust rav) (fromJust rbv) (fromJust rcv)
                            (fromJust rdv) (fromJust rev) (fromJust rfv)
                            (fromJust rgv) (fromJust rhv) (fromJust riv)
                            (fromJust rjv) (fromJust rkv)
           b <- legal value
           if b then do showError errorref Nothing env 
                        return (Just value)
                else do showError errorref (Just errmsg) env 
                        return Nothing

      setl nvl env |
        cons nva nvb nvc nvd nve nvf nvg nvh nvi nvj nvk =:<= nvl = do
        showError errorref Nothing env
        seta nva env
        setb nvb env
        setc nvc env
        setd nvd env
        sete nve env
        setf nvf env
        setg nvg env
        seth nvh env
        seti nvi env
        setj nvj env
        setk nvk env
       where nva, nvb, nvc, nvd, nve, nvf, nvg, nvh, nvi, nvj, nvk free 
