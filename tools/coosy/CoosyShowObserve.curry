module CoosyShowObserve where

import CoosyTrace
import List(intersperse,isPrefixOf)
import CoosyPrettier as Prettier
import Directory

main = readAndPrintEvents putStr ShowLogVarBinds

-- Read trace file and print it with parameter function:
readAndPrintEvents :: (String -> IO ()) -> ViewConf -> IO ()
readAndPrintEvents outstr viewConf =
  getDirectoryContents logDir >>= \files ->
  mapIO_ (\label -> readFile (logFile label) >>= \eventStr ->
                    printCatEvents outstr viewConf (label,readTrace eventStr))
         (eventLabels files)

eventLabels files =
    let eventFiles = filter (isPrefixOf logFilePrefix) files
        initialLength = length logFilePrefix in
      map (drop initialLength) eventFiles


printCatEvents :: (String -> IO ()) -> ViewConf -> (String,Trace)
                  -> IO ()
printCatEvents outstr viewConf (label,events) = 
    outstr label >> outstr "\n" >>
    outstr (take (length label) (repeat '-')) >>
    mapIO_ (\t -> outstr "\n" >>
                  mapIO_ (\s -> outstr s >> outstr "\n")
                         ((reverse . (map (prettyShowTerm viewConf))) t))
           (catTerms events) >>
    outstr "\n"

catTerms :: Trace -> [[Term]]
catTerms events = 
    let traces =  map reverse $ (startSeparation2 $ makeevent events)
        termTraces = categoriseBy (getRef . head) traces
    in
      map ((map term) . repairOrder . snd)  termTraces

makeevent event = zip (reverse event) (repeat False)

data Term = Cons String [Term]
          | FunTerm [(Term,Term)]
          | LogVarTerm Term
          | UnEval
          | Bottom
          | Black Term
 
startSeparation :: [(Event,Bool)] -> [Trace]
startSeparation [] = []
startSeparation ((_,True):ts) = startSeparation ts
startSeparation ((t,False):ts) 
  = let 
      (newts,termEvent) = separate [getParent t, getPred t] ts
    in (t:termEvent):startSeparation newts
  
separate :: [EventID] -> [(Event,Bool)] -> ([(Event,Bool)],Trace)
separate _ [] = ([],[])
separate toFind ((dv,flag):ts)
  | elem (getRef dv) toFind
  = let 
      newToFind = getParent dv : getPred dv : filter (/=(getRef dv)) toFind
      (newts,termEvent) = separate newToFind ts 
    in ((dv,True):newts,dv:termEvent)
  | otherwise 
  = let 
      (newts,termEvent) = separate toFind ts 
    in ((dv,flag):newts,termEvent) 

startSeparation2 [] = []
startSeparation2 ((_,True):trace) = startSeparation2 trace
startSeparation2 ((event,False):trace) 
  = let 
      (termTrace,restTrace) = separate' False [getParent event, getPred event] 
                                              trace
    in (event:termTrace):startSeparation2 restTrace
  
separate' tokenSet toFind ((event,flag):trace)
  | found && getParent event == (-1)
  = let 
      tokenTrace = if not tokenSet  
                      then [Separator,event]
                      else [event]
    in (tokenTrace,(event,True):trace)
  | found
  = let 
      newToFind = getParent event:getPred event:filter (/=(getRef event)) toFind
      (termTrace,restTrace) = separate' flag newToFind trace
      tokenTrace = if not tokenSet && flag 
                      then (Separator:event:termTrace) 
                      else (event:termTrace)
    in (tokenTrace,(event,True):restTrace)
  | otherwise 
  = let 
      (termTrace,restTrace) = separate' tokenSet toFind trace
    in (termTrace,(event,flag):restTrace) 
  where
    found = elem (getRef event) toFind 

startSeparation3 [] = []
startSeparation3 ((_,True):trace) = startSeparation3 trace
startSeparation3 ((event,False):trace) 
  = let 
      (termTrace,restTrace) = separate3 False [getRef event] ((event,False):trace)
    in termTrace ++ startSeparation3 restTrace

separate3 tokenSet toFind ((event,flag):trace)
  | found && getParent event == (-1)
  = let 
      tokenTrace = if not tokenSet  
                      then [[Separator,event]]
                      else [[event]]
    in (tokenTrace,(event,True):trace)
  | found
  = let 
      newToFind = getParent event:getPred event:filter (/=(getRef event)) toFind
      (termTrace:termTraces,restTrace) = separate3 flag newToFind trace
      tokenTrace = if not tokenSet && flag 
                      then (Separator:event:termTrace)
                      else (event:termTrace)
    in (tokenTrace:termTraces,(event,True):restTrace)
  | flag
  = let 
      (termTraces,restTrace) = separate3 tokenSet toFind trace
    in (termTraces,(event,flag):restTrace) 
  | otherwise
  = let 
      (termTrace1:termTraces1,restTrace1) = separate3 False [getParent event,getPred event] 
                                          trace
      (termTraces2,restTrace2) = separate3 tokenSet toFind restTrace1
    in (termTraces2++(event:termTrace1):termTraces1,(event,True):restTrace2) 
  where
    found = elem (getRef event) toFind 

startSeparation4 [] = []
startSeparation4 (event:trace) 
  = let 
      (termTrace,restTrace) = separate4 [getParent event, getPred event] trace
    in (event:termTrace):startSeparation4 restTrace

separate4 _ [] = ([],[])
separate4 toFind (event:trace)
  | elem (getRef event) toFind 
  = let 
      newToFind = getParent event:getPred event:filter (/=(getRef event)) toFind
      (termTrace,restTrace) = separate4 newToFind trace
    in (event:termTrace,restTrace)
  | otherwise 
  = let 
      (termTrace,restTrace) = separate4 toFind trace
    in (termTrace,event:restTrace) 
  

repairOrder []  = []
repairOrder [x] = [x]
repairOrder (t1:t2:traces) 
  = (newGrey1++Separator:newBlack1) :
      repairOrder ((newGrey2++Separator:newBlack2):traces)
  where
    (grey1,_:black1) = span (/= Separator) t1
    (grey2,_:black2) = span (/= Separator) t2
    (newGrey1,newBlack1) = splitAt (length grey2) (grey1++black1)
    (newGrey2,newBlack2) = splitAt (length grey1) (grey2++black2)

term :: Trace -> Term
term events = constructTermD (-1) 0 events

constructTermD :: EventID -> EventID -> Trace -> Term
constructTermD _ _ [] = UnEval
constructTermD parentRef seekArgNr (Demand argNr ref parent _:events) 
   | argNr == seekArgNr && parent == parentRef
   =  constructTermV ref events
   | otherwise = constructTermD parentRef seekArgNr events
constructTermD parentRef seekArgNr (Value _ _ _ _ _:events) 
   = constructTermD parentRef seekArgNr events
constructTermD parentRef seekArgNr (Fun _ _ _:events) 
   = constructTermD parentRef seekArgNr events
constructTermD parentRef seekArgNr (LogVar _ _ _:events) 
   = constructTermD parentRef seekArgNr events
constructTermD parentRef seekArgNr (Separator:events) 
   = Black $ constructTermD parentRef seekArgNr events

constructTermV :: EventID -> [Event] -> Term
constructTermV _ [] = Bottom
constructTermV parentRef (Value arity cname ref parent _ :events)
   | parent == parentRef =
         Cons cname [constructTermD ref arg events | arg<-[1..arity]]
   | otherwise = constructTermV parentRef events

constructTermV parentRef (Fun ref parent _:events)
   | parent == parentRef
   = let [a1,a2]=[constructTermD ref arg events | arg<-[1..2]] in
       case constructTermV parentRef events of
         FunTerm mappings -> FunTerm ((a1,a2):mappings)
         Black (FunTerm mappings) -> Black (FunTerm ((a1,a2):mappings))
         Bottom           -> FunTerm [(a1,a2)]
         Black Bottom     -> FunTerm [(a1,a2)]
   | otherwise = constructTermV parentRef events

constructTermV parentRef (LogVar ref parent _:events)
   | parent == parentRef
   = LogVarTerm (constructTermV ref events)
   | otherwise = constructTermV parentRef events

constructTermV parentRef (Demand _ _ _ _:events) 
   = constructTermV parentRef events

constructTermV parentRef (Separator:events) 
   = Black $ constructTermV parentRef events

-- Pretty printing of a term

data ViewConf = ShowLogVarBinds
              | HideLogVarBinds

prettyShowTerm :: ViewConf -> Term -> String
prettyShowTerm viewConf t = prettyFlat (prettierTerm viewConf t)

prettierTerm :: ViewConf -> Term -> DOC
prettierTerm viewConf = render 0 False

  where
    render :: Int -> Bool -> Term -> DOC
    render prec _ (Cons name args)
     | name=="(:)" && length args == 2 =
      paren (prec > 5)
        (nest 2
          (render 10 False (head args) <> text ":" <> render 5 False (args!!1)))
     | take 2 name == "(," = -- tuple
      paren (prec /= 0)
        (nest 2
          (foldr (<>) nil . intersperse (text ",") .
           map ((Prettier.break <>) . render 10 False) $ args))
     {-| isChar name  = -- char
        text (showTerm name)
     | name == "'\t'" = -- Char tab
      text "'\\t'"
     | name == "'\r'" = -- Char line feed
      text "'\\r'"
     | name == "'\f'" = -- Char form feed
      text "'\\f'"-}
     | otherwise =
      paren (length args > 0 && prec /= 0)
        (nest 2 (text name <> 
          (foldr (<>) nil . map ((Prettier.sep <>) . render 10 False) $ args)))
       where isChar "" = False
             isChar (c:_) | c== '\''  = True
                          | otherwise = False
             
    render _ _ (FunTerm pairs) = 
     if length maps == 1
     then text "{ " <> renderMap (head maps) <> text " }"
     else text "{ " <> foldl1 (\ a b -> a <> line <> text ", " <> b)
                              (map renderMap maps) <> line <> text "}"
     where
       maps = findMaps pairs
    render prec par (LogVarTerm val) =
      if val == Bottom 
        then text "?" 
        else case viewConf of
               ShowLogVarBinds ->
                 paren (prec < 10 || par) 
                 (text "?" <> text "/" <> Prettier.break <> render 9 False val)
               HideLogVarBinds ->
                 render prec par val
    render _ _ UnEval = text "_"
    render _ _ Bottom = text "!"

    render prec par (Black t) 
      = text [chr 7] <> (render prec par t)  <> text [chr 7]

    renderMap :: ([Term],Term) -> DOC
    renderMap (args,res) =
      group  (nest 3
        (text "\\ " <>
          foldr (\ a b -> nest 0 (render 10 False a) <> sp <> b)
            nil
            args <> sep <> text "-> " <> render 0 False res))
        
-- accumulates all arguments for each mapping
findMaps :: [(Term,Term)] -> [([Term],Term)]
findMaps [] = []
findMaps ((a,r):pairs) = 
  map (\(as',r') -> (a:as',r')) (findFn r) ++ findMaps pairs

findFn :: Term -> [([Term],Term)]
findFn t = case t of
  FunTerm pairs -> findMaps pairs
  _ -> [([],t)]  -- result of mapping

paren :: Bool -> DOC -> DOC
paren False doc = group (nest 0 doc)
paren True  doc = group (nest 0 (text "(" <> nest 0 doc <> Prettier.break <> text ")"))

sp = text " "


-- auxiliaries:

categoriseBy :: (a -> b) -> [a] -> [(b,[a])]
categoriseBy catF = foldr (\x -> insertToCategory (catF x) x)
                          []

insertToCategory :: b -> a -> [(b,[a])] -> [(b,[a])]
insertToCategory cat x [] = [(cat,[x])]
insertToCategory cat x ((cat',xs):cats)
        | cat == cat' = ((cat',x:xs):cats)
        | otherwise   = (cat',xs):insertToCategory cat x  cats


