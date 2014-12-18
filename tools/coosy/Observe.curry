module Observe (observe,
                oLit,
                oInt,
                oBool,
                oChar,
                oFloat,
                oSuccess,
                oOpaque,
                oOpaqueConstr,
                oList,
                oString,
                oPair,
                oTriple,
                o4Tuple,
                o5Tuple,
                oMaybe,
                oEither,
                oFun,
                (~>),
                o0,o1,o2,o3,o4,o5,
                clearLogFile,
                Observer,
                derive
               ) where

import Unsafe
import ReadNumeric(readInt)
--import CoosyRead
import CoosyDerive
import IOExts
import ReadShowTerm
import System (system)
import CoosyTrace


infixr 5 ~>

type Observer a = (a -> Label -> EventID -> [EventID] -> a)

stateName = "coosy.state"

getNewID :: IO EventID
getNewID = do
   maybeStr <- getAssoc stateName
   maybe (setAssoc stateName "1" >>
          return 0)
         (\str -> let Just (n,"") = readInt str in
                  setAssoc stateName (show (n+1)) >>
                  return n)
         maybeStr

getPred :: EventID -> [EventID] -> (EventID,[EventID])
getPred p xs | isVar xs  = (p,xs)
             | otherwise = getPred (head xs) (tail xs)

writeToTraceFile :: Label -> Event -> IO ()
writeToTraceFile label event 
  = appendFile (logFile label) (showEvent event ++"\n")

recordEvent :: Label -> (EventID -> EventID -> EventID -> Event) ->
               EventID -> [EventID] -> IO (EventID,[EventID])
recordEvent label event parent preds 
  = do eventID <- getNewID
       let (pred,logVar) = getPred parent preds 
       doSolve (logVar =:= (eventID:newLogVar))
       writeToTraceFile label (event eventID parent pred)
       return (eventID, newLogVar)
    where newLogVar free

clearLogFile = do
   system ("touch "++logFile ""++"; rm "++logFile "*")
   setAssoc stateName "0" --(show 0)

clearFileCheck = do
  let clearFile = logFileClear
  clearStr <- readFile clearFile
  if clearStr == "1"
    then do setAssoc stateName "0" --(show 0)
            writeFile clearFile ""
    else return ()

observe :: Observer a -> String -> a -> a
observe observeA label x = initialObserver observeA 0 x label (-1) preds
    where preds free

initialObserver :: Observer a -> Int -> Observer a 
initialObserver observeA argNr x label parent preds = unsafePerformIO $ do
      clearFileCheck
      observer' observeA argNr x label parent preds

observer :: Observer a -> Int -> Observer a
observer observeA argNr x label parent preds = unsafePerformIO $
      observer' observeA argNr x label parent preds

observer' :: Observer a -> Int -> a -> Label -> EventID -> [EventID] -> IO a
observer' observeA argNr x l parent preds 
  = do (eventID, newPreds) <- recordEvent l (Demand argNr) parent preds
       if isVar x 
         then 
         do (logVarID,logVarPreds) <- recordEvent l LogVar eventID newPreds
            spawnConstraint 
              (seq (ensureNotFree x) (observeA x l logVarID logVarPreds =:= x))
              (return x)
         else return $ observeA x l eventID newPreds

-- Don't use oLit for type success!!
oLit :: Observer _
oLit l = o0 (showTerm l) l

oInt :: Observer Int 
oInt = oLit

oBool :: Observer Bool
oBool = oLit

oChar :: Observer Char
oChar = oLit

oFloat :: Observer Float 
oFloat = oLit

oSuccess  :: Observer Success 
oSuccess _ = o0 "success" success

oOpaque :: Observer _
oOpaque x = o0 "#" x

oOpaqueConstr :: String -> Observer _
oOpaqueConstr constr x = o0 constr x

oList :: Observer a -> Observer [a]
oList _ [] = o0 "[]" []
oList observeA (x:xs) =
      o2 observeA (oList observeA) "(:)" (:) x xs

oString :: Observer String
oString = oList oChar

oPair  :: Observer a -> Observer b -> Observer (a,b)
oPair observeA observeB (x,y) =
      o2 observeA observeB "(,)" (\a b -> (a,b)) x y

oTriple :: Observer a -> Observer b -> Observer c -> Observer (a,b,c)
oTriple observeA observeB observeC (x,y,z) =
      o3 observeA observeB observeC "(,,)" (\a b c -> (a,b,c)) x y z

o4Tuple :: Observer a -> Observer b -> Observer c -> Observer d
                                                  -> Observer (a,b,c,d)
o4Tuple observeA observeB observeC observeD (x1,x2,x3,x4) =
      o4 observeA observeB observeC observeD "(,,,)"
         (\a b c d -> (a,b,c,d)) x1 x2 x3 x4

o5Tuple :: Observer a -> Observer b -> Observer c -> Observer d -> Observer e
                                                  -> Observer (a,b,c,d,e)
o5Tuple observeA observeB observeC observeD observeE (x1,x2,x3,x4,x5) =
      o5 observeA observeB observeC observeD observeE "(,,,,)"
         (\a b c d e -> (a,b,c,d,e)) x1 x2 x3 x4 x5

oMaybe :: Observer a -> Observer (Maybe a)
oMaybe _ Nothing = o0 "Nothing" Nothing
oMaybe observeA (Just x) = o1 observeA "Just" Just x

oEither :: Observer a -> Observer b -> Observer (Either a b)
oEither observeA _ (Left a)  = o1 observeA "Left" Left a
oEither _ observeB (Right b) = o1 observeB "Right" Right b

oIO :: Observer a -> Observer (IO a)
oIO observeA action parent preds label = 
       action >>= \res -> o1 observeA "<IO>" return res parent preds label

oFun :: Observer a -> Observer b -> Observer (a -> b)        
oFun observeA observeB f label parent preds arg =
  (unsafePerformIO $
    do (eventID,newPreds) <- recordEvent label Fun parent preds
       return (\x -> (observer observeB 2
                        (f (observer observeA 1 x label eventID newPreds))
                        label eventID newPreds)))
  arg
      
(~>) :: Observer a -> Observer b -> Observer (a -> b) 
a ~> b = oFun a b

o0 :: String -> Observer _
o0 constrStr x label parent preds =
  unsafePerformIO $
    do recordEvent label (Value 0 constrStr) parent preds
       return x

o1 :: Observer a -> String 
      -> (a -> b) -> a -> Label -> EventID -> [EventID] -> b
o1 observeA constrStr constr x label parent preds =
  unsafePerformIO $
    do (eventID,newPreds) <- recordEvent label (Value 1 constrStr) parent preds
       return (constr (observer observeA 1 x label eventID newPreds))
   
o2 :: Observer a -> Observer b -> String -> 
      (a -> b -> c) -> a -> b -> Label -> EventID -> [EventID] ->  c
o2 observeA observeB constrStr constr x1 x2 label parent preds =
  unsafePerformIO $
    do (eventID,newPreds) <- recordEvent label (Value 2 constrStr) parent preds
       return (constr (observer observeA 1 x1 label eventID newPreds)
                      (observer observeB 2 x2 label eventID newPreds))
    
o3 :: Observer a -> Observer b -> Observer c -> String ->
      (a -> b -> c -> d) -> 
      a -> b -> c -> Label -> EventID -> [EventID] -> d
o3 observeA observeB observeC constrStr constr x1 x2 x3 label parent preds =
  unsafePerformIO $
    do (eventID,newPreds) <- recordEvent label (Value 3 constrStr) parent preds
       return (constr (observer observeA 1 x1 label eventID newPreds)
                      (observer observeB 2 x2 label eventID newPreds)
                      (observer observeC 3 x3 label eventID newPreds))

o4 :: Observer a -> Observer b -> Observer c -> Observer d -> String ->
      (a -> b -> c -> d -> e) -> 
      a -> b -> c -> d -> Label -> EventID -> [EventID] -> e
o4 observeA observeB observeC observeD constrStr constr x1 x2 x3 x4
  label parent preds =
  unsafePerformIO $
    do (eventID,newPreds) <- recordEvent label (Value 4 constrStr) parent preds
       return (constr (observer observeA 1 x1 label eventID newPreds)
                      (observer observeB 2 x2 label eventID newPreds)
                      (observer observeC 3 x3 label eventID newPreds)
                      (observer observeD 4 x4 label eventID newPreds))

o5 :: Observer a -> Observer b -> Observer c -> Observer d -> 
      Observer e -> String ->
      (a -> b -> c -> d -> e -> f) ->
      a -> b -> c -> d -> e -> Label -> EventID -> [EventID] -> f
o5 observeA observeB observeC observeD observeE constrStr constr
  x1 x2 x3 x4 x5 label parent preds =
  unsafePerformIO $
    do (eventID,newPreds) <- recordEvent label (Value 5 constrStr) parent preds
       return (constr (observer observeA 1 x1 label eventID newPreds)
                      (observer observeB 2 x2 label eventID newPreds)
                      (observer observeC 3 x3 label eventID newPreds)
                      (observer observeD 4 x4 label eventID newPreds)
                      (observer observeE 5 x5 label eventID newPreds))

