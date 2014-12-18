
module CoosyTrace (logDir,
                  logFilePrefix,
                  logFile,
                  logFileClear,
                  Label,
                  EventID,
                  Event(..),
                  Trace,
                  getRef,
                  isValue,
                  isDemand,
                  getPred,
                  getParent,
                  readTrace,
                  showEvent
                  ) where

import ReadNumeric(readInt)
--import CoosyRead

logDir = "COOSYLOGS"

logFilePrefix = "TRAIL."

logFile label = logDir++"/TRAIL."++label

logFileClear = logDir++"/CLEAR"

type Label = String

type EventID = Int

data Event = Demand Int   EventID EventID EventID
           --       argNr ownID   parent  pred
           | Value  Int   String EventID EventID EventID
           --       arity constr ownID   parent  pred
           | Fun          EventID EventID EventID
           --             ownID   parent  pred
           | LogVar       EventID EventID EventID
           --             ownID   parent  pred
           | Separator
  deriving Eq

type Trace = [Event]

getRef :: Event -> EventID
getRef (Demand _ ref _ _) = ref
getRef (Value  _ _ ref _ _) = ref
getRef (Fun    ref _ _) = ref
getRef (LogVar ref _ _) = ref

getPred :: Event -> EventID
getPred (Value  _ _ _ _ pred) = pred
getPred (Demand _ _ _ pred) = pred
getPred (Fun    _ _ pred) = pred
getPred (LogVar _ _ pred) = pred

getParent :: Event -> EventID
getParent (Value  _ _ _ parent _) = parent
getParent (Demand _ _ parent _) = parent
getParent (Fun    _ parent _) = parent
getParent (LogVar _ parent _) = parent

isValue :: Event -> Bool
isValue (Value  _ _ _ _ _) = True
isValue (Demand _ _ _ _) = False
isValue (Fun  _ _ _) = False
isValue (LogVar _ _ _) = False

isDemand :: Event -> Bool
isDemand (Value  _ _ _ _ _) = False
isDemand (Demand _ _ _ _) = True
isDemand (Fun  _ _ _) = False
isDeamnd (LogVar _ _ _) = False

isLogVar :: Event -> Bool
isLogVar (Value  _ _ _ _ _) = False
isLogVar (Demand _ _ _ _) = False
isLogVar (Fun    _ _ _) = False
isLogVar (LogVar _ _ _) = True

isFun :: Event -> Bool
isFun (Value  _ _ _ _ _) = False
isFun (Demand _ _ _ _) = False
isFun (Fun    _ _ _) = True
isFun (LogVar _ _ _) = False


showEvent (Demand argNr eventID parent pred) =
    (show eventID)++'\t':'D':(showInt argNr)++'\t':(showInt parent)++
    '\t':showInt pred
showEvent (Value arity constr eventID parent pred) =
    (show eventID)++'\t':'V':(showInt arity)++'\t':(showInt parent)++
    '\t':(showInt pred)++(show constr)
showEvent (Fun eventID parent pred) =
    (show eventID)++'\t':'F':(showInt parent)++'\t':(showInt pred)
showEvent (LogVar eventID parent pred) =
    (show eventID)++'\t':'L':(showInt parent)++'\t':(showInt pred)

showInt :: Int -> String
showInt i = if i<0 then '-':show (negate i) else show i

readEvent :: String -> Event
readEvent str =
   case readInt str of
     Just (ref,_:dv:str1) ->
           if dv=='L' || dv=='F'
              then
                case readInt str1 of
                  Just (parent,_:str2) ->
                    case readInt str2 of 
                      Just (pred,_) -> 
                        if dv=='L'
                           then LogVar ref parent pred
                           else Fun    ref parent pred 
           else
            case readInt str1 of
              Just (argNr,_:str2) ->
                case readInt str2 of
                  Just (parent,_:str3) ->
                    case readInt str3 of 
                      Just (pred,str4) ->
                        if dv=='D'
                          then Demand argNr ref parent pred 
                          else Value argNr (core str4) ref parent pred

core (_:xs) = core' xs
  where core' [_] = []
        core' (x:y:ys) = x:core' (y:ys)

readTrace :: String -> Trace
readTrace str = map readEvent (lines str)
