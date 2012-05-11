--------------------------------------------------------------------------
--- module to convert Umbrello 1.5.52 output to datatype ERD 
--------------------------------------------------------------------------

module XML2ERD(convert) where

import XML
import ERD
import Read
import List
import Maybe
import Char
import ReadShowTerm
import Time



findElements :: [XmlExp] -> [String] -> [XmlExp]
findElements [] _ = []
findElements (x@(XElem t _ _) : xs) [s] 
  | t == s = (x : findElements xs [s])
  | otherwise = findElements xs [s]
findElements ((XElem t _ content) : xs) (z:y:ys) 
  | t == z = findElements content (y:ys)
  | otherwise = findElements xs (z:y:ys)

getContent :: XmlExp -> [XmlExp]
getContent (XElem _ _ content) = content
getContent (XText _) = []

getUMLModel :: [XmlExp] -> String -> [XmlExp]
getUMLModel [] _ = []
getUMLModel (x@(XElem tag _ _):xs) name 
  | tag == "UML:Model" = case (lookup "name" (getAttrs x)) of
      Just d  -> if d == name then getContent x
                 else getUMLModel xs name
      Nothing -> getUMLModel xs name
  | otherwise = getUMLModel xs name

convert :: XmlExp -> ERD
convert xml = 
  let contentxml  = getContent 
                      (head 
                        (findElements 
                           [xml] 
                           ["XMI","XMI.content","UML:Model",
                            "UML:Namespace.ownedElement"]))
      logicalView = getContent 
                      (head 
                        (findElements 
                          (getUMLModel contentxml "Logical View") 
                          ["UML:Namespace.ownedElement"]))
      erm         = getUMLModel contentxml "Entity Relationship Model"
      exml        = findElements erm ["UML:Namespace.ownedElement",
                                      "UML:Entity"]
      rxml        = findElements erm ["UML:Namespace.ownedElement",
                                      "UML:Association"]
      idlist      = (iddata logicalView) ++ (identities exml)
      name        = fromJust 
                      (lookup "name" 
                              (getAttrs 
                                 (head (findElements 
                                          erm 
                                          ["XMI.extension","diagrams",
                                           "diagram"]))))
      es          = map (convertE idlist) exml
      rs          = map (convertR idlist) rxml
  in
  if uniqueNames es rs
  then ERD name es rs
  else error "names (entity, relationship, role) in er-diagramm not unique"

uniqueNames :: [Entity] -> [Relationship] -> Bool
uniqueNames es rs = 
  length es + length rs == length (nub ((map eName es)++(concatMap rNames rs)))

eName :: Entity -> String
eName (Entity n _) = n

rNames :: Relationship -> [String]
rNames (Relationship rn [REnd r1 _ _, REnd r2 _ _]) = [rn, r1, r2]
  

 
iddata :: [XmlExp] -> [(String,String)]
iddata [] = []
iddata (x@(XElem t attrs _):elems) 
      | t == "UML:DataType" || t == "UML:Class" =
          let id = fromJust (lookup "xmi.id" attrs)
              name = fromJust (lookup "name" attrs)
          in
          (id,name) : iddata elems
      | t == "UML:Package" = 
          (iddata (findElements [x] 
                                ["UML:Package","UML:Namespace.ownedElement",
                                 "UML:DataType"]))
          ++ (iddata elems)
      | otherwise = iddata elems


identities :: [XmlExp] -> [(String,String)]
identities [] = []
identities ((XElem t attrs _):elems) 
      | t == "UML:Entity" =
          let id = fromJust (lookup "xmi.id" attrs)
              name = fromJust (lookup "name" attrs)
          in
          (id,name) : identities elems
      | otherwise = identities elems
    
getAttrs (XElem _ attrs _) = attrs
  



-- convert entity 
convertE :: [(String, String)] -> XmlExp -> Entity
convertE idlist (XElem "UML:Entity" attrs alist) =
  let name = fromJust (lookup "name" attrs)
      attributes = map (convertAttr idlist) alist
  in
  if alist==[]
  then error ("Entity " ++ name ++ " without attributes")
  else Entity name (map (checkAttr name) attributes)

checkAttr :: String -> Attribute -> Attribute 
checkAttr ename (Attribute name domain key null) =
  let n = if (isLower (head name))
          then (toUpper (head name)):(tail name)
          else name
      v = getValue domain
  in
  if n == "Key" 
  then error ("attribute name Key is not allowed in entity "++ename)
  else if v
       then 
         if null 
         then error ("attribute "++name
                      ++" with default value should not be null in entity "++ename)
         else if key==Unique
              then error ("attribute "++name
                           ++" with unique value should not be null in entity "
                           ++ename)
              else Attribute n domain key null
       else Attribute n domain key null

getValue :: Domain -> Bool
getValue (IntDom Nothing) = False
getValue (IntDom (Just _)) = True
getValue (FloatDom Nothing) = False
getValue (FloatDom (Just _)) = True
getValue (CharDom Nothing) = False
getValue (CharDom (Just _)) = True
getValue (StringDom Nothing) = False
getValue (StringDom (Just _)) = True
getValue (BoolDom Nothing) = False
getValue (BoolDom (Just _)) = True
getValue (DateDom Nothing) = False
getValue (DateDom (Just _)) = True
getValue (UserDefined _ Nothing) = False
getValue (UserDefined _ (Just _)) = True


  

-- convert relationship
convertR :: [(String, String)] -> XmlExp -> Relationship
convertR idlist 
         (XElem "UML:Association" attrs 
                [(XElem "UML:Association.connection" _ [end1, end2])]) =
  let name = fromJust (lookup "name" attrs)
      rends = [convertREnd idlist end1, 
               convertREnd idlist end2]
  in 
  if twoMin rends
  then error ("relationship " ++ name ++ " has two minima")
  else if name==""
       then error "relationship without name"
       else Relationship (toUpper (head name) : tail name) rends
    where
      convertREnd :: [(String, String)] -> XmlExp -> REnd
      convertREnd idl (XElem "UML:AssociationEnd" alist _) =
        let t = fromJust (lookup "type" alist)
            name = fromJust (lookup "name" alist)  
            mult = lookup "multiplicity" alist
        in
        if name==""
        then error "role without name"
        else REnd (fromJust (lookup t idl)) 
                  (toLower (head name) : tail name) 
                  (convertCard mult)
      twoMin [REnd _ _ c1, REnd _ _ c2] = case c1 of
        Between i _ -> if i > 0 
                       then case c2 of Between j _ -> j > 0 
                                       Exactly _   -> False
                       else False
        Exactly _ -> case c2 of Between m _ -> m > 0 
                                Exactly _   -> True

      -- exactly: >0
      -- range: min<max
convertCard :: Maybe String -> Cardinality
convertCard c = case c of
        Nothing -> error "cardinality missing"
        Just "m" -> Between 0 Infinite
        Just "n" -> Between 0 Infinite
        Just ('(':m:ms) -> let (min, (_:max')) = break (== ',') (m:ms) 
                               max = fst (break (== ')') max')
                           in
                           if all isDigit min 
                           then let minimum = readInt min
                                in
                                if all isDigit max
                                then if minimum == readInt max then Exactly minimum
                                     else if minimum < readInt max
                                          then Between minimum (Max (readInt max))
                                          else error "wrong cardinality"
                                else Between minimum Infinite
                           else error "wrong cardinality (min)"
        Just i   -> if all isDigit i 
                    then let e = readInt i
                         in
                         if e > 0 then Exactly e else error "cardinality <= 0"
                    else error "wrong cardinality"

-- convert attribute
convertAttr :: [(String, String)] -> XmlExp -> Attribute
convertAttr idlist (XElem "UML:EntityAttribute" alist _) =
  let t = fromJust (lookup "type" alist)
      name = fromJust (lookup "name" alist)
      init = lookup "initialValue" alist  
      d = convertDomain (lookup t idlist) init
      dbindex_type = fromJust (lookup "dbindex_type" alist) 
      pkey = if dbindex_type == "1101"
             then PKey
             else if dbindex_type == "1103"  
                  then Unique
                  else NoKey
      allow_null = fromJust (lookup "allow_null" alist)
      null = if allow_null == "0" 
             then False
             else True
  in 
  Attribute name d pkey null 

-- datatypes:                
int    = ["Int","int"] 
char   = ["Char", "char"]
string = ["String","string", "text", "varchar"]
float  = ["Float", "float", "Double", "double"]
bool   = ["Bool", "bool"]
date   = ["Date", "date"]

convertDomain :: Maybe String -> Maybe String -> Domain
convertDomain Nothing _ = error "domain missing"
convertDomain (Just t) Nothing
  | elem t int    = IntDom Nothing
  | elem t float  = FloatDom Nothing
  | elem t char   = CharDom Nothing
  | elem t string = StringDom Nothing
  | elem t bool   = BoolDom Nothing
  | elem t date   = DateDom Nothing
  | otherwise     = UserDefined t Nothing
convertDomain (Just t) (Just d) = 
  if d == "" 
  then convertDomain (Just t) Nothing
  else convertD t d
    where 
      convertD :: String -> String -> Domain
      convertD typ dom 
        | elem typ int    = IntDom (Just (readInt dom))
        | elem typ float  = FloatDom (Just (readQTerm dom))
        | elem typ char   = CharDom (Just (head dom))
        | elem typ string = StringDom (Just dom)
        | elem typ bool   = if dom == "true" 
                            then BoolDom (Just True) 
                            else BoolDom (Just False)
        | elem t date     = DateDom (Just (parseDate dom))
        | otherwise       = UserDefined t (Just d)

       
      -- 01.01.2007 15:16:17 ~> CalendarTime 2007 1 1 15 16 17 0
      parseDate :: String -> CalendarTime
      parseDate s = 
        let (ts,_:cs) = break (== ' ') s
            d1    = break (== '.') ts
            day   = readInt (fst d1)
            d2    = break (== '.') (tail (snd d1))
            month = readInt (fst d2)
            year  = readInt (tail (snd d2))    
            c1    = break (== ':') cs 
            hour  = readInt (fst c1)
            c2    = break (== ':') (tail (snd c1))
            minute = readInt (fst c2)
            second = readInt (tail (snd c2))
        in
        CalendarTime year month day hour minute second 0

-------------------------------------------------------------------------------
test = do
  xml <- readXmlFile "./Uni.xmi"
  print (convert xml)

{-
(ERD "Uni" 
  [(Entity "Student" 
     [(Attribute "MatrikelNr" (IntDom Nothing) PKey False),
      (Attribute "Name" (StringDom Nothing) NoKey False),
      (Attribute "Vorname" (StringDom Nothing) NoKey False),
      (Attribute "Email" (UserDefined "MyModule.Email" Nothing) NoKey True)]),
   (Entity "Veranstaltung" 
     [(Attribute "Nr" (IntDom Nothing) PKey False),
      (Attribute "Titel" (StringDom Nothing) Unique False),
      (Attribute "SWS" (IntDom (Just 4)) NoKey False)]),
   (Entity "Dozent" 
     [(Attribute "Nr" (IntDom Nothing) PKey False),
      (Attribute "Name" (StringDom Nothing) NoKey False),
      (Attribute "Vorname" (StringDom Nothing) NoKey False)]),
   (Entity "Gruppe" 
     [(Attribute "Termin" (StringDom Nothing) NoKey False)])] 
  [(Relationship "Veranstalten" 
     [(REnd "Dozent" "wird_gehalten" (Exactly 1)),
      (REnd "Veranstaltung" "haelt" (Between 0 Infinite))]),
   (Relationship "Teilnahme" 
     [(REnd "Student" "wird_besucht" (Between 0 Infinite)),
      (REnd "Veranstaltung" "nimmt_teil" (Between 0 Infinite))]),
   (Relationship "Zugehoerigkeit" 
     [(REnd "Student" "besteht_aus" (Exactly 3)),
      (REnd "Gruppe" "ist_in" (Between 0 Infinite))])])
-}