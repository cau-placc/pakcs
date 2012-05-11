import AbstractCurry
import AbstractGoodies
import AbstractCurryPrinter
import Char
import FileGoodies
import List
import System
 
data Options = LowCase | FileName String

main = do args <- getArgs
          derive (reverse (argsToOptions args))

usagemsg = "usage: data2xml [-low] <filename>\n"++
           "Options:\n" ++
           "-low: make all tags lowercase"

argsToOptions [] = error usagemsg 
argsToOptions [s] = [FileName (stripSuffix s)]
argsToOptions ("-low":o:opts) = LowCase : argsToOptions (o:opts)

derive (FileName fn:opts)
  = do CurryProg modName  _ ts _ _ <- readCurry fn
       let (specials,types) = if isPrelude modName 
                                then (specialFuncs opts,filterSpecials ts)
                                else ([],ts)
           progName = fn++"DataToXml"
           impTypes = maybeString $ nub $ filter ((/=modName) .fst) $ 
                         concatMap requiredTypesTypeDecl types
       imports <- importTypes modName impTypes
       writeFile (progName++".curry") $ showProg $
        CurryProg progName (nub $ ["XML",fn] ++ imports) [] 
          (map (mkType2Xml opts) types++map (mkXml2Type opts) types++specials) []
       putStrLn ("You can now import "++progName)

maybeString xs 
  = if not (elem (p "String") xs) && elem (p "[]") xs && elem (p "Char") xs
       then (p "String":xs) else xs
  where
    p s = ("Prelude",s)

----------------------------
-- naming the new functions
----------------------------

toXmlName (m,s) = case (isPrelude m,s,isTupleName s) of 
                   (True,"[]",_)     -> (m,"list_To_Xml")
                   (True,"()",_)     -> (m,"unitToXml")
                   (True,_,True)     -> (m,"tuple"++show (length s-2)++"ToXml")
                   (_,c:cs,_)        -> (m,toLower c:cs ++ "ToXml")
                
                  
 
fromXmlName (m,s) 
  = case (isPrelude m,s,isTupleName s) of
     (True,"[]",_)     -> (m,"xml_To_List")
     (True,"()",_)     -> (m,"xmlToUnit")
     (True,_,True)     -> (m,"xmlToTuple"++show (length s-1))
     _                 -> (m,"xmlTo"++s)
                  

listTag opts = tag opts "[List]"

isTupleName (n:name) = n=='(' && isTuple name
  where
    isTuple ""  = False
    isTuple ")" = True
    isTuple (c:c':cs) = c==',' && isTuple (c':cs)
    
----------------------------
-- generating tags
----------------------------

tag opts s = if elem LowCase opts then map toLower s else s

-------------------------------------------------
-- make functions to transform data terms to xml
-------------------------------------------------
 
mkType2Xml _ (CTypeSyn name vis vars t) 
  = CFunc (toXmlName name) 1 vis
          (CFuncType (CTCons name (map CTVar vars)) xmlType)
          (CRules CFlex [CRule [CPVar (0,"x0")] 
                               [noGuard (call2xml (t,0))] []])
mkType2Xml opts (CType name vis vars cs) 
  = CFunc (toXmlName name) (1+length vars) vis
          (type2XmlType vars 
            (CFuncType (CTCons name (map CTVar vars)) xmlType))
          (CRules CFlex (map (mkConsDecl2Xml opts $ 
                                map (CPVar . renVar) vars) cs))
  
mkConsDecl2Xml opts patVars (CCons name arity _ args)
  = CRule (newPatVars++[CPComb name (pVars arity)] )
          [noGuard 
             (xml opts (snd name) [] (map call2xml (zip args [0..])))] []
  where
    newPatVars = renameUnused (map renVar $ concatMap allTVars args) patVars 


type2XmlType vars t 
  = foldr CFuncType t (map (\x->CFuncType (CTVar x) xmlType) vars)  

call2xml (t,i) = CApply (call2xmlType t) (toVar i)

call2xmlType (CTVar v) = CVar (renVar v)
call2xmlType (CTCons name args) 
  | snd name == "[]" &&  args==[CTCons ("Prelude","Char") []]
  = sym $ toXmlName ("Prelude","String")
  | otherwise 
  = app (CSymbol $ toXmlName name) (map call2xmlType args)
call2xmlType (CFuncType _ _) = error "unable to transform functions to XML"

xml opts name attrs elems 
  = app (sym ("XML","XElem")) [cString (tag opts name),cList attrs,cList elems]

xmlType = CTCons ("XML","XmlExp") []

-------------------------------------------------
-- make functions to transform xml to data terms 
-------------------------------------------------

mkXml2Type _ (CTypeSyn name vis vars t) 
  = CFunc (fromXmlName name) 1 vis
          (CFuncType xmlType (CTCons name (map CTVar vars)))
          (CRules CFlex [CRule [CPVar (0,"x0")] 
                               [noGuard (callXml2 (t,0))] []])
mkXml2Type opts (CType name vis vars cs) 
  = CFunc (fromXmlName name) (1+length vars) vis
          (xml2typeType vars 
             (CFuncType xmlType (CTCons name (map CTVar vars))))
          (CRules CFlex (map (mkXml2ConsDecl opts $ map (CPVar . renVar) vars) cs))
  
renVar (i,s) = case s of
                ('x':xs) -> (i,'t':xs)
                _ -> (i,s)

xml2typeType vars t 
  = foldr CFuncType t (map (\x->CFuncType xmlType (CTVar x)) vars)  

mkXml2ConsDecl opts patVars (CCons name arity _ args)
  = CRule (newPatVars++[pxml opts (snd name) [] (pVars arity)])
          [noGuard (app (sym name) (map callXml2 (zip args [0..])))] []
  where
    newPatVars = renameUnused (map renVar $ concatMap allTVars args) patVars 

renameUnused _ [] = []
renameUnused usedVars (CPVar (i,v):vs) 
      | elem (i,v) usedVars = CPVar (i,v) : renameUnused usedVars vs
      | otherwise = CPVar (i,"_") : renameUnused usedVars vs


pxml opts name attrs elems 
  = CPComb ("XML","XElem") [pString (tag opts name),pList attrs,pList elems]

callXml2 (t,i) = CApply (callXml2Type t) (toVar i)

callXml2Type (CTVar v) = CVar (renVar v)
callXml2Type (CTCons name args)
  | snd name=="[]" && args==[CTCons ("Prelude","Char") []]
  = sym (fromXmlName ("Prelude","String"))
  | otherwise 
  = app (CSymbol $ fromXmlName name) (map callXml2Type args)
callXml2Type (CFuncType _ _) = error "unable to transform functions from XML"

-----------------------------
-- treat imported data types
-----------------------------

importTypes m ts = 
                 do is <- sequenceIO $ map importType ts
                    let nubis = nub (concat is)
                        specials = if isPrelude m then ["Read"] else []
                    imessage nubis
                    return (nubis++specials)
 
imessage [] = done
imessage [m] = putStrLn $ "You also need the module "++m
imessage (m:m':ms) = putStrLn $ "You also need the modules "++(show $ m:m':ms)
                 
importType (m,f)
  | isPrelude m && elem f ["String","[]","Char"] 
  = return ["PreludeDataToXml"]
  | isPrelude m && f == "Int" = return ["PreludeDataToXml"]
  | isPrelude m && f == "Float"
  = do putStrLn "Problem: There is no read function for Floats"
       return []
  | otherwise = return [m++"DataToXml"]

-----------------------------------------
-- treat special prelude types
-----------------------------------------

specialNames = ["Int","Float","String","Char","IO","Success","[]"]

filterSpecials 
  = filter ((\ (m,n) -> not (isPrelude m && elem n specialNames)) . typeName)

specialFuncs opts
             = [mkList2xml opts,mkXml2List opts,
                baseType2xml opts "String",baseTypeXml2 opts "String",
                baseType2xml opts "Int",baseTypeXml2 opts "Int",
                baseType2xml opts "Char",baseTypeXml2 opts "Char"]

mkList2xml opts = 
   CFunc (toXmlName ("Prelude","[]")) 2 Private
     (CFuncType (CFuncType (CTVar (0,"a")) xmlType)
                (CFuncType (CTCons ("Prelude","[]") [CTVar (0,"a")]) xmlType))
     (CRules CFlex 
        [CRule (pVars 2)
             [noGuard (app (sym ("XML","XElem")) [cString (listTag opts),nil, 
                            app (app (preSym "map") [toVar 0]) [toVar 1]])] []])

mkXml2List opts = 
   CFunc (fromXmlName ("Prelude","[]")) 2 Private
     (CFuncType (CFuncType xmlType (CTVar (0,"a")))
                (CFuncType xmlType (CTCons ("Prelude","[]") [CTVar (0,"a")])))
     (CRules CFlex 
        [CRule [x, CPComb ("XML","XElem") [pString (listTag opts),pNil,y]]
             [noGuard  (app (preSym "map") [toVar 0,toVar 1])] []])
  where
    [x,y] = pVars 2

baseType2xml opts s 
  = CFunc (toXmlName ("Prelude",s)) 1 Private
     (CFuncType (CTCons ("Prelude",s) []) xmlType)
     (CRules CFlex 
       [CRule (pVars 1) 
         [noGuard (xml opts s [] [writeFun s])] []])

baseTypeXml2 opts s 
  = CFunc (fromXmlName ("Prelude",s)) 1 Private
     (CFuncType xmlType (CTCons ("Prelude",s) []))
     (CRules CFlex 
        (CRule [pxml opts s [] [CPComb ("XML","XText") (pVars 1)]]
           [noGuard (readFun s)] []:
         if s=="String" 
           then [CRule [pxml opts s [] []]
                       [noGuard (cString "")] []]
           else []))

readFun "Int" = app (sym ("Read","readInt")) [toVar 0]
readFun "Char" = app (preSym "head") [toVar 0]
readFun "Float" = error "no readFloat"
readFun "String" = toVar 0

writeFun s = case s of
              "String" -> app (CSymbol ("XML","xtxt")) [toVar 0]
              _        -> app (CSymbol ("XML","xtxt")) [cShow (toVar 0)]




