------------------------------------------------------------------------------
--- Library for processing XML data.
---
--- Warning: the structure of this library is not stable and
--- might be changed in the future!
---
--- @author Michael Hanus
--- @version January 2011
------------------------------------------------------------------------------

module XML(XmlExp(..),Encoding(..),XmlDocParams(..),
       tagOf,elemsOf,textOf,textOfXml,xtxt,xml,
       showXmlDoc,showXmlDocWithParams,
       writeXmlFile,writeXmlFileWithParams,parseXmlString,readXmlFile,
       readUnsafeXmlFile,readFileWithXmlDocs,updateXmlFile) where

import Char
import Read
import List(intersperse)

------------------------------------------------------------------------------
--- The data type for representing XML expressions.
--- @cons XText - a text string (PCDATA)
--- @cons XElem - an XML element with tag field, attributes, and a list
---               of XML elements as contents
data XmlExp = XText String                             -- text string (PCDATA)
            | XElem String [(String,String)] [XmlExp]  -- (tag attrs contents)

------------------------------------------------------------------------------
--- The data type for encodings used in the XML document.
data Encoding = StandardEnc
              | Iso88591Enc

-- Transform an encoding into its XML-attribute form
encoding2Attribute :: Encoding -> String
encoding2Attribute StandardEnc = ""
encoding2Attribute Iso88591Enc = "encoding=\"iso-8859-1\" "

-- Transform an encoding into its encoding-function
encoding2EncFunc :: Encoding -> String -> String
encoding2EncFunc StandardEnc = standardEncoding
encoding2EncFunc Iso88591Enc = iso88591Encoding

------------------------------------------------------------------------------
-- List of encoding maps
------------------------------------------------------------------------------
-- standard encoding map
standardEncoding :: String -> String
standardEncoding [] = []
standardEncoding (c:cs) 
   | c=='<'      = "&lt;"   ++ standardEncoding cs
   | c=='>'      = "&gt;"   ++ standardEncoding cs
   | c=='&'      = "&amp;"  ++ standardEncoding cs
   | c=='"'      = "&quot;" ++ standardEncoding cs
   | c=='\''     = "&apos;" ++ standardEncoding cs
   | ord c < 32  = "&#"     ++ show (ord c) ++ ";" ++ standardEncoding cs
   | ord c > 127 = "&#"     ++ show (ord c) ++ ";" ++ standardEncoding cs
   | otherwise   = c : standardEncoding cs

-- iso-8859-1
iso88591Encoding :: String -> String
iso88591Encoding [] = []
iso88591Encoding (c:cs) = 
   if ord c `elem` iso88591list
   then c : iso88591Encoding cs
   else standardEncoding [c] ++ iso88591Encoding cs

-- iso-8859-1-list
-- not yet completed...
iso88591list = [192,193,194,195,196,197,198,199,200,201,202,203,204,205,207,
                208,209,210,211,212,214,216,217,218,219,220,221,224,225,228,
                229,226,227,230,231,233,232,235,234,236,237,239,240,241,248,
                246,242,243,244,245,250,249,252,251,253,255]


------------------------------------------------------------------------------
--- The data type for XML document parameters.
--- @cons Enc    - the encoding for a document
--- @cons DtdUrl - the url of the DTD for a document
data XmlDocParams = Enc Encoding
                  | DtdUrl String

-- get the right encoding (i.e., first or standard encoding if not present)
-- from a list of XmlDocParams
lookupEncoding :: [XmlDocParams] -> Encoding
lookupEncoding (Enc f:_) = f
lookupEncoding (DtdUrl _:l) = lookupEncoding l
lookupEncoding [] = StandardEnc

-- get the first DtdUrl from a list of XmlDocParams
lookupDtdUrl :: [XmlDocParams] -> String
lookupDtdUrl (Enc _:l) = lookupDtdUrl l
lookupDtdUrl (DtdUrl url:_) = url

-- does a XmlDocParam include a DtdUrl?
hasDtdUrl :: [XmlDocParams] -> Bool
hasDtdUrl [] = False
hasDtdUrl (DtdUrl _:_) = True
hasDtdUrl (Enc _:l) = hasDtdUrl l

------------------------------------------------------------------------------
-- useful selectors:

--- Returns the tag of an XML element (or empty for a textual element).
tagOf :: XmlExp -> String
tagOf (XElem tag _ _) = tag
tagOf (XText _) = ""


--- Returns the child elements an XML element.
elemsOf :: XmlExp -> [XmlExp]
elemsOf (XElem _ _ xexps) = xexps
elemsOf (XText _) = []


--- Extracts the textual contents of a list of XML expressions.
--- Useful auxiliary function when transforming XML expressions into
--- other data structures.
---
--- For instance,
--- <code>textOf [XText "xy", XElem "a" [] [], XText "bc"] == "xy bc"</code>
textOf :: [XmlExp] -> String
textOf = unwords . filter (not . null) . map textOfXmlExp
 where
   textOfXmlExp (XText s) = s
   textOfXmlExp (XElem _ _ xs) = textOf xs

--- Included for backward compatibility, better use <code>textOf</code>!
textOfXml :: [XmlExp] -> String
textOfXml = textOf

------------------------------------------------------------------------------
-- some useful abbreviations:

--- Basic text (maybe containing special XML chars).
xtxt   :: String -> XmlExp
xtxt s = XText s

--- XML element without attributes.
xml :: String -> [XmlExp] -> XmlExp
xml t c = XElem t [] c

------------------------------------------------------------------------------
-- Pretty printer for XML documents
------------------------------------------------------------------------------
--- Writes a file with a given XML document.
writeXmlFile :: String -> XmlExp -> IO ()
writeXmlFile file xexp = 
   writeXmlFileWithParams file [Enc StandardEnc] xexp

--- Writes a file with a given XML document and XML parameters.
writeXmlFileWithParams :: String -> [XmlDocParams] -> XmlExp -> IO ()
writeXmlFileWithParams file ps xexp =
   writeFile file (showXmlDocWithParams ps xexp)


------------------------------------------------------------------------------
--- Show an XML document in indented format as a string.
------------------------------------------------------------------------------

showXmlDoc :: XmlExp -> String
showXmlDoc xexp = showXmlDocWithParams [] xexp

showXmlDocWithParams :: [XmlDocParams] -> XmlExp -> String
showXmlDocWithParams ps (XElem root attrL xmlEL) =
   "<?xml version=\"1.0\" " ++ 
   (encoding2Attribute (lookupEncoding ps)) ++ "standalone=\"" ++
   (if hasDtdUrl ps then "no" else "yes") ++ "\"?>\n\n" ++
   (if hasDtdUrl ps
    then "<!DOCTYPE " ++ root ++ " SYSTEM \"" ++ lookupDtdUrl ps ++ "\">\n\n"
    else "") ++
   showXmlExp 0 (encoding2EncFunc (lookupEncoding ps))
                (XElem root attrL xmlEL)

showXmlExp :: Int -> (String -> String) -> XmlExp -> String
showXmlExp i encFun (XText s)  = xtab i ++ (encFun s) ++ "\n"
showXmlExp i encFun (XElem tag attrs xexps) =
  xtab i ++ showXmlOpenTag tag attrs encFun ++
  if xexps == []
  then " />\n"
  else if length xexps == 1 && isXText (head xexps)
       then let [XText s] = xexps
            in  ">" ++ (encFun s) ++ "</" ++ tag ++ ">\n"
       else ">\n" ++ showXmlExps (i+2) xexps encFun ++
            xtab i ++ "</" ++ tag ++ ">\n"

xtab n = take n (repeat ' ')

showXmlOpenTag tag attrs encFun =
  "<" ++ tag ++ concat (map ((" "++) . attr2string) attrs)
  where attr2string (attr,value) = attr ++ "=\""
                                        ++ (encFun value) ++ "\""

showXmlExps :: Int -> [XmlExp] -> (String -> String) -> String
showXmlExps encFun xexps i = concatMap (showXmlExp encFun i) xexps

isXText :: XmlExp -> Bool
isXText (XText _) = True
isXText (XElem _ _ _) = False

-- unquote special characters (<,>,&,',") in an XML string:
xmlUnquoteSpecials :: String -> String
xmlUnquoteSpecials [] = []
xmlUnquoteSpecials (c:cs)
  | c=='&'    = let (special,rest) = splitAtChar ';' cs
                 in xmlUnquoteSpecial special rest
  | otherwise = c : xmlUnquoteSpecials cs

xmlUnquoteSpecial special cs
  | special=="lt"   = '<'  : xmlUnquoteSpecials cs
  | special=="gt"   = '>'  : xmlUnquoteSpecials cs
  | special=="amp"  = '&'  : xmlUnquoteSpecials cs
  | special=="quot" = '"'  : xmlUnquoteSpecials cs
  | special=="apos" = '\'' : xmlUnquoteSpecials cs
  | special=="auml" = '\228' : xmlUnquoteSpecials cs
  | special=="ouml" = '\246' : xmlUnquoteSpecials cs
  | special=="uuml" = '\252' : xmlUnquoteSpecials cs
  | special=="Auml" = '\196' : xmlUnquoteSpecials cs
  | special=="Ouml" = '\214' : xmlUnquoteSpecials cs
  | special=="Uuml" = '\220' : xmlUnquoteSpecials cs
  | special=="szlig"= '\223' : xmlUnquoteSpecials cs
  | otherwise       = unquoteUnicode special ++ xmlUnquoteSpecials cs

unquoteUnicode :: String -> String
unquoteUnicode [] = []
unquoteUnicode (c:cs)
  | c=='#'     = case cs of
                   'x':cs' -> [chr (readHex cs')]
                   _       -> [chr (readInt cs)]
  | otherwise  = '&':(c:cs) ++ ";"

------------------------------------------------------------------------------
-- Parser for XML documents
------------------------------------------------------------------------------

--- Reads a file with an XML document and returns
--- the corresponding XML expression.
readXmlFile :: String -> IO XmlExp
readXmlFile file =
 do xmlstring <- readFile file
    let xexps = parseXmlString xmlstring
    if xexps==[]
      then error ("File "++file++" contains no XML document!")
      else if tail xexps /= []
            then error ("File "++file++" contains more than one XML document!")
            else return (head xexps)

--- Tries to read a file with an XML document and returns
--- the corresponding XML expression, if possible.
--- If file or parse errors occur, Nothing is returned.
readUnsafeXmlFile :: String -> IO (Maybe XmlExp)
readUnsafeXmlFile file =
  catch (readXmlFile file >>= return . Just) (\_ -> return Nothing)

--- Pretty prints the contents of an XML file.
showXmlFile :: String -> IO ()
showXmlFile file = readXmlFile file >>= putStr . showXmlDoc

--- Reads a file with an arbitrary sequence of XML documents and
--- returns the list of corresponding XML expressions.
readFileWithXmlDocs :: String -> IO [XmlExp]
readFileWithXmlDocs file = readFile file >>= return . parseXmlString

------------------------------------------------------------------------------
--- Transforms an XML string into a list of XML expressions.
--- If the XML string is a well structured document, the list
--- of XML expressions should contain exactly one element.
parseXmlString :: String -> [XmlExp]
parseXmlString s = fst (parseXmlTokens (scanXmlString s) Nothing)

-- parse a list of XML tokens into list of XML expressions:
-- parseXmlTokens tokens stoptoken = (xml_expressions, remaining_tokens)
parseXmlTokens :: [XmlExp] -> Maybe String -> ([XmlExp],[XmlExp])
parseXmlTokens [] Nothing = ([],[])
parseXmlTokens (XText s : xtokens) stop =
  let (xexps, rem_xtokens) = parseXmlTokens xtokens stop
  in  (XText (xmlUnquoteSpecials s) : xexps, rem_xtokens)
parseXmlTokens (XElem (t:ts) args cont : xtokens) stop
 | t == '<' && head ts /= '/'
   = let (xexps1, xtokens1) = parseXmlTokens xtokens (Just ts)
         (xexps, rem_xtokens) = parseXmlTokens xtokens1 stop
     in  (XElem ts args xexps1 : xexps, rem_xtokens)
 | t == '<' && head ts == '/'
   = if maybe False (==(tail ts)) stop
     then ([], xtokens) -- stop this parser if appropriate stop token reached
     else let (xexps, rem_xtokens) = parseXmlTokens xtokens stop
          in  (XElem ts args cont : xexps, rem_xtokens)
 | otherwise = let (xexps, rem_xtokens) = parseXmlTokens xtokens stop
               in  (XElem (t:ts) args cont : xexps, rem_xtokens)


-- scan an XML string into list of XML tokens:
-- here we reuse XML expressions for representing XML tokens:
-- single open or closing tags are returned by the scanner
-- as an XElem with no contents and first character '<' added to the tag field
scanXmlString :: String -> [XmlExp]
scanXmlString s = scanXml (dropBlanks s)
 where
  scanXml []     = []
  scanXml (c:cs) = if c=='<'
                   then scanXmlElem cs
                   else let (initxt,remtag) = scanXmlText (c:cs)
                         in XText initxt : scanXml remtag

-- scan an XML text until next tag and remove superflous blanks:
scanXmlText :: String -> (String,String)
--original definition:
--scanXmlText s = let (s1,s2) = break (=='<') s
--                 in (concat (intersperse " " (words s1)), s2)
--this implementation is more efficient:
scanXmlText [] = ([],[])
scanXmlText (c:cs) | c=='<' = ([],c:cs)
                   | isSpace c = let (txt,rem) = scanXmlText (dropBlanks cs)
                                  in (if null txt then txt else ' ':txt, rem)
                   | otherwise = let (txt,rem) = scanXmlText cs
                                  in (c:txt,rem)

-- scan an XML element:
scanXmlElem :: String -> [XmlExp]
scanXmlElem [] = []
scanXmlElem (c:cs)
 | c=='!' = if take 2 cs == "--"
            then scanXmlComment (drop 2 cs)
            else scanXmlCData cs
 | c=='?' = scanXmlProcInstr cs
 | otherwise = scanXmlElemName [c] cs

scanXmlElemName ct [] = [XElem ('<':ct) [] []]
scanXmlElemName ct (c:cs)
  | c=='>'    = XElem ('<':ct) [] [] : scanXmlString cs
  | isSpace c = let (attrs,rest) = parseAttrs (dropBlanks cs) in
                 if (head rest)=='/'
                 then XElem ct attrs [] : scanXmlString (drop 2 rest)
                 else XElem ('<':ct) attrs [] : scanXmlString (tail rest)
  | c=='/' && head cs == '>' = XElem ct [] [] : scanXmlString (tail cs)
  | otherwise = scanXmlElemName (ct++[c]) cs

-- scan (and drop) an XML comment:
scanXmlComment :: String -> [XmlExp]
scanXmlComment [] = []
scanXmlComment (c:cs) =
  if c=='-' && take 2 cs == "->"
  then scanXmlString (drop 2 cs)
  else scanXmlComment cs

-- scan (and drop) an XML CDATA element (simplified version):
scanXmlCData :: String -> [XmlExp]
scanXmlCData cs =
  let rest = dropCData cs
  in  if head rest == '>' then scanXmlString (tail rest)
                          else scanXmlCData rest

dropCData [] = []
dropCData (c:cs) 
 | c=='[' = tail (dropWhile (/=']') cs) -- must be improved
 | c=='>' = c:cs
 | otherwise = dropCData cs

-- scan (and drop) an XML processing instructions:
scanXmlProcInstr :: String -> [XmlExp]
scanXmlProcInstr [] = []
scanXmlProcInstr (c:cs) =
  if c=='?' && head cs == '>'
  then scanXmlString (tail cs)
  else scanXmlProcInstr cs

-- parse a string as an attribute list:
parseAttrs :: String -> ([(String,String)],String)
parseAttrs [] = ([],[])
parseAttrs (c:cs)
 | isAlpha c = let (name,rest1)  = splitAtChar '=' (c:cs)
                   (value,rest2) = splitAtChar '"' (tail rest1)
                   (rem_attrs,rem_inp) = parseAttrs (dropBlanks rest2)
               in ((name,xmlUnquoteSpecials value):rem_attrs, rem_inp)
 | otherwise = ([],c:cs)

-- drop blanks in input string:
dropBlanks = dropWhile isSpace

-- split string at particular character, if possible:
splitAtChar _ [] = ([],[])
splitAtChar char (c:cs) =
 if c==char then ([],cs)
            else let (first,rest) = splitAtChar char cs in (c:first,rest)

------------------------------------------------------------------------------
--- An action that updates the contents of an XML file by some transformation
--- on the XML document.
--- @param f - the function to transform the XML document in the file
--- @param file - the name of the XML file
updateXmlFile :: (XmlExp -> XmlExp) -> String -> IO ()
updateXmlFile xmltrans filename = do
  xdoc <- readXmlFile filename
  writeXmlFile filename $!! (xmltrans xdoc)

-- end of XML library
