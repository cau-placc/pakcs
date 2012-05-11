------------------------------------------------------------------------------
--- This module contains a very simple parser for HTML documents.
---
--- @author Michael Hanus
--- @version November 2011
------------------------------------------------------------------------------

module HtmlParser(readHtmlFile,parseHtmlString) where

import HTML
import Char

--- Reads a file with HTML text and returns the corresponding HTML expressions.
--- @param file - the name of a file containing HTML text
--- @return a list of HTML expressions (if the file contains exactly one
---         HTML document, this list should contain exactly one element)
readHtmlFile :: String -> IO [HtmlExp]
readHtmlFile file = readFile file >>= return . parseHtmlString

------------------------------------------------------------------------------
--- Transforms an HTML string into a list of HTML expressions.
--- If the HTML string is a well structured document, the list
--- of HTML expressions should contain exactly one element.
parseHtmlString :: String -> [HtmlExp]
parseHtmlString s = reverse (parseHtmlTokens [] (scanHtmlString s))

--- The data type for representing HTML tokens.
data HtmlToken = HText String | HElem String [(String,String)]

-- parse a list of HTML tokens into list of HTML expressions:
-- (first argument "helems" is a stack of already read tokens)
parseHtmlTokens :: [HtmlExp] -> [HtmlToken] -> [HtmlExp]
parseHtmlTokens helems [] = helems
parseHtmlTokens helems (HText s : hs) =
 parseHtmlTokens (HtmlText s : helems) hs
parseHtmlTokens helems (HElem (t:ts) args : hs) =
 if t == '/'
 then let (structargs,elems,rest) = splitHtmlElems ts helems
      in parseHtmlTokens ([HtmlStruct ts structargs elems] ++ rest) hs
 else parseHtmlTokens (HtmlStruct (t:ts) args [] : helems) hs


-- split the HTML token stack up to a particular token:
splitHtmlElems _ [] = ([],[],[])
splitHtmlElems tag (HtmlText s : hs) =
 let (largs,elems,rest) = splitHtmlElems tag hs
 in (largs, elems ++ [HtmlText s], rest)
splitHtmlElems tag (HtmlStruct s args cont@(_:_) : hs) =
 let (largs,elems,rest) = splitHtmlElems tag hs
 in (largs, elems ++ [HtmlStruct s args cont], rest)
splitHtmlElems tag (HtmlStruct s args []: hs) =
 if tag==s
 then (args,[],hs)
 else let (largs,elems,rest) = splitHtmlElems tag hs
      in  (largs, elems ++ [HtmlStruct s args []], rest)


-- scan an HTML string into list of HTML tokens:
scanHtmlString :: String -> [HtmlToken]
scanHtmlString s = scanHtml s
 where
  scanHtml []     = []
  scanHtml (c:cs) =
    if c=='<'
    then if take 3 cs == "!--"
         then scanHtmlComment cs
         else if take 4 (map toLower cs) == "pre>"
              then scanHtmlPre "" (skipFirstNewLine (drop 4 cs))
              else scanHtmlElem [] cs
    else let (initxt,remtag) = break (=='<') (c:cs)
          in HText initxt : scanHtml remtag

-- scan an HTML element
scanHtmlElem :: String -> String -> [HtmlToken]
scanHtmlElem ct [] = [HText ('<':ct)] -- incomplete element
scanHtmlElem ct (c:cs)
  | c=='>'    = HElem ct [] : scanHtmlString cs
  | isSpace c = let (args,rest) = splitAtElement (=='>') (dropWhile isSpace cs)
                    revargs = reverse args
                 in if null args || head revargs /= '/'
                    then HElem ct (string2args args) : scanHtmlString rest
                    else HElem ct (string2args (reverse (tail revargs)))
                           : HElem ('/':ct) [] : scanHtmlString rest
  | c=='/' && head cs == '>' = HElem ct [] : HElem ('/':ct) []
                                           : scanHtmlString (tail cs)
  | otherwise = scanHtmlElem (ct++[toLower c]) cs

-- scan an HTML comment
scanHtmlComment :: String -> [HtmlToken]
scanHtmlComment [] = []
scanHtmlComment (c:cs) =
  if c=='-' && take 2 cs == "->"
  then scanHtmlString (drop 2 cs)
  else scanHtmlComment cs

-- scan an HTML preformatted element
scanHtmlPre :: String -> String -> [HtmlToken]
scanHtmlPre _ [] = []  -- errorneous incomplete element
scanHtmlPre pre (c:cs) =
  if c=='<' && take 5 (map toLower cs) == "/pre>"
  then HElem "pre" [] : HText (reverse pre) : HElem "/pre" []
       : scanHtmlString (drop 5 cs)
  else scanHtmlPre (c:pre) cs

-- split a string into blank separated list of strings:
string2args :: String -> [(String,String)]
string2args [] = []
string2args (c:cs) =
   let (arg1,rest) = splitAtElement isSpace (c:cs)
   in  deleteApo (splitAtElement (=='=') arg1)
        : string2args (dropWhile isSpace rest)

deleteApo (tag,[]) = (map toLower tag,[])
deleteApo (tag,c:cs) | c=='"'    = (map toLower tag, deleteLastApo cs)
                     | c=='\''   = (map toLower tag, deleteLastApo cs)
                     | otherwise = (map toLower tag, c:cs)

deleteLastApo [] = []
deleteLastApo [c] = if c=='"' || c=='\'' then [] else [c]
deleteLastApo (c1:c2:cs) = c1 : deleteLastApo (c2:cs)


-- split a list at the first element satisfying a predicate:
splitAtElement _ [] = ([],[])
splitAtElement p (c:cs) =
  if p c then ([],cs)
         else let (first,rest) = splitAtElement p cs in (c:first,rest)

skipFirstNewLine :: String -> String
skipFirstNewLine [] = []
skipFirstNewLine (c:cs) = 
  if c=='\n' then cs
             else if isSpace c then skipFirstNewLine cs else c:cs

-- end of HTML parser
