--- Provides a datatype representing data in JavaScript Object Notation
--- as well as traversal, read and show functions for that datatype.
---
--- See http://www.crockford.com/JSON/ for further information.
---
--- @author Sebastian Fischer
module Json ( Json(..), trJson, readJson, showJson ) where

import Char
import Parse
import ReadShowTerm

infixr 0 &&>

(&&>) :: Bool -> a -> a
b &&> c | b = c

data Json
  = Object [(String, Json)]
  | Array [Json]
  | String String
  | Int Int
  | Bool Bool
  | Null
 deriving (Eq,Show)

--- Universal transformation for JSON values.
trJson :: ([(String, a)] -> a) 
       -> ([a] -> a)
       -> (String -> a)
       -> (Int -> a)
       -> (Bool -> a)
       -> a
       -> Json 
       -> a

trJson object array string int bool null (Object ms) 
  = object (map member ms)
 where
  member (key, value)
    = (key, (trJson object array string int bool null) value)

trJson object array string int bool null (Array vs) 
  = array (map (trJson object array string int bool null) vs)

trJson _ _ string _ _ _ (String s) = string s
trJson _ _ _ int _ _ (Int n) = int n
trJson _ _ _ _ bool _ (Bool b) = bool b
trJson _ _ _ _ _ null Null = null

--- Transforms a JSON value into its String representation.
showJson :: Json -> String
showJson json = trJson object array string int bool null json []
 where
  list = foldr1 (\x xs -> x . (","++) . xs)

  object [] = ("{}"++)
  object (m:ms) = ("{"++) . list (map member (m:ms)) . ("}"++)
  member (key, value) = (show key++) . (":"++) . value

  array [] = ("[]"++)
  array (v:vs) = ("["++) . list (v:vs) . ("]"++)

  string s = (show s++)
  int n = (show n++)
  bool b = (map toLower (show b)++)
  null = ("null"++)

--- Parses a JSON value from its string representation.
readJson :: String -> Json
readJson s | null sols = failed
           | otherwise = head sols
 where
  sols = map fst (filter (null . snd) (jsonP s))

spaceP :: Parser Char String
spaceP s = [span isSpace s]

listP :: Show a => Parser Char a -> Parser Char [a]
listP p s 
  = case (p <.> spaceP) s of
      [] -> [([],s)]
      [(v,',':s1)] -> update (v:) (spaceP <:> listP p) s1
      [(v,s1)] -> [([v],s1)]
      xs -> error (show xs)

stringP :: Parser Char String
stringP = readsQTerm

jsonP :: Parser Char Json
jsonP = spaceP <:> (valueP <.> spaceP)

valueP :: Parser Char Json
valueP s = case s of
  '{':cs -> update Object (listP memberP <.> spaceP <.> terminal '}') cs
  '[':cs -> update Array (listP jsonP <.> spaceP <.> terminal ']') cs
  '\"':_ -> update String stringP s
  't':'r':'u':'e':cs -> [(Bool True,cs)]
  'f':'a':'l':'s':'e':cs -> [(Bool False,cs)]
  'n':'u':'l':'l':cs -> [(Null,cs)]
  c:cs -> if isDigit c || c=='-' && isDigit (head cs) 
           then update Int readsQTerm s else []
  _ -> []

memberP :: Parser Char (String, Json)
memberP
  = spaceP <:> (stringP <*> \key ->
                update (\val -> (key,val)) (spaceP <:> terminal ':' <:> jsonP))


