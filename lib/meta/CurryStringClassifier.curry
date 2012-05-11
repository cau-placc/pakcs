----------------------------------------------------------------------
--- The Curry string classifier is a simple tool to process strings containing 
--- Curry source code. The source string is classified into the following
--- categories:
---
---   * moduleHead - module interface, imports, operators
---
---   * code - the part where the actual program is defined
---
---   * big comment - parts enclosed in {- ... -}
---
---   * small comment - from "--" to the end of a line
---
---   * text - a string, i.e. text enclosed in "..."
---
---   * letter - the given string is the representation of a character
---
---   * meta - containing information for meta programming
---
--- For an example to use the state scanner cf. addtypes, the tool 
--- to add function types to a given program.
---
--- @author Bernd Brassel
--- @version April 2005
----------------------------------------------------------------------


module CurryStringClassifier
         (Tokens,Token(..), scan, plainCode, unscan,
          isSmallComment, isBigComment, isComment, isText, isLetter,
          isCode, isModuleHead, isMeta,  readScan,testScan) 
      where

import Char(isDigit,isSpace)

--- The different categories to classify the source code.
data Token  = SmallComment String 
            | BigComment String 
            | Text String 
            | Letter String 
            | Code String 
            | ModuleHead String 
            | Meta String 

type Tokens = [Token]


--- test for category "SmallComment"
isSmallComment x = case x of 
                    SmallComment _ -> True
                    _ -> False

--- test for category "BigComment"
isBigComment x = case x of 
                    BigComment _ -> True
                    _ -> False

--- test if given token is a comment (big or small)
isComment x = isSmallComment x || isBigComment x

--- test for category "Text" (String)
isText x = case x of 
                    Text _ -> True
                    _ -> False

--- test for category "Letter" (Char)
isLetter x = case x of 
                    Letter _ -> True
                    _ -> False

--- test for category "Code"
isCode x = case x of 
                    Code _ -> True
                    _ -> False

--- test for category "ModuleHead", ie imports and operator declarations
isModuleHead x = case x of 
                    ModuleHead _ -> True
                    _ -> False

--- test for category "Meta", ie between {+ and +}
isMeta x = case x of 
                    Meta _ -> True
                    _ -> False

weaveIntoCode :: (Tokens -> Tokens) -> Tokens -> Tokens
weaveIntoCode f ts  = 
   let (cs,ncs) = unweaveCode ts in weave (f cs,ncs)

unweaveCode :: Tokens -> (Tokens,Tokens)
unweaveCode [] = ([],[])
unweaveCode (t:ts) = let (cs,ncs) = unweaveCode ts in 
  if isCode t then (t:cs,ncs) else (cs,t:ncs)
 
weave xys = case xys of
       ([],[]) -> []
       ([],[y]) -> [y]
       ([x],[]) -> [x]
       (x:xs,y:ys) -> x:y:weave (xs,ys)


--- Divides the given string into the six categories.
--- For applications it is important to know whether a given part of
--- code is at the beginning of a line or in the middle. The state scanner
--- organizes the code in such a way that every string categorized as 
--- "Code" <b>always</b> starts in the middle of a line.

scan :: [Char] -> Tokens
scan s = modHead id (stateScan 1 (Code x) x s) where x free

stateScan :: Int -> Token -> [Char] -> [Char] -> Tokens
stateScan _ token x ""  | x=:="" = [token]
stateScan _ (Code co) x [c] | x=:=[c] = maybeCode co []
stateScan _ (Text t) x [c] 
  | x=:="" = if c=='\"' then [Text t]
                        else error "File ended while scanning string."
stateScan _ (BigComment _) x [_]
  | x=:="" = error "File ended while expecting -}"
stateScan _ (Meta _) x [_]
  | x=:="" = error "File ended while expecting +}"
stateScan line (Code co) x (c:c':cs)
  | c=='\"' = (x=:="") &>
                    (maybeCode co (stateScan line (Text y) y (c':cs))) 
  | c=='-' && c'=='-' 
  = let (comment,rest) = span (/='\n') cs
     in (x=:="") &> maybeCode co 
                       (SmallComment comment :
                       (stateScan line (Code y) y rest))
  | c=='{' && c'=='-'
  = (x=:="") &> maybeCode co (stateScan line (BigComment y) y cs)
  | c=='{' && c'=='+'
  = (x=:="") &> maybeCode co (stateScan line (Meta y) y cs)
  | c'=='\'' && elem c (infixIDs++delimiters) 
  = (x=:=[c]) &> maybeCode co $ 
     case cs of
       '\\':l:'\'':rest -> Letter ['\\',l] : (stateScan line (Code y) y rest)
       '\\':a:b:d:'\'':rest -> 
         if all isDigit [a,b,d] 
           then Letter ['\\',a,b,d] : (stateScan line (Code y) y rest)
           else error $ "Improperly terminated character found in line "
                        ++ show line
       l:'\'':rest      -> Letter [l] : (stateScan line (Code y) y rest)
       _ -> error $ "Improperly terminated character in line "++show line
  | c=='\n'
  = (x=:=c:y) &> stateScan (line+1) (Code co) y (c':cs)
  | otherwise
  = (x=:=c:y) &> stateScan line (Code co) y (c':cs)
  where
    y free
stateScan line (BigComment co) x (c:c':cs)
  | c=='-' && c'=='}' 
  = (x=:="") &> BigComment co : (stateScan line (Code y) y cs)
  | c=='\n'
  = (x=:=c:y) &> stateScan (line+1) (BigComment co) y (c':cs)
  | otherwise 
  = (x=:=c:y) &> stateScan line (BigComment co) y (c':cs)
  where
   y free
stateScan line (Meta co) x (c:c':cs)
  | c=='+' && c'=='}' 
  = (x=:="") &> Meta co : (stateScan line (Code y) y cs)
  | c=='\n'
  = (x=:=c:y) &> stateScan (line+1) (Meta co) y (c':cs)
  | otherwise 
  = (x=:=c:y) &> stateScan line (Meta co) y (c':cs)
  where
   y free
stateScan line (Text t) x (c:c':cs)
  | c == '\"' = (x=:="") &> Text t : (stateScan line (Code y) y (c':cs))
  | c == '\\' = (x=:=c:c':y) &> stateScan line (Text t) y cs
  | elem c toBeEscaped = error $ "unescaped "++c:" encountered in line "
                                 ++ show line
  | otherwise = (x=:=c:y) &> stateScan line (Text t) y (c':cs)
  where
   y free

modHead :: ([Char] -> [Char]) -> Tokens -> Tokens
modHead fs (Code c : ts) 
  = case break (\x->x=='\n'||x=='\r') c of
      ("","")      -> modHead fs ts
      ("",n:rest)  -> modHead (fs . (n:)) (Code rest : ts)
      (line,"")    -> if any (lineBeginsWith line) headers
                        then ModuleHead (fs line) : (modHeadInLine id ts)
                        else maybeMo (fs "") (Code c:ts)
      (line,n:rest)-> if any (lineBeginsWith line) headers
                        then modHead (fs . (line++) . (n:)) (Code rest:ts) 
                        else maybeMo (fs "") (Code c : ts)
modHead fs (BigComment c : ts) = maybeMo (fs "") (BigComment c : (modHead id ts))
modHead fs (SmallComment c : ts) =  maybeMo (fs "") (SmallComment c : (modHead id ts))
modHead fs (Meta c : ts) =  maybeMo (fs "") (Meta c : ts)
modHead fs [] = maybeMo (fs "") []

modHeadInLine :: ([Char] -> [Char]) -> Tokens -> Tokens
modHeadInLine fs [] = maybeMo (fs "") []
modHeadInLine fs (Code c : ts)
  = case break (\x->x=='\n'||x=='\r') c of
      (line,n:rest) -> modHead (fs . (line++) . (n:)) (Code rest : ts) 
      _ -> modHead fs (Code c : ts)
modHeadInLine fs (BigComment c : ts) = 
  maybeMo (fs "") (BigComment c : (modHeadInLine id ts))
modHeadInLine fs (SmallComment c : ts) = 
  maybeMo (fs "") (SmallComment c : (modHeadInLine id ts))
modHeadInLine fs (Meta c : ts) = maybeMo (fs "") (Meta c : ts)

headers :: [[Char]]
headers = [" ","import","infix","infixl","infixr","module"]

lineBeginsWith :: [Char] -> [Char] -> Bool
lineBeginsWith line s | length line < lens = False
                      | otherwise 
                      = line==s || 
                        let (s',rest) = splitAt (length s) line 
                         in s==s' && (null rest || isSep (head rest))
  where
    lens = length s


isSep :: Char -> Bool
isSep c = isSpace c || elem c infixIDs || elem c "([{" 

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

delimiters :: String
delimiters = " ([{,}])"

toBeEscaped :: [Char]
toBeEscaped = "\\\n\r\t\""

maybeCode :: [Char] -> Tokens -> Tokens
maybeCode s ts = {-if s=="" then ts else-} Code s:ts

maybeMo s ts = if s=="" then ts else ModuleHead s:case ts of
  Code c:ts' -> Code ('\n':c):ts'
  _ -> ts

--- Yields the program code without comments 
--- (but with the line breaks for small comments).

plainCode :: Tokens -> String
plainCode (ModuleHead s:ts) = case ts of
                            Code c : ts' -> s++drop 1 c++plainCode ts'
                            _ -> s++plainCode ts
plainCode (Code s:ts) = s++plainCode ts
plainCode (Text s:ts) = '\"':s++'\"':plainCode ts
plainCode (Letter s:ts) = '\'':s++'\'':plainCode ts
plainCode (BigComment _:ts) = plainCode ts
plainCode (SmallComment _:ts) = plainCode ts
plainCode (Meta s:ts) = "{+"++s++"+}"++plainCode ts
plainCode [] = ""


--- Inverse function of scan, i.e., unscan (scan x) = x.
--- unscan is used to yield a program after changing the list of tokens.

unscan :: Tokens -> String
unscan (ModuleHead s:ts) = s++case ts of 
                                (Code (_:c):ts') -> unscan (Code c:ts')
                                _ -> unscan ts
unscan (Code s:ts) = s++unscan ts
unscan (Text s:ts) = '\"':s++'\"':unscan ts
unscan (Letter s:ts) = '\'':s++'\'':unscan ts
unscan (BigComment s:ts) = "{-"++s++"-}"++unscan ts
unscan (SmallComment s:ts) = "--"++s++unscan ts 
unscan (Meta s:ts) = "{+"++s++"+}"++unscan ts
unscan [] = ""

--- return tokens for given filename
readScan :: [Char] -> IO Tokens
readScan fn = readFile fn >>= return . scan
 

--- test whether (unscan . scan) is identity
testScan :: [Char] -> IO ()
testScan fn = do c <- readFile fn
                 print (unscan (scan c)==c)

testWeave :: [Char] -> IO ()
testWeave fn = do c <- readFile fn
                  print (unscan (weave (unweaveCode (scan c)))==c)


