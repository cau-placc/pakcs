----------------------------------------------------------------------
--- Some auxiliary operations of CurryDoc to read programs.
---
--- @author Michael Hanus
----------------------------------------------------------------------

module CurryDocRead where

import FlatCurry
import AnaCompleteness
import Char

--------------------------------------------------------------------------
-- read the comments of a source file to be put in the HTML documentation
readComments :: String -> IO (String,[(SourceLine,String)])
readComments filename =
 do prog <- readFile filename
    return (groupLines . filter (/=OtherLine) . map classifyLine . lines
            $ prog)

--- This datatype is used to classify all input lines.
--- @cons Comment   - a comment for CurryDoc
--- @cons FuncDef   - a definition of a function
--- @cons DataDef   - a definition of a datatype
--- @cons ModDef    - a line containing a module definition
--- @cons OtherLine - a line not relevant for CurryDoc
data SourceLine = Comment String  -- a comment for CurryDoc
                | FuncDef String  -- a definition of a function
                | DataDef String  -- a definition of a datatype
                | ModDef          -- a line containing a module definition
                | OtherLine       -- a line not relevant for CurryDoc

-- classify a line of the source program:
-- here we replace blank line comments by a "breakline" tag
classifyLine :: String -> SourceLine
classifyLine line
 | take 3 line == "---" && all isSpace (drop 3 line) = Comment "" --"<br/>"
 | take 4 line == "--- "     = Comment (drop 4 line)
 | take 7 line == "module "  = ModDef
 | take 7 line == "import "  = ModDef
 | otherwise = let id1 = getFirstId line
                in if id1==""
                    then OtherLine
                    else if id1=="data" || id1=="type"
                          then DataDef (getDatatypeName line)
                          else FuncDef id1
 where
   getDatatypeName = takeWhile isIdChar . dropWhile (==' ') . dropWhile isIdChar

-- get the first identifier (name or operator in brackets) in a string:
getFirstId [] = ""
getFirstId (c:cs) | isAlpha c = takeWhile isIdChar (c:cs)
                  | c=='('    = takeWhile (/=')') cs
                  | otherwise = ""

-- is an alphanumeric character, underscore, or apostroph?
isIdChar c = isAlphaNum c || c=='_' || c=='\''


-- group the classified lines into module comment and list of
-- (Func/DataDef,comment) pairs:
groupLines :: [SourceLine] -> (String,[(SourceLine,String)])
groupLines sls =
  let (modcmts,progcmts) = break (==ModDef) sls
   in if progcmts==[]
      then ("", groupProgLines sls)
      else (concatMap getComment modcmts,
            groupProgLines (filter (/=ModDef) (tail progcmts)))
 where
   getComment (Comment cmt) = cmt++"\n"
   getComment (FuncDef _)   = ""  -- this case should usually not occur
   getComment (DataDef _)   = ""  -- this case should usually not occur

groupProgLines :: [SourceLine] -> [(SourceLine,String)]
groupProgLines [] = []
groupProgLines (Comment cmt : sls) = groupComment cmt sls
groupProgLines (FuncDef f : sls) = (FuncDef f, "") : skipFuncDefs f sls
groupProgLines (DataDef d : sls) = (DataDef d, "") : skipDataDefs d sls

groupComment _ [] = []  -- comment not followed by definition -> ignore
groupComment cmt (Comment cmt1 : sls) = groupComment (cmt++"\n"++cmt1) sls
groupComment cmt (FuncDef f : sls) = (FuncDef f, cmt) : skipFuncDefs f sls
groupComment cmt (DataDef d : sls) = (DataDef d, cmt) : skipDataDefs d sls

skipFuncDefs _ [] = []
skipFuncDefs _ (Comment cmt : sls) = groupProgLines (Comment cmt : sls)
skipFuncDefs _ (DataDef d   : sls) = groupProgLines (DataDef d   : sls)
skipFuncDefs f (FuncDef f1  : sls) =
  if f==f1 then skipFuncDefs f sls
           else groupProgLines (FuncDef f1 : sls)

skipDataDefs _ [] = []
skipDataDefs _ (Comment cmt : sls) = groupProgLines (Comment cmt : sls)
skipDataDefs _ (FuncDef f   : sls) = groupProgLines (FuncDef f   : sls)
skipDataDefs d (DataDef d1  : sls) =
  if d==d1 then skipDataDefs d sls
           else groupProgLines (DataDef d1 : sls)


--------------------------------------------------------------------------
-- get comment for a function name:
getFuncComment _ [] = ""
getFuncComment fname ((FuncDef f, cmt):fdcmts) =
  if fname == f
  then cmt
  else getFuncComment fname fdcmts
getFuncComment fname ((DataDef _,_):fdcmts) = getFuncComment fname fdcmts

getConsComment [] _ = Nothing
getConsComment (conscmt:conscmts) cname =
  let (consname,rconscmt) = span isIdChar conscmt
   in if consname == cname
      then let (conscall,callcmt) = break (=='-') conscmt
            in Just (if null callcmt then (consname,rconscmt)
                                     else (conscall,callcmt))
      else getConsComment conscmts cname

-- get comment for a type name:
getDataComment _ [] = ""
getDataComment n ((DataDef d, cmt):fdcmts) =
  if n == d then cmt
            else getDataComment n fdcmts 
getDataComment n ((FuncDef _,_):fdcmts) = getDataComment n fdcmts


-- get all comments of a particular type (e.g., "param", "cons"):
getCommentType ctype cmts = map snd (filter (\c->fst c==ctype) cmts)



--------------------------------------------------------------------------
-- Split a comment into its main part and parts preceded by "@...":
-- Example: splitComment "aaaa\nbbbb\n@param xxxx\n@return yyyy"
--          = ("aaaa\nbbbb",[("param","xxxx"),("return","yyyy")])

splitComment :: String -> (String,[(String,String)])
splitComment cmt = splitCommentMain (lines cmt)

splitCommentMain [] = ("",[])
splitCommentMain (l:ls) =
  if l=="" || head l /= '@'
  then let (maincmt,rest) = splitCommentMain ls
        in (l++('\n':maincmt),rest)
  else ([],splitCommentParams (takeWhile isAlpha (tail l))
                              (dropWhile isAlpha (tail l)) ls)

splitCommentParams param paramcmt [] = [(param,skipWhiteSpace paramcmt)]
splitCommentParams param paramcmt (l:ls) =
  if l=="" || head l /= '@'
  then splitCommentParams param (paramcmt++('\n':l)) ls
  else ((param,skipWhiteSpace paramcmt)
        : splitCommentParams (takeWhile isAlpha (tail l))
                             (dropWhile isAlpha (tail l)) ls)

-----------------------------------------------------------------------
-- Datatype for passing analysis results:

data AnaInfo =
   AnaInfo ((String,String) -> Bool)  -- overlapping?
           ((String,String) -> CompletenessType)  -- completely defined?
           ((String,String) -> Bool)  -- indeterministically defined?
           ((String,String) -> Bool)  -- solution complete?

getOverlappingInfo :: AnaInfo -> (String,String) -> Bool
getOverlappingInfo (AnaInfo oi _ _ _) = oi

getCompleteInfo :: AnaInfo -> (String,String) -> CompletenessType
getCompleteInfo (AnaInfo _ cdi _ _) = cdi

getIndetInfo :: AnaInfo -> (String,String) -> Bool
getIndetInfo (AnaInfo _ _ idi _) = idi

getOpCompleteInfo :: AnaInfo -> (String,String) -> Bool
getOpCompleteInfo (AnaInfo _ _ _ oci) = oci

-- Translate a standard analysis result into functional form:
getFunctionInfo :: [((String,String),a)] -> (String,String) -> a
getFunctionInfo [] n = error ("No analysis result for function "++show n)
getFunctionInfo ((fn,fi):fnis) n = if fn==n then fi
                                            else getFunctionInfo fnis n


--------------------------------------------------------------------------
-- auxiliaries:

isFunctionType (TVar _)       = False
isFunctionType (FuncType _ _) = True
isFunctionType (TCons _ _)    = False

-- skip leading blanks or CRs in a string:
skipWhiteSpace = dropWhile isWhiteSpace

isWhiteSpace c = c==' ' || c=='\n'

-- enclose a non-letter identifier in brackets:
showId :: String -> String
showId name = if isAlpha (head name) then name
                                     else ('(':name)++")"

-- if first argument is True, put brackets around second argument:
brackets :: Bool -> String -> String
brackets False s = s
brackets True  s = "("++s++")"

-- extract last name from a path name:
getLastName = reverse . takeWhile (/='/') . reverse

--------------------------------------------------------------------------
