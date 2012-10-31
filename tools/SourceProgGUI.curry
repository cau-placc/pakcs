------------------------------------------------------------------------
--- A simple GUI for highlighting functions in the source text
--- of a Curry module.
---
--- To start: SourceProgGUI <module name>
--- Select highlighting by commands on stdin:
--- +fun          -> highlight function "fun"
--- -fun          -> remove highlighting for function "fun"
--- <empty line>  -> terminate GUI
--- q             -> terminate GUI
---
--- @version June 2011
------------------------------------------------------------------------

import IO
import GUI
import ReadNumeric
import FlatCurryShow(showCurryId)
import List(isPrefixOf)
import Distribution(getLoadPathForFile)
import FileGoodies
import System(getArgs)
import Char(isAlpha,isSpace)
import Maybe(isJust)

---------------------------------------------------------------------
-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath modname = do
  loadpath <- getLoadPathForFile modname
  mbfname <- lookupFileInPath (baseName modname) [".lcurry",".curry"] loadpath
  maybe (error $ "Curry file for module \""++modname++"\" not found!")
        return
        mbfname

-- find a function declaration in a program text:
findFunDeclInProgText :: String -> String -> Int
findFunDeclInProgText progtext fname =
  findFirstDeclLine (showCurryId fname) (lines progtext) 1

-- finds first declaration line:
findFirstDeclLine _ [] _ = 0 -- not found
findFirstDeclLine f (l:ls) n =
     if isPrefixOf f l then n else findFirstDeclLine f ls (n+1)

---------------------------------------------------------------------
-- The definition of the GUI together with a handler
-- "extHandler" that is responsible to handle the external input:
sourceProgGUI :: String -> [(String,(Int,Int))]
             -> (Widget,[Handle -> GuiPort -> IO [ReconfigureItem]])
sourceProgGUI cnt progdefs =
 (col [row [Label [Text "Focus on function:"],
            Entry [WRef rinp, Background "yellow", FillX]
           ],
       TextEditScroll [WRef ptxt, Text cnt, Background "white",
                       Height 10, Width 70, Fill]],
  [extHandler])

 where
   ptxt,rinp free

   extHandler :: Handle -> GuiPort -> IO [ReconfigureItem]
   extHandler h gp = do
     inp <- hGetLine h
     if inp=="" || head inp == 'q'
      then exitGUI gp
      else maybe done
                 (\ (start,end) ->
                   if head inp == '+'
                   then do
                     setValue rinp (tail inp) gp
                     addRegionStyle ptxt (start,0) (end+1,0) (Bg Yellow) gp
                     seeText ptxt ((start+end) `div` 2,0) gp
                   else do
                     removeRegionStyle ptxt (start,0) (end+1,0) (Bg Yellow) gp
                     setValue rinp "" gp
                     extHandler h gp >> done
                 )
                (lookup (tail inp) progdefs)
     return []

-- start the counter GUI:
startGUI prog = do
  filename <- findSourceFileInLoadPath prog
  contents <- readFile filename
  runHandlesControlledGUI ("Module: "++filename)
     (sourceProgGUI contents (splitProgDefs contents))
     [stdin]

main = do
  args <- getArgs
  startGUI (head args)

----------------------------------------------------------------------------
-- Extract start and end lines of all function definitions in a program text:
splitProgDefs :: String -> [(String,(Int,Int))]
splitProgDefs ptxt =
  groupFuns (dropWhile (null . fst)
  (deleteAdjacentFuns
   (concatMap
      (\ (mb,i) -> maybe [] (\s->if s `elem` keywords then [] else [(s,i)]) mb)
      (zip (map funDefOfLine (lines ptxt)) [1..]))))

groupFuns [] = []
groupFuns [(f,i)] = [(f,(i,i))]
groupFuns [(f1,i1),(f2,i2)] = 
  if f1==f2 then [(f1,(i1,i2))] else
  if null f2 then [(f1,(i1,i1))] else [(f1,(i1,i1)),(f2,(i2,i2))]
groupFuns ((f1,i1):(f2,i2):(f3,i3):fis)
 | null f2 && f1==f3 = groupFuns ((f1,i1):(f3,i3):fis)
 | null f2 = (f1,(i1,i2-1)) : groupFuns ((f3,i3):fis)
 | f1==f2 = groupFuns ((f1,i1):(f3,i3):fis)
 | otherwise = (f1,(i1,i2-1)) : groupFuns ((f2,i2):(f3,i3):fis)

-- delete subsequent function definitions
deleteAdjacentFuns [] = []
deleteAdjacentFuns [x] = [x]
deleteAdjacentFuns ((f1,i1):(f2,i2):xs) =
  if f1==f2 then deleteAdjacentFuns ((f1,i1):xs)
            else (f1,i1) : deleteAdjacentFuns ((f2,i2):xs)

keywords = ["module","import","data","infix","infixr","infixl"]

funDefOfLine l
  | all isSpace l = Nothing
  | isAlpha (head l) = Just (head (words l))
  | head l == '(' = Just (reverse (tail (reverse (head (words (tail l))))))
  | isCommentLine l = Just ""
  | otherwise = Nothing

isCommentLine l = take 2 (dropWhile isSpace l) == "--"
  
----------------------------------------------------------------------------

m prog = do
  filename <- findSourceFileInLoadPath prog
  contents <- readFile filename
  print $ splitProgDefs contents
