module Main where

import PrettyAbstract
import XML
import XML2ERD
import ERD
import ERDGoodies
import Transformation
import CodeGeneration
import System(getArgs,system)
import FileGoodies(dirName)
import ReadShowTerm(readUnqualifiedTerm)
import Time
import Directory
import ERD2Graph
import System(exitWith)

banner = "ERD->Curry Compiler (Version of 22/12/10)\n"

--- Main function for saved state.
main = do
  putStrLn banner
  args <- getArgs
  callStart (parseArgs ("",True,Files ".",False) args)

parseArgs _ [] = Nothing
parseArgs (file,fromxml,storage,vis) (arg:args) = case arg of
  "-t" -> parseArgs (file,False,storage,vis) args
  "-d" -> parseArgs(file,fromxml,DB,vis) args
  "-p" -> if null args then Nothing else
          parseArgs(file,fromxml,Files (head args),vis) (tail args)
  "-v" -> parseArgs(file,fromxml,storage,True) args
  f    -> if null args then Just (f,fromxml,storage,vis) else Nothing

helpText =
  "Usage: erd2curry [-d] [-t] [-v] <file>\n" ++
  "Parameters:\n" ++
  "-d: generate SQL interface instead of file-based interface\n" ++
  "-t: generate from ERD term instead of xmi document\n" ++
  "-p <path>: prefix DB files with <path>\n" ++
  "-v: visualize ERD term with dotty\n" ++
  "<file>: name of file containing xmi document of ERD term\n"

callStart Nothing = do
  putStrLn $ "ERROR: Illegal arguments\n\n" ++ helpText
  exitWith 1
callStart (Just (file,fromxml,storage,vis)) =
 if vis
 then readERDTermFile file >>= viewERD
 else start (storage,WithConsistencyTest) fromxml file "." --(dirName file)

--- Main function to invoke the ERD->Curry translator.
start :: Option -> Bool -> String -> String -> IO ()
start opt fromxml srcfile path = do
  (erdfile,erd) <- if fromxml
                   then transformXmlFile srcfile path
                   else readERDTermFile srcfile >>= \e -> return (srcfile,e)
  let transerdfile = addPath path (erdName erd ++ "_ERDT.term")
      curryfile    = addPath path (erdName erd ++ ".curry")
      transerd     = transform erd
  writeFile transerdfile
            ("{- ERD specification transformed from "++erdfile++" -}\n\n " ++
             showERD 2 transerd ++ "\n")
  putStrLn $ "Transformed ERD term written into file '"++transerdfile++"'."
  moveOldVersion curryfile
  writeFile curryfile $ showCProg $ erd2code opt $ transform erd
  putStrLn $ "Database operations generated into file '"++curryfile++"'\n"++
             "with " ++ showOption opt ++ ".\n"
 where
  showOption (Files f,_) = "database files stored in directory '"++f++"'"
  showOption (DB,_) = "SQL database interface"

--- Adds a path to a file name.
addPath :: String -> String -> String
addPath path fname | path=="." = fname
                   | otherwise = path++"/"++fname

--- Moves a file (if it exists) to one with extension ".versYYMMDDhhmmss".
moveOldVersion :: String -> IO ()
moveOldVersion fname = do
  exists <- doesFileExist fname
  if exists
   then do
     mtime <- getModificationTime fname
     cmtime <- toCalendarTime mtime
     let fnamevers = fname ++ ".vers" ++ calTime2Digits cmtime
     system $ "mv "++fname++" "++fnamevers
     putStrLn $ "Old contents of file \""++fname++"\" saved into file \""++
                fnamevers++"\"."
   else done
 where
  calTime2Digits (CalendarTime y mo d h mi s _) =
    toD (y `mod` 100) ++ toD mo ++ toD d ++ toD h ++ toD mi ++ toD s

  toD i = if i<10 then '0':show i else show i
  

--test = start (Files ".", WithConsistencyTest) True "./Uni.xmi" "."
test = start (Files ".", WithConsistencyTest) False "./Uni_ERD.term" "."

testTransformation = do
  xml <- readXmlFile "./Uni.xmi"
  let erd = convert xml
  putStr (showERD 0 erd)
  putStr "\n\n"
  putStrLn (showERD 0 (transform erd))
  
--- Read an ERD specification from an XML file in Umbrello format.
transformXmlFile :: String -> String -> IO (String,ERD)
transformXmlFile xmlfile path = do
  putStrLn $ "Reading XML file " ++ xmlfile ++ "..."
  xml <- readXmlFile xmlfile
  let erd     = convert xml
  let erdfile = addPath path (erdName erd ++ "_ERD.term")
  writeFile erdfile
            ("{- ERD specification read from "++xmlfile++" -}\n\n " ++
             showERD 2 erd ++ "\n")
  putStrLn $ "ERD term written into file \""++erdfile++"\"."
  return (erdfile,erd)

{-
-- Uni.xmi -> ERD term:
(ERD "Uni"
 [(Entity "Student" [(Attribute "MatNum" (IntDom Nothing) PKey False),
                     (Attribute "Name" (StringDom Nothing) NoKey False),
                     (Attribute "Firstname" (StringDom Nothing) NoKey False),
                     (Attribute "Email" (UserDefined "MyModule.Email" Nothing) NoKey True)]),
  (Entity "Lecture" [(Attribute "Id" (IntDom Nothing) PKey False),
                     (Attribute "Title" (StringDom Nothing) Unique False),
                     (Attribute "Hours" (IntDom (Just 4)) NoKey False)]),
  (Entity "Lecturer" [(Attribute "Id" (IntDom Nothing) PKey False),
                      (Attribute "Name" (StringDom Nothing) NoKey False),
                      (Attribute "Firstname" (StringDom Nothing) NoKey False)]),
  (Entity "Group" [(Attribute "Time" (StringDom Nothing) NoKey False)])]
 [(Relationship "Teaching" [(REnd "Lecturer" "taught_by" (Exactly 1)),
                            (REnd "Lecture" "teaches" (Between 0 Infinite))]),
  (Relationship "Participation" [(REnd "Student" "participated_by" (Between 0 Infinite)),
                                 (REnd "Lecture" "participates" (Between 0 Infinite))]),
  (Relationship "Membership" [(REnd "Student" "consists_of" (Exactly 3)),
                              (REnd "Group" "member_of" (Between 0 Infinite))])])

-- Transformation of ERD term:
(ERD "Uni"
 [(Entity "Membership"
          [(Attribute "Student_Membership_Key" (KeyDom "Student") PKey False),
           (Attribute "Group_Membership_Key" (KeyDom "Group") PKey False)]),
  (Entity "Participation"
          [(Attribute "Student_Participation_Key" (KeyDom "Student") PKey False),
           (Attribute "Lecture_Participation_Key" (KeyDom "Lecture") PKey False)]),
  (Entity "Student"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "MatNum" (IntDom Nothing) Unique False),
           (Attribute "Name" (StringDom Nothing) NoKey False),
           (Attribute "Firstname" (StringDom Nothing) NoKey False),
           (Attribute "Email" (UserDefined "MyModule.Email" Nothing) NoKey True)]),
  (Entity "Lecture"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "Lecturer_Teaching_Key" (KeyDom "Lecturer") NoKey False),
           (Attribute "Id" (IntDom Nothing) Unique False),
           (Attribute "Title" (StringDom Nothing) Unique False),
           (Attribute "Hours" (IntDom (Just 4)) NoKey False)]),
  (Entity "Lecturer"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "Id" (IntDom Nothing) Unique False),
           (Attribute "Name" (StringDom Nothing) NoKey False),
           (Attribute "Firstname" (StringDom Nothing) NoKey False)]),
  (Entity "Group"
          [(Attribute "Key" (IntDom Nothing) PKey False),
           (Attribute "Time" (StringDom Nothing) NoKey False)])]
 [(Relationship [] [(REnd "Student" [] (Exactly 1)),
                    (REnd "Membership" "member_of" (Between 0 Infinite))]),
  (Relationship [] [(REnd "Group" [] (Exactly 1)),
                    (REnd "Membership" "consists_of" (Exactly 3))]),
  (Relationship [] [(REnd "Student" [] (Exactly 1)),
                    (REnd "Participation" "participates" (Between 0 Infinite))]),
  (Relationship [] [(REnd "Lecture" [] (Exactly 1)),
                    (REnd "Participation" "participated_by" (Between 0 Infinite))]),
  (Relationship "Teaching" [(REnd "Lecturer" "taught_by" (Exactly 1)),
                            (REnd "Lecture" "teaches" (Between 0 Infinite))])])
-}
