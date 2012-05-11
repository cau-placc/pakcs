------------------------------------------------------------------------------
-- Translate a Curry program into a FlatCurry program in
-- XML representation (the corresponding DTD can be found in
-- http://www.informatik.uni-kiel.de/~curry/flatcurry.dtd)
--
-- Michael Hanus, March 2001
------------------------------------------------------------------------------

import Flat
import FlatTools
import FlatXML
import Flat2Fcy
import XML
import List
import System
import FlatCurry(flatCurryFileName)

-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
    [mod]          -> curry2xmlfile (stripCurrySuffix mod)
    ["-print",mod] -> curry2xml (stripCurrySuffix mod)
    ["-fcy",mod]   -> xml2fcyfile (stripCurrySuffix mod)
    _ -> printArgError args

printArgError args =  putStrLn $
  "ERROR: Illegal arguments for curry2xml: " ++
  concat (intersperse " " args) ++ "\n" ++
 "Usage: curry2xml [-fcy|-print] <module_name>\n" ++
  "Parameters:\n" ++
  "-fcy  : translate FlatCurry XML file into FCY file\n" ++
  "-print: print only on standard out instead of writing into file\n"


stripCurrySuffix n = let rn = reverse n in
  if take 6 rn == "yrruc." then reverse (drop 6 rn) else
  if take 7 rn == "yrrucl." then reverse (drop 7 rn) else n


-- translate Curry program into XML/FlatCurry and print this on stdout:
curry2xml :: String -> IO ()
curry2xml progname =
  do flatprog <- readFlatCurry progname
     putStrLn "Translated FlatCurry program in XML representation:"
     putStrLn "===================================================\n"
     putStrLn (showXmlDoc (flatCurry2Xml flatprog))

-- translate Curry program into XML/FlatCurry and store this in an XML file:
curry2xmlfile :: String -> IO ()
curry2xmlfile progname =
  do flatprog <- readFlatCurry progname
     putStr "Write FlatCurry program in XML representation to file "
     putStr ("\""++progname++"_flat.xml\"...\n")
     flatCurry2XmlFile flatprog (progname++"_flat.xml")

-- translate Curry program from XML/FlatCurry file into .fcy file:
xml2fcyfile :: String -> IO ()
xml2fcyfile progname =
  do putStrLn ("Reading XML/FlatCurry file \""++progname++"_flat.xml\"...")
     flatprog <- xmlFile2FlatCurry (progname++"_flat.xml")
     let fcyprogname = flatCurryFileName progname
     putStr "Writing FlatCurry program to file "
     putStr ("\""++fcyprogname++"\"...\n")
     writeFCY fcyprogname flatprog

-- examples:

-- curry2xml "../examples/rev"
-- curry2xml "../examples/chords"
-- curry2xmlfile "../examples/rev"
-- curry2xmlfile "../examples/chords"
