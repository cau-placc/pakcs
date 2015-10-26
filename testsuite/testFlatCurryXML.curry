------------------------------------------------------------------------------
--- Some tests for library FlatCurryXML.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testFlatCurryXML"
--- 
--- @author Michael Hanus
--- @version April 2005
------------------------------------------------------------------------------

import FlatCurry.Files
import FlatCurry.XML
import XML
import Assertion

-- Shows a program in XML format:
showxml mod = do
  prog <- readFlatCurry mod
  putStrLn (showXmlDoc (flatCurry2Xml prog))

-- Store a program in XML format:
store mod = do
  prog <- readFlatCurry mod
  flatCurry2XmlFile prog (mod++"_fcy.xml")
  putStrLn (mod++"_fcy.xml"++" written")

-- Test for equality after XML encoding/decoding:
testEqualFcy prog = prog == xml2FlatCurry (flatCurry2Xml prog)

readAndTestEqualFcy mod = do
  prog <- readFlatCurry mod
  return (testEqualFcy prog)


test1  = assertIO "XML test for rev" (readAndTestEqualFcy "rev") True

--test2  = assertIO "XML test for nats" (readAndTestEqualFcy "nats") True

--test3  = assertIO "XML test for chords" (readAndTestEqualFcy "chords") True

