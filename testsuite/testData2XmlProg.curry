-- Test for Data2Xml tool
--
-- Execute:
-- > data2xml Prelude
-- > data2xml FlatCurry
-- > <REPL> :load testData2XmlProg :eval main :quit

import FlatCurry.Files
import FlatCurry_TypesDataToXml
import XML
import System

main :: IO ()
main = do
  prog <- readFlatCurry "testData2XmlProg"
  --print prog
  --putStrLn (showXmlDoc (progToXml prog))
  let nprog = xmlToProg (head (parseXmlString (showXmlDoc (progToXml prog))))
  exitWith (if (prog==nprog) then 0 else 1)


