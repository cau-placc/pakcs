-- Test for Data2Xml tool
--
-- Execute:
-- > data2xml Prelude
-- > data2xml FlatCurry
-- > <REPL> :load xmldatatest :eval main :quit

import Distribution(installDir)
import System(system)
import Test.EasyCheck

testGenerateXMLConversions = init `returns` 0
 where init = do system (installDir++"/bin/data2xml Prelude")
                 system (installDir++"/bin/data2xml FlatCurry.Types")

testXMLDataConversion = system convertCmd `returns` 0
 where
  convertCmd = installDir++"/bin/curry :set v0 :set parser -Wnone " ++
               ":l testData2XmlProg :eval main :q"

-- Clean:
testCleanup = clean `returns` 0
 where
  clean = do
    system (installDir++"/bin/cleancurry PreludeDataToXml")
    system (installDir++"/bin/cleancurry FlatCurry_TypesDataToXml")
    system (installDir++"/bin/cleancurry testData2XmlProg")
    system "/bin/rm -f PreludeDataToXml.curry FlatCurry_TypesDataToXml.curry"


