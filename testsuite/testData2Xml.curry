-- Test for Data2Xml tool
--
-- Execute:
-- > data2xml Prelude
-- > data2xml FlatCurry
-- > <REPL> :load xmldatatest :eval main :quit

import Assertion -- for testing

import Distribution(installDir)
import System(system)

-- Generate XML conversions:
testInitialize = assertIO "initialize" init 0
 where init = do system (installDir++"/bin/data2xml Prelude")
                 system (installDir++"/bin/data2xml FlatCurry.Types")

test1 = assertIO "test XML data conversion" convert 0
 where
  convert = system $
    installDir++"/bin/curry :set v0 :l testData2XmlProg :eval main :q"

-- Clean:
testFinalize = assertIO "finalize" clean 0
 where
  clean = do
    system (installDir++"/bin/cleancurry PreludeDataToXml")
    system (installDir++"/bin/cleancurry FlatCurry_TypesDataToXml")
    system (installDir++"/bin/cleancurry testData2XmlProg")
    system "/bin/rm -f PreludeDataToXml.curry FlatCurry_TypesDataToXml.curry"


