-- a simple demo for checkbuttons in the GUI library

import GUI

checkbuttonWidget =
     Col [] [
       Label [Text "CheckButton Demo:"],
       Entry [WRef vresult, Text "off", Background "yellow"],
       Row [] [CheckButton [Text "Check1", CheckInit "1", WRef vc1,
                            Cmd showChecks],
               CheckButton [Text "Check2", WRef vc2,
                            Cmd showChecks],
               Button exitGUI [Text "Stop"]]]
  where vc1, vc2, vresult free

        showChecks gport =
         do c1 <- getValue vc1 gport
            c2 <- getValue vc2 gport
            setValue vresult ("Check 1: "++c1++" / Check 2: "++c2) gport


main = runGUI "CheckButton Demo" checkbuttonWidget

