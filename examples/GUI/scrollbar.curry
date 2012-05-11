-- a simple demo for scrollbars:

import GUI

widget =
 Col [] [
   Label [Text "A simple text editor with scrollbars:"],
   Matrix []
     [[TextEdit [WRef txt1, Background "yellow", Fill],
       ScrollV txt1 [FillY]],
      [ScrollH txt1 [FillX]]],
   Button exitGUI [Text "Stop"]
   ]
 where txt1 free

main = runGUI "Text Demo" widget
