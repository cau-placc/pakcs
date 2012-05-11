-- A simple GUI demonstrating the use of "appendValue"
-- in TextEdit widgets:

import GUI

addlineGUI =
 Col [] [
   Label [Text "A simple GUI for appending lines:"],
   Row [] [Label [Text "Line to be appended in window below:"],
           Entry [WRef rline, Width 30, Background "white", Cmd addline]],
   TextEditScroll [WRef rtxt, Background "yellow"],
   Button exitGUI [Text "Exit"]]
 where
   rline,rtxt free

   addline wp = do line <- getValue rline wp
                   appendValue rtxt (line++"\n") wp

main = runGUI "appendValue Demo" addlineGUI


